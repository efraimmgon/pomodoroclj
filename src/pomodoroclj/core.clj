(ns pomodoroclj.core
  (:require
   [clojure.tools.logging :as log]
   [clojure.spec.alpha :as s]
   [pomodoroclj.protocols :as p]
   [pomodoroclj.db :as db]
   [pomodoroclj.utils :refer [safe-future]]
   [pomodoroclj.datetime :as datetime]
   ;; register specs
   pomodoroclj.specs
   ;; required to alter the reader tags:
   pomodoroclj.setup))

;;; ----------------------------------------------------------------------------
;;; State
;;; ----------------------------------------------------------------------------

(def duration
  "A map containing the duration in minutes for each session type."
  {:work 25
   :short-break 5
   :long-break 15})


(def starting-state
  #:session{:type :work
            :is-running false
            :time-elapsed 0})

(defonce state
  (atom starting-state))

;;; ----------------------------------------------------------------------------
;;; DB: Pomodoro specific


(defn load-last-session!
  "loads the last session data into the state atom.
   creates default session data if none exists."
  [state]
  (let [data (db/get-by-id "settings" "last-session")
        now (java.time.Instant/now)]
    (swap! state assoc :last-session/Stats
           (if (and (seq data)
                    (datetime/inst-same-date?
                     now
                     (-> data :last-session/logged-at)))
             data
             #:last-session{:id "last-session"
                            :logged-at now
                            :pomodoros-completed 0}))))


(defn pomodoros-completed-today
  "Returns the number of pomodoros completed today.
   Returns 0 if logged-at is nil, pomodoros-completed is nil, or if logged-at 
   is not from today."
  [logged-at pomodoros-completed]
  (if (and logged-at
           pomodoros-completed
           (datetime/inst-same-date?
            (java.time.Instant/now)
            logged-at))
    pomodoros-completed
    0))


(defn current-cycle
  "Returns a string representation of the current pomodoro cycle progress.
   Uses filled (●) and empty (○) circles to show completed and remaining 
   pomodoros in the cycle."
  [pomodoros-completed-today]
  (let [curr-cycle
        (mod pomodoros-completed-today 4)

        cycle-progress
        (repeat 4 "○")

        cycle-progress
        (map-indexed (fn [i dot]
                       (if (<= i curr-cycle) "●" dot))
                     cycle-progress)]
    (apply str cycle-progress)))

(assert
 (= ["●○○○"
     "●●○○"
     "●●●○"
     "●●●●"]
    (mapv current-cycle (range 4))))

;;; ----------------------------------------------------------------------------
;;; Utils
;;; ----------------------------------------------------------------------------


(defn calc-elapsed-secs
  "Calculates the elapsed time in seconds between start-time and now."
  [start-time now]
  (- (.getEpochSecond now)
     (.getEpochSecond start-time)))

(defn should-continue-session?
  "Returns true if the session is running and has not exceeded its duration."
  [is-running elapsed duration]
  (and is-running (< elapsed duration)))

(defn session-time-expired?
  [time-elapsed duration]
  (>= time-elapsed duration))

(defn not-running-with-time-remaining?
  "Returns true if the session is paused and has time remaining."
  [is-running elapsed duration]
  (and (not is-running)
       (< elapsed duration)))

(defn whole-minute?
  "Returns true if the given number of seconds represents a whole minute."
  [seconds]
  (zero? (mod seconds 60)))

(defn work-session?
  "Returns true if the session type is :work."
  [session-type]
  (= session-type :work))

(defn break-session?
  [session-type]
  (#{:short-break :long-break} session-type))

(defn session-type->complete-msg
  "Returns an appropriate message for the completed session type."
  [session-type]
  (case session-type
    :work "Work session complete! Time for a break."
    :short-break "Short break over! Back to work."
    :long-break "Long break complete! Ready for a new session."))

(defn calc-current-stats
  "Calculates current session statistics including pomodoros completed today 
   and cycle progress."
  [state]
  (let [{:last-session/keys [logged-at pomodoros-completed]}
        (:last-session/Stats state)

        pomodoros-completed-today
        (pomodoros-completed-today logged-at pomodoros-completed)]
    {:pomodoros-completed-today pomodoros-completed-today
     :cycle-progress (current-cycle pomodoros-completed-today)}))


(defn next-session
  "Determines the next session type based on the current state.
   Returns :long-break after 4 work sessions, :short-break after a work session,
   and :work after any break."
  [state]
  (let [pomodoros-completed
        (-> state :last-session/Stats :last-session/pomodoros-completed)]
    (cond
      (and (= (:session/type state) :work)
           (pos? pomodoros-completed)
           (zero? (mod pomodoros-completed 4)))
      :long-break

      (= (:session/type state) :work)
      :short-break

      :else
      :work)))

(defn- mk-session-test [type pomodoros]
  {:session/type type
   :last-session/Stats {:last-session/pomodoros-completed pomodoros}})

(assert (= :short-break (-> (mk-session-test :work 0) (next-session))))
(assert (= :work (-> (mk-session-test :short-break 0) (next-session))))
(assert (= :work (-> (mk-session-test :long-break 0) (next-session))))
(assert (= :long-break (-> (mk-session-test :work 4) (next-session))))
(assert (= :long-break (-> (mk-session-test :work 8) (next-session))))

(defn session-duration
  "Returns the duration of the next session in seconds."
  [state]
  (case (:session/type state)
    :work (* 60 (:work duration))
    :short-break (* 60 (:short-break duration))
    :long-break (* 60 (:long-break duration))))

(declare start)

(defn handle-session-completion!
  "Handles the completion of a session by updating the state and starting the 
   next session if appropriate. Increments pomodoro count for work sessions 
   and transitions to the next session type."
  [state]
  (let [next-session-type (cond-> @state
                            (work-session? (:session/type @state))
                            ;; - we must take into account that we just finished
                            ;; a session, but haven't yet updated the state
                            (update-in [:last-session/Stats
                                        :last-session/pomodoros-completed]
                                       inc)
                            :always (next-session))]
    ;; reset state in preparation for next cycle
    (swap! state dissoc
           :session/duration
           :session/start-time)
    (swap! state assoc
           :session/time-elapsed 0
           :session/is-running false
           :session/type next-session-type)

    (when (break-session? next-session-type)
      (start))))


(defn timer-thread!
  "Creates and manages a timer thread that handles session timing, 
   notifications, and state updates.
   Takes a state atom and optional notifier, store, and reporter components."
  [state {:keys [notifier store reporter]
          :or {notifier (p/->SystemNotifier)
               store (p/->FileStore)
               reporter (p/->ConsoleReporter)}}]
  (let [start-time (java.time.Instant/now)
        duration (or (:session/duration @state) (session-duration @state))]

    (swap! state assoc
           :session/start-time start-time
           :session/duration duration)

    (safe-future
     :timer-thread
     (when (:session/is-running @state)
       (p/report-session-start reporter
                               (:session/type @state)
                               (:task/name @state))

       (when (work-session? (:session/type @state))
         (p/report-stats
          reporter
          (calc-current-stats @state))))

     (loop []
       (let [now (java.time.Instant/now)
             elapsed (calc-elapsed-secs start-time now)]

         (when-not (s/valid? :session/State @state)
           (log/warn "Invalid state:"
                     (with-out-str (s/explain :session/State @state))))

         (cond
           (should-continue-session?
            (:session/is-running @state) elapsed duration)
           (do
             (when (whole-minute? elapsed)
               (p/report-time-remaining reporter (int (/ (- duration elapsed) 60))))
             (swap! state assoc :session/time-elapsed elapsed) ; for debugging
             (Thread/sleep 1000)
             (recur))

           (not-running-with-time-remaining?
            (:session/is-running @state) elapsed duration)
           (p/report-msg reporter "Timer paused")

           (session-time-expired? elapsed duration)
           (do
             (when (work-session? (:session/type @state))
               (p/save-session! store @state)
               (p/save-pomodoro! store {:pomodoro/timestamp now
                                        :task/name (:task/name @state)}))

             (p/notify notifier (session-type->complete-msg (:session/type @state)))
             (handle-session-completion! state))))))))


(defn stop-timer!
  "Stops the timer by setting the is-running flag to false in the state atom."
  [state]
  (swap! state assoc :session/is-running false))

(declare start-timer!)

(defn start-timer!
  "Starts the timer by setting the is-running flag to true and initiating the 
   timer thread."
  [state]
  (swap! state assoc :session/is-running true)
  (swap! state assoc :session/start-time (java.time.Instant/now))
  (timer-thread! state {:notifier (p/->SystemNotifier)
                        :store (p/->FileStore)
                        :reporter (p/->ConsoleReporter)}))


;;; ----------------------------------------------------------------------------
;;; UI
;;; ----------------------------------------------------------------------------


(defn start
  "Starts the Pomodoro timer with the default state."
  ([] (start nil))
  ([task-name]
   (load-last-session! state)
   (when task-name
     (swap! state assoc :task/name task-name))
   (start-timer! state)))


(defn stop
  "Stops (pauses) the Pomodoro timer."
  []
  (stop-timer! state))


(defn reset
  "Resets the Pomodoro timer to its initial state and starts it again for
   the current session."
  []
  (stop)
  (swap! state dissoc :session/duration :session/start-time)
  (start))


(defn skip
  "Skips the current session and moves to the next session type."
  []
  (swap! state assoc :session/type (next-session @state))
  (reset))


(defn setup-app!
  "Initializes the application by loading the last session state."
  []
  (load-last-session! state))

(defn help
  "Displays available commands and their descriptions."
  []
  (println "\nAvailable commands:\n")
  (doseq [[name var] (sort-by first (ns-publics 'pomodoroclj.core))
          :let [doc (:doc (meta var))]
          :when (and doc (#{'start 'stop 'reset 'skip} name))]
    (println (str name ":"))
    (println (str "  " doc "\n"))))


(setup-app!)


(comment
  (reset! state starting-state)

  (start "test caveman")
  (start "analyze sind")
  (skip)
  (stop)
  (reset)

  @state


  ; TODO
  ; - Stats:
  ; - weekly stats
  ; - monthly stats

  :end)
