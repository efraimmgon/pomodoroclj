(ns pomodoroclj.core
  (:require
   [clojure.edn]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.test :as test]
   [clojure.tools.logging :as log]))

(defmacro safe-future [name & body]
  `(future
     (try
       ~@body
       (catch Throwable t#
         (log/error t# (str "Exception in future " ~name " -> " (.getMessage t#)))))))

;;; ----------------------------------------------------------------------------
;;; State
;;; ----------------------------------------------------------------------------


(def duration
  "A map containing the duration in minutes for each session type."
  {:work 25
   :short-break 5
   :long-break 15})


; An atom containing the current state of the Pomodoro timer.
(defonce state
  (atom
   {:task-name nil
    :current-session :work
    :duration nil
    :is-running false
    :start-time nil}))

;;; ----------------------------------------------------------------------------
;;; DB
;;; ----------------------------------------------------------------------------

(defn read-string* [s]
  (clojure.edn/read-string
   {:readers {'instant #(java.time.Instant/parse %)}}
   s))

(def db-path
  (str (System/getProperty "user.dir") "/db"))

(defn get-path [& args]
  (let [file (apply io/file args)
        parent (.getParentFile file)]
    (when-not (.exists parent)
      (.mkdirs parent))
    file))

(defn next-id! [] (System/currentTimeMillis))

(defn create! [collection record]
  (let [id (or (:_id record) (next-id!))
        file (get-path db-path collection (str id))]
    (spit file (assoc record :_id id))))


(defn get-by-id [collection id]
  (let [file (get-path db-path collection (str id))]
    (when (.exists file)
      (-> file
          (slurp)
          (read-string*)))))

(defn- update-data [original-data new-data]
  (if (test/function? new-data)
    (new-data original-data)
    (merge original-data new-data)))


(defn update!
  "Update an existing entry."
  [collection id data-or-fn]
  (let [entry-file (get-path db-path collection (str id))
        existing-data (read-string* (slurp entry-file))
        updated-data (update-data existing-data data-or-fn)]
    (spit entry-file (pr-str updated-data))
    updated-data))

;;; ----------------------------------------------------------------------------
;;; DB: Pomodoro specific


(defn get-aggregate-data []
  (try (get-by-id "user" "aggregate-data")
       (catch java.io.FileNotFoundException e
         nil)))


(defn load-aggregate-data! [state]
  (let [data (get-aggregate-data)]
    (swap! state assoc :aggregate-data
           (if data data
               {:_id "aggregate-data"
                :last-logged-at (java.time.Instant/now)
                :last-task nil
                :pomodoros-completed 0}))))



(declare inst-same-date?)

(defn number-of-pomodoros-completed-today [state]
  (let [msg "Pomodoros completed today:"

        num-of-pomodoros
        (if (inst-same-date?
             (java.time.Instant/now)
             (get-in @state [:aggregate-data :last-logged-at]))
          (get-in @state [:aggregate-data :pomodoros-completed])
          0)]
    (println msg num-of-pomodoros)))


(defn number-of-pomodoros-completed-in-current-cycle [state]
  (let [total-pomodoros
        (get-in @state [:aggregate-data :pomodoros-completed] 0)

        current-cycle
        (mod total-pomodoros 4)

        cycle-progress
        (repeat 4 "○")

        cycle-progress
        (map-indexed (fn [i dot]
                       (if (< i current-cycle) "●" dot))
                     cycle-progress)]
    (println "Current cycle:" (apply str cycle-progress))))


;;; ----------------------------------------------------------------------------
;;; Date Time
;;; ----------------------------------------------------------------------------


(defn instant->local-date
  ([inst]
   (instant->local-date inst (java.time.ZoneId/systemDefault)))
  ([inst zone-id]
   (-> inst
       (.atZone zone-id)
       (.toLocalDate))))


(defn inst-same-date? [x y]
  (= (instant->local-date x)
     (instant->local-date y)))

(assert (inst-same-date? (java.time.Instant/now) (java.time.Instant/now)))
(assert (not (inst-same-date? (java.time.Instant/now)
                              (.plus (java.time.Instant/now) 1
                                     java.time.temporal.ChronoUnit/DAYS))))

;;; ----------------------------------------------------------------------------
;;; Utils
;;; ----------------------------------------------------------------------------


(defn session-duration
  "Returns the duration of the next session in seconds."
  [state]
  (case (:current-session state)
    :work (* 60 (:work duration))
    :short-break (* 60 (:short-break duration))
    :long-break (* 60 (:long-break duration))))


(defn next-session
  "Determines the next session type based on the current state.
   Returns :long-break after 4 work sessions, :short-break after a work session,
   and :work after any break."
  [state]
  (let [pomodoros-completed (get-in state [:aggregate-data :pomodoros-completed])]
    (cond
      (and (= (:current-session state) :work)
           (pos? pomodoros-completed)
           (zero? (mod pomodoros-completed 4)))
      :long-break

      (= (:current-session state) :work)
      :short-break

      :else
      :work)))

(assert (= :short-break
           (next-session {:current-session :work
                          :aggregate-data {:pomodoros-completed 0}})))
(assert (= :work
           (next-session {:current-session :short-break
                          :aggregate-data {:pomodoros-completed 0}})))
(assert (= :work
           (next-session {:current-session :long-break
                          :aggregate-data {:pomodoros-completed 0}})))
(assert (= :long-break
           (next-session {:current-session :work
                          :aggregate-data {:pomodoros-completed 4}})))
(assert (= :long-break
           (next-session {:current-session :work
                          :aggregate-data {:pomodoros-completed 8}})))


(defn stop-timer!
  "Stops the timer by setting the is-running flag to false in the state atom."
  [state]
  (swap! state assoc :is-running false))


(defn show-alert!
  "Shows a system alert dialog with the given title and message using AppleScript."
  [title message]
  (let [script (format "display alert \"%s\" message \"%s\" buttons {\"OK\"} default button \"OK\""
                       title
                       message)]
    (shell/sh "osascript" "-e" script)))


(defn say!
  "Uses the system's text-to-speech to speak the given message."
  [msg]
  (shell/sh "say" msg))


(defn notify-user!
  "Notifies the user about session completion using both text-to-speech and a system alert.
   The message varies based on the type of session that just completed."
  [state]
  (let [msg (case (:current-session state)
              :work "Work session complete! Time for a break."
              :short-break "Short break over! Back to work."
              :long-break "Long break complete! Ready for a new session.")]
    (safe-future :say (say! msg))
    (safe-future :show-alert (show-alert! "Pomodoro Timer" msg))))


(defn timer-thread!
  "Manages the timer countdown in a separate thread.
   Updates the state every second, handles session transitions,
   and notifies the user when sessions complete."
  [state]
  (let [start-time (java.time.Instant/now)
        duration 10]
        ;duration (or (:duration @state) (session-duration @state))]
    (swap! state assoc
           :start-time start-time
           :duration duration)
    (safe-future
     :timer-thread
     (load-aggregate-data! state)
     (when (:is-running @state)
       (newline)
       (let [header
             (str "Starting " (-> @state :current-session name) " timer"
                  (when-let [task (:task-name @state)]
                    (str " for task: " task)))]
         (println header))
       (newline)
       (number-of-pomodoros-completed-today state)
       (number-of-pomodoros-completed-in-current-cycle state)
       (newline))
     (loop []
       (let [now (java.time.Instant/now)
             elapsed (-> now
                         (.getEpochSecond)
                         (- (.getEpochSecond start-time)))]

         (cond
            ; the timer is running and there's time remaining
           (and (:is-running @state)
                (< elapsed duration))
           (do
             (when (zero? (mod elapsed 60))
               (println (format "%d minutes remaining..."
                                (int (/ (- duration elapsed) 60)))))
             (swap! state assoc :time-elapsed elapsed) ; for debugging purposes
             (Thread/sleep 1000)
             (recur))

            ; the timer is paused (is not running and there's time remaining)
           (and (not (:is-running @state))
                (< elapsed duration))
           (do
             (println "Timer paused"))

            ; the timer done (is running and there's no time remaining)
           :else
           (do
             (if (get-by-id "user" "aggregate-data")
               (update!
                "user" "aggregate-data"
                (fn [m]
                  (cond-> m
                    (inst-same-date? (:last-logged-at m) now)
                    (update :pomodoros-completed inc)

                    (not (inst-same-date? (:last-logged-at m) now))
                    (assoc :pomodoros-completed 1)

                    true
                    (assoc :last-logged-at now
                           :last-task (:task-name @state)))))
               (create! "user"
                        {:_id "aggregate-data"
                         :last-logged-at now
                         :last-task (:task-name @state)
                         :pomodoros-completed 1}))

             (create!
              "pomodoro"
              {:timestamp now
               :task-name (:task-name @state)})

             (notify-user! @state)
             (swap! state assoc
                    :is-running false
                    :time-elapsed nil
                    :duration nil
                    :start-time nil
                    :current-session (next-session @state)))))))))


(defn start-timer!
  "Starts the timer by setting the is-running flag to true and initiating the timer thread."
  [state]
  (swap! state assoc :is-running true)
  (swap! state assoc :start-time (java.time.Instant/now))
  (timer-thread! state))


;;; ----------------------------------------------------------------------------
;;; UI
;;; ----------------------------------------------------------------------------


(defn start
  "Starts the Pomodoro timer with the default state."
  ([] (start nil))
  ([task-name]
   (swap! state assoc :task-name task-name)
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
  (swap! state assoc
         :duration nil
         :start-time nil)
  (start))


(defn skip
  "Skips the current session and moves to the next session type."
  []
  (swap! state assoc :current-session (next-session @state))
  (reset))


(defn setup-app! []
  (load-aggregate-data! state))

(setup-app!)

(comment
  (start "code pomodoro")
  (skip)
  (stop)
  (reset)

  @state

  :end)
