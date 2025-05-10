(ns pomodoroclj.core
  (:require
   [clojure.edn]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.test :as test]))

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
    :time-remaining nil
    :pomodoros-completed 0
    :is-running false}))


;;; ----------------------------------------------------------------------------
;;; DB
;;; ----------------------------------------------------------------------------

(defn read-string* [s]
  (clojure.edn/read-string
   {:readers {'inst #(java.time.Instant/parse %)}}
   s))

(def db-path
  (str (System/getProperty "user.dir") "/db"))

(defn next-id! [] (System/currentTimeMillis))

(defn create! [collection record]
  (let [id (or (:_id record) (next-id!))
        file (io/file db-path collection (str id))]
    (when-not (-> file .getParentFile .exists)
      (-> file .getParentFile .mkdirs))
    (spit file (assoc record :_id id))))


(defn get-by-id [collection id]
  (let [file (io/file db-path collection (str id))]
    (-> file
        (slurp)
        (read-string*))))

(defn- update-data [original-data new-data]
  (if (test/function? new-data)
    (new-data original-data)
    (merge original-data new-data)))


(defn update!
  "Patches an existing entry."
  [collection id data-or-fn]
  (let [entry-file (io/file db-path collection (str id))]

    (if (.exists entry-file)

      (let [existing-data (read-string* (slurp entry-file))
            updated-data (update-data existing-data data-or-fn)]
        (spit entry-file (pr-str updated-data))
        updated-data)

      false)))


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
  (cond
    (and (= (:current-session state) :work)
         (pos? (:pomodoros-completed state))
         (zero? (mod (:pomodoros-completed state) 4)))
    :long-break

    (= (:current-session state) :work)
    :short-break

    :else
    :work))

(assert (= :short-break
           (next-session {:current-session :work :pomodoros-completed 0})))
(assert (= :work
           (next-session {:current-session :short-break :pomodoros-completed 0})))
(assert (= :work
           (next-session {:current-session :long-break :pomodoros-completed 0})))
(assert (= :long-break
           (next-session {:current-session :work :pomodoros-completed 4})))
(assert (= :long-break
           (next-session {:current-session :work :pomodoros-completed 8})))


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
    (future (future (say! msg)))
    (future (show-alert! "Pomodoro Timer" msg))))


(defn timer-thread!
  "Manages the timer countdown in a separate thread.
   Updates the state every second, handles session transitions,
   and notifies the user when sessions complete."
  [state]
  (let [initial-time (or (:time-remaining @state) (session-duration @state))]
    (swap! state assoc :time-remaining initial-time)
    (future
      (when (:is-running @state)
        (if-let [task (:task-name @state)]
          (println "Starting" (-> @state :current-session name) "timer for task:" task)
          (println "Starting" (-> @state :current-session name) "timer")))
      (loop []
        (let [remaining (:time-remaining @state)]
          (cond

            ; the timer is running and there's time remaining
            (and (:is-running @state)
                 (pos? remaining))
            (do
              (when (zero? (mod remaining 60))

                (println (format "%d minutes remaining..."
                                 (int (/ remaining 60)))))
              (swap! state update :time-remaining dec)
              (Thread/sleep 1000)
              (recur))

            ; the timer paused (is not running and there's time remaining)
            (and (not (:is-running @state))
                 (pos? remaining))
            nil

            ; the timer done (is running and there's no time remaining)
            :else
            (do

              (future
                (let [now (java.time.Instant/now)]
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
                    :task-name (:task-name @state)})))

              (notify-user! @state)
              (swap! state update :pomodoros-completed inc)
              (swap! state assoc
                     :is-running false
                     :current-session (next-session @state)
                     :time-remaining (session-duration @state)))))))))


(defn start-timer!
  "Starts the timer by setting the is-running flag to true and initiating the timer thread."
  [state]
  (swap! state assoc :is-running true)
  (timer-thread! state))


;;; ----------------------------------------------------------------------------
;;; UI
;;; ----------------------------------------------------------------------------


(defn start
  "Starts the Pomodoro timer with the default state."
  ([]
   (start-timer! state))
  ([task-name]
   (swap! state assoc :task-name task-name)
   (start-timer! state)))


(defn stop
  "Stops the Pomodoro timer."
  []
  (stop-timer! state))


(defn reset
  "Resets the Pomodoro timer to its initial state and starts it again."
  []
  (stop)
  (swap! state :time-remaining nil)
  (start))


(defn skip
  "Skips the current session and moves to the next session type."
  []
  (reset)
  (swap! state assoc :current-session (next-session @state))
  (start))


(comment
  (start "code")
  (skip)
  (stop)
  (reset)

  state

  :end)
