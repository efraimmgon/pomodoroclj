(ns pomodoroclj.core
  (:require
   [clojure.edn]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.test :as test]
   [clojure.tools.logging :as log]
   [clojure.spec.alpha :as s]
   clojure+.hashp
   clojure+.error
   clojure+.print))


;;; ----------------------------------------------------------------------------
;;; Env setup
;;; ----------------------------------------------------------------------------

; we currently use the reader to store an #instant into the db
(clojure+.hashp/install!)
(clojure+.error/install! {:reverse? true})
(clojure+.print/install!)


;;; ----------------------------------------------------------------------------
;;; Utils
;;; ----------------------------------------------------------------------------

(defmacro safe-future
  "Executes body in a future with error handling. Logs any exceptions that occur during execution.
   Takes a name parameter for identification in error messages."
  [name & body]
  `(future
     (try
       ~@body
       (catch Throwable t#
         (log/error t# (str "Exception in future " '~name " -> " (.getMessage t#)))))))


;;; ----------------------------------------------------------------------------
;;; Specs
;;; ----------------------------------------------------------------------------

(s/def :common/timestamp #(instance? java.time.Instant %))
(s/def :common/id (s/and pos? int?))

(s/def :task/name (s/nilable string?))

(s/def :session/type #{:work :short-break :long-break})
(s/def :session/duration (s/and pos? int?))
(s/def :session/is-running boolean?)
(s/def :session/start-time :common/timestamp)
(s/def :session/time-elapsed (s/and #(not (neg? %)) int?))

(s/def :session/Session
  (s/keys :req [:session/type
                :session/is-running
                :session/time-elapsed]
          :opt [:session/duration
                :session/start-time]))

(s/def :pomodoro/id :common/id)
(s/def :pomodoro/timestamp :common/timestamp)

(s/def :pomodoro/New
  (s/keys :req [:task/name
                :pomodoro/timestamp]))

(s/def :pomodoro/Pomodoro
  (s/merge :pomodoro/New
           (s/keys :req
                   [:pomodoro/id])))


(s/def :settings/id string?)
(s/def :last-session/logged-at :common/timestamp)
(s/def :last-session/pomodoros-completed (s/and int? #(not (neg? %))))

(s/def :last-session/Stats
  (s/keys :req [:settings/id
                :last-session/logged-at
                :last-session/pomodoros-completed]
          :opt [:task/name]))


(s/def :session/State
  (s/merge
   :session/Session
   (s/keys :opt [:task/name
                 :last-lession/Stats])))

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
;;; DB
;;; ----------------------------------------------------------------------------

(defn read-string*
  "Reads an EDN string with custom readers for instant type.
   Used for reading serialized data from files."
  [s]
  (clojure.edn/read-string
   {:readers {'instant #(java.time.Instant/parse %)}}
   s))

(def db-path
  (str (System/getProperty "user.dir") "/db"))

(defn get-path
  "Creates and returns a file path, ensuring parent directories exist.
   Takes variable number of path segments as arguments."
  [& args]
  (let [file (apply io/file args)
        parent (.getParentFile file)]
    (when-not (.exists parent)
      (.mkdirs parent))
    file))

(defn next-id!
  "Generates a unique ID using current system time in milliseconds."
  []
  (System/currentTimeMillis))

(defn create!
  "Creates a new record in the specified collection.
   Automatically assigns an ID if not provided in the record."
  [collection record]
  (let [id-key (keyword collection "id")
        id (or (get record id-key) (next-id!))
        file (get-path db-path collection (str id))]
    (spit file (assoc record id-key id))))


(defn get-by-id
  "Retrieves a record from the specified collection by its ID.
   Returns nil if the record doesn't exist."
  [collection id]
  (let [file (get-path db-path collection (str id))]
    (when (.exists file)
      (-> file
          (slurp)
          (read-string*)))))

(defn- update-data
  "Updates data by either applying a function to it or merging with new data."
  [original-data new-data]
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


(defn get-last-session
  "Retrieves the last session data from the settings collection."
  []
  (get-by-id "settings" "last-session"))


(defn load-last-session!
  "Loads the last session data into the state atom.
   Creates default session data if none exists."
  [state]
  (let [data (get-last-session)]
    (swap! state assoc :last-session/Stats
           (if (seq data)
             data
             #:last-session{:id "last-session"
                            :logged-at (java.time.Instant/now)
                            :pomodoros-completed 0}))))


(declare inst-same-date?)

(defn pomodoros-completed-today
  "Returns the number of pomodoros completed today.
   Returns 0 if logged-at is nil, pomodoros-completed is nil, or if logged-at is not from today."
  [logged-at pomodoros-completed]
  (if (and logged-at
           pomodoros-completed
           (inst-same-date?
            (java.time.Instant/now)
            logged-at))
    pomodoros-completed
    0))

#_(number-of-pomodoros-completed-today
   (get-in @state [:last-session/Stats :last-session/logged-at])
   (get-in @state [:last-session/Stats :last-session/pomodoros-completed]))


(defn current-cycle
  "Returns a string representation of the current pomodoro cycle progress.
   Uses filled (●) and empty (○) circles to show completed and remaining pomodoros in the cycle."
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
;;; Date Time
;;; ----------------------------------------------------------------------------


(defn instant->local-date
  "Converts an Instant to a LocalDate using the system default timezone or a specified zone."
  ([inst]
   (instant->local-date inst (java.time.ZoneId/systemDefault)))
  ([inst zone-id]
   (-> inst
       (.atZone zone-id)
       (.toLocalDate))))


(defn inst-same-date?
  "Returns true if two instants represent the same date in the system timezone."
  [x y]
  (= (instant->local-date x)
     (instant->local-date y)))

(assert (inst-same-date? (java.time.Instant/now) (java.time.Instant/now)))
(assert (not (inst-same-date? (java.time.Instant/now)
                              (.plus (java.time.Instant/now) 1
                                     java.time.temporal.ChronoUnit/DAYS))))


;;; ----------------------------------------------------------------------------
;;; Utils
;;; ----------------------------------------------------------------------------

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

(defprotocol Notifier
  (notify [this message]))

(defprotocol PomodoroStore
  (save-session! [this state])
  (save-pomodoro! [this pomodoro]))

(defprotocol ProgressReporter
  (report-msg [this msg])
  (report-time-remaining [this remaining-time])
  (report-session-start [this session-type task])
  (report-stats [this stats]))

(defrecord SystemNotifier []
  Notifier
  (notify [_ msg]
    (safe-future :say (say! msg))
    (safe-future :show-alert (show-alert! "Pomodoro Timer" msg))))

(defrecord ConsoleReporter []
  ProgressReporter
  (report-msg [_this msg]
    (println msg))

  (report-time-remaining [_this remaining-mins]
    (println (format "%d minutes remaining ..." remaining-mins)))

  (report-session-start [_this session-type task]
    (println (str "Starting " (name session-type) " timer"
                  (when task (str " for task: " task)))))

  (report-stats [_this {:keys [pomodoros-completed-today cycle-progress]}]
    (println "Pomodoros completed today:" pomodoros-completed-today)
    (println "Current cycle:" cycle-progress)
    (newline)))

(defrecord FileStore []
  PomodoroStore
  (save-session! [_ state]
    (let [now (java.time.Instant/now)]
      (if (zero? (-> state
                     :last-session/Stats
                     :last-session/pomodoros-completed))
        (create! "settings"
                 {:settings/id "last-session"
                  :last-session/logged-at now
                  :task/name (:task/name state)
                  :last-session/pomodoros-completed 1})
        (update!
         "settings" "last-session"
         (fn [m]
           (cond-> m
             (inst-same-date? (:last-session/logged-at m) now)
             (update :last-session/pomodoros-completed inc)

             (not (inst-same-date? (:last-session/logged-at m) now))
             (assoc :last-session/pomodoros-completed 1)

             true
             (assoc :last-session/logged-at now
                    :task/name (:task/name state))))))))

  (save-pomodoro! [_ pomodoro]
    (create! "pomodoro" pomodoro)))


(defn calc-elapsed-secs
  "Calculates the elapsed time in seconds between start-time and now."
  [start-time now]
  (- (.getEpochSecond now)
     (.getEpochSecond start-time)))

(defn should-continue-session?
  "Returns true if the session is running and has not exceeded its duration."
  [is-running elapsed duration]
  (and is-running (< elapsed duration)))

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
  "Calculates current session statistics including pomodoros completed today and cycle progress."
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

(assert (= :short-break
           (next-session {:session/type :work
                          :last-session/Stats {:last-session/pomodoros-completed 0}})))
(assert (= :work
           (next-session {:session/type :short-break
                          :last-session/Stats {:last-session/pomodoros-completed 0}})))
(assert (= :work
           (next-session {:session/type :long-break
                          :last-session/Stats {:last-session/pomodoros-completed 0}})))
(assert (= :long-break
           (next-session {:session/type :work
                          :last-session/Stats {:last-session/pomodoros-completed 4}})))
(assert (= :long-break
           (next-session {:session/type :work
                          :last-session/Stats {:last-session/pomodoros-completed 8}})))

(defn session-duration
  "Returns the duration of the next session in seconds."
  [state]
  (case (:session/type state)
    :work (* 60 (:work duration))
    :short-break (* 60 (:short-break duration))
    :long-break (* 60 (:long-break duration))))

(declare start)

(defn handle-session-completion!
  "Handles the completion of a session by updating the state and starting the next session if appropriate.
   Increments pomodoro count for work sessions and transitions to the next session type."
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
  "Creates and manages a timer thread that handles session timing, notifications, and state updates.
   Takes a state atom and optional notifier, store, and reporter components."
  [state {:keys [notifier store reporter]
          :or {notifier (->SystemNotifier)
               store (->FileStore)
               reporter (->ConsoleReporter)}}]
  (let [start-time (java.time.Instant/now)
        duration (or (:session/duration @state) (session-duration @state))]

    (swap! state assoc
           :session/start-time start-time
           :session/duration duration)

    (safe-future
     :timer-thread
     (when (:session/is-running @state)
       (report-session-start reporter
                             (:session/type @state)
                             (:task/name @state))

       (when (work-session? (:session/type @state))
         (report-stats
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
               (report-time-remaining reporter (int (/ (- duration elapsed) 60))))
             (swap! state assoc :session/time-elapsed elapsed)
             (Thread/sleep 1000)
             (recur))

           (not-running-with-time-remaining?
            (:session/is-running @state) elapsed duration)
           (report-msg reporter "Timer paused")

           :else
           (do
             (when (work-session? (:session/type @state))
               (save-session! store @state)
               (save-pomodoro! store {:pomodoro/timestamp now
                                      :task/name (:task/name @state)}))

             (notify notifier (session-type->complete-msg (:session/type @state)))
             (handle-session-completion! state))))))))

;;; ----------------------------------------------------------------------------
;;; Utils
;;; ----------------------------------------------------------------------------

(defn stop-timer!
  "Stops the timer by setting the is-running flag to false in the state atom."
  [state]
  (swap! state assoc :session/is-running false))




(declare start-timer!)

(defn start-timer!
  "Starts the timer by setting the is-running flag to true and initiating the timer thread."
  [state]
  (swap! state assoc :session/is-running true)
  (swap! state assoc :session/start-time (java.time.Instant/now))
  (timer-thread! state {:notifier (->SystemNotifier)
                        :store (->FileStore)
                        :reporter (->ConsoleReporter)}))

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

  (s/valid? :session/State @state)
  (with-out-str (s/explain :session/State @state))

  @state


  ; TODO
  ; - Stats:
  ; - weekly stats
  ; - monthly stats

  :end)
