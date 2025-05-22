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

(defmacro safe-future [name & body]
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
  (s/keys :req [:session/type
                :session/is-running
                :session/time-elapsed]
          :opt [:task/name
                :session/duration
                :session/start-time
                :last-session/Stats]))

;;; ----------------------------------------------------------------------------
;;; State
;;; ----------------------------------------------------------------------------

(def duration
  "A map containing the duration in minutes for each session type."
  {:work 25
   :short-break 5
   :long-break 15})


; An atom containing the current state of the Pomodoro timer.
(def starting-state
  #:session{:type :work
            :is-running false
            :time-elapsed 0})

(defonce state
  (atom starting-state))


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
  (let [id-key (keyword collection "id")
        id (or (get record id-key) (next-id!))
        file (get-path db-path collection (str id))]
    (spit file (assoc record id-key id))))


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


(defn get-last-session []
  (get-by-id "settings" "last-session"))



(defn load-last-session! [state]
  (let [data (get-last-session)]
    (swap! state assoc :last-session/Stats
           (if data
             data
             #:last-session{:id "last-session"
                            :logged-at (java.time.Instant/now)
                            :pomodoros-completed 0}))))


(declare inst-same-date?)

(defn number-of-pomodoros-completed-today
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


(defn number-of-pomodoros-completed-in-current-cycle
  [pomodoros-completed-today]
  (let [current-cycle
        (mod pomodoros-completed-today 4)

        cycle-progress
        (repeat 4 "○")

        cycle-progress
        (map-indexed (fn [i dot]
                       (if (<= i current-cycle) "●" dot))
                     cycle-progress)]
    (apply str cycle-progress)))

(assert
 (= ["●○○○"
     "●●○○"
     "●●●○"
     "●●●●"]
    (mapv number-of-pomodoros-completed-in-current-cycle (range 4))))

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
  (case (:session/type state)
    :work (* 60 (:work duration))
    :short-break (* 60 (:short-break duration))
    :long-break (* 60 (:long-break duration))))


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


(defn stop-timer!
  "Stops the timer by setting the is-running flag to false in the state atom."
  [state]
  (swap! state assoc :session/is-running false))


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
  (let [msg (case (:session/type state)
              :work "Work session complete! Time for a break."
              :short-break "Short break over! Back to work."
              :long-break "Long break complete! Ready for a new session.")]
    (safe-future :say (say! msg))
    (safe-future :show-alert (show-alert! "Pomodoro Timer" msg))))


(declare start-timer!)

(defn timer-thread!
  "Manages the timer countdown in a separate thread.
   Updates the state every second, handles session transitions,
   and notifies the user when sessions complete."
  [state]
  (let [start-time (java.time.Instant/now)
        duration (or (:session/duration @state) (session-duration @state))]
    (swap! state assoc
           :session/start-time start-time
           :session/duration duration)
    (safe-future
     :timer-thread
     (load-last-session! state)
     (when (:session/is-running @state)
       (newline)
       (let [header
             (str "Starting " (-> @state :session/type name) " timer"
                  (when-let [task (:task/name @state)]
                    (str " for task: " task)))]
         (println header))
       (newline)

       (when (= :work (:session/type @state))
         (let [lsession (:last-session/Stats @state)
               logged-at (:last-session/logged-at lsession)

               pomodoros-completed (:last-session/pomodoros-completed lsession)

               pomodoros-completed-today
               (number-of-pomodoros-completed-today
                logged-at pomodoros-completed)]
           (println
            "Pomodoros completed today:" pomodoros-completed-today)

           (println
            "Current cycle:"
            (number-of-pomodoros-completed-in-current-cycle
             pomodoros-completed-today)))
         (newline)))

     (loop []
       (let [now (java.time.Instant/now)
             elapsed (-> now
                         (.getEpochSecond)
                         (- (.getEpochSecond start-time)))]

         (cond
            ; the timer is running and there's time remaining
           (and (:session/is-running @state)
                (< elapsed duration))
           (do
             (when (zero? (mod elapsed 60))
               (println (format "%d minutes remaining..."
                                (int (/ (- duration elapsed) 60)))))
             (swap! state assoc :session/time-elapsed elapsed) ; for debugging purposes
             (Thread/sleep 1000)
             (recur))

            ; the timer is paused (is not running and there's time remaining)
           (and (not (:session/is-running @state))
                (< elapsed duration))
           (do
             (println "Timer paused"))

            ; the timer done (is running and there's no time remaining)
           :else
           (do
             (when (#{:work} (:session/type @state))
               (if (get-by-id "settings" "last-session")
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
                             :task/name (:task/name @state)))))
                 (create! "settings"
                          {:settings/id "last-session"
                           :last-session/logged-at now
                           :task/name (:task/name @state)
                           :last-session/pomodoros-completed 1}))
               (create!
                "pomodoro"
                {:pomodoro/timestamp now
                 :task/name (:task/name @state)}))

             (notify-user! @state)
             (let [next-session-type (cond-> @state
                                       ;; - we must take into account that we 
                                       ;; justfinished a session, but haven't 
                                       ;; yet updated the state
                                       (= :work (:session/type @state))
                                       (update-in [:last-session/Stats
                                                   :last-session/pomodoros-completed]
                                                  inc)


                                       :always (next-session))]

               (swap! state dissoc
                      :session/time-elapsed
                      :session/duration
                      :session/start-time)
               (swap! state assoc
                      :session/is-running false
                      :session/type next-session-type)
               ;; Auto-start if the next session is a break
               (when (#{:short-break :long-break} next-session-type)
                 (start-timer! state))))))))))


(defn start-timer!
  "Starts the timer by setting the is-running flag to true and initiating the timer thread."
  [state]
  (swap! state assoc :session/is-running true)
  (swap! state assoc :session/start-time (java.time.Instant/now))
  (timer-thread! state))


;;; ----------------------------------------------------------------------------
;;; UI
;;; ----------------------------------------------------------------------------


(defn start
  "Starts the Pomodoro timer with the default state."
  ([] (start nil))
  ([task-name]
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


(defn setup-app! []
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

  (start "caveman")
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
