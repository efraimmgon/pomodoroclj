(ns pomodoroclj.core
  (:require
   [clojure.edn]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.test :as test]
   [clojure.tools.logging :as log]
   [clojure.spec.alpha :as s]))

(defmacro safe-future [name & body]
  `(future
     (try
       ~@body
       (catch Throwable t#
         (log/error t# (str "Exception in future " ~name " -> " (.getMessage t#)))))))

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
(s/def :session/time-elapsed (s/and pos? int?))

(s/def session/State
  (s/keys :req [:session/type
                :session/is-running
                :session/time-elapsed]
          :opt [:task/name
                :session/duration
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


(s/def :last-session/id :common/id)
(s/def :last-session/logged-at :common/timestamp)
(s/def :last-session/pomodoros-completed (s/and int? #(not (neg? %))))

(s/def :last-session/Stats
  (s/keys :req [:last-session/id
                :last-session/logged-at
                :task/name
                :last-session/pomodoros-completed]))


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
   #:session{:type :work
             :is-running false
             :time-elapsed 0}))

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


(defn get-last-session []
  (get-by-id "user" "last-session"))


(defn load-last-session! [state]
  (let [data (get-last-session)]
    (swap! state merge
           (if data
             data
             #:last-session{:id "last-session"
                            :last-logged-at (java.time.Instant/now)
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
   (:last-session/logged-at @state)
   (:last-session/pomodoros-completed @state))


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
  (let [pomodoros-completed (:last-session/pomodoros-completed state)]
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
                          :last-session/pomodoros-completed 0})))
(assert (= :work
           (next-session {:session/type :short-break
                          :last-session/pomodoros-completed 0})))
(assert (= :work
           (next-session {:session/type :long-break
                          :last-session/pomodoros-completed 0})))
(assert (= :long-break
           (next-session {:session/type :work
                          :last-session/pomodoros-completed 4})))
(assert (= :long-break
           (next-session {:session/type :work
                          :last-session/pomodoros-completed 8})))


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
         (let [logged-at (:last-session/logged-at @state)

               pomodoros-completed (:last-session/pomodoros-completed @state)

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
             (swap! state assoc :time-elapsed elapsed) ; for debugging purposes
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
             (if (get-by-id "user" "last-session")
               (update!
                "user" "last-session"
                (fn [m]
                  (cond-> m
                    (inst-same-date? (:last-session/logged-at m) now)
                    (update :last-session/pomodoros-completed inc)

                    (not (inst-same-date? (:last-session/logged-at m) now))
                    (assoc :last-session/pomodoros-completed 1)

                    true
                    (assoc :last-sesion/logged-at now
                           :task/name (:task/name @state)))))
               (create! "user"
                        {:_id "last-session"
                         :last-session/logged-at now
                         :task/name (:task/name @state)
                         :last-session/pomodoros-completed 1}))

             (create!
              "pomodoro"
              {:pomodoro/timestamp now
               :task/name (:task/name @state)})

             (notify-user! @state)
             (let [next-session-type (next-session @state)]
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


(setup-app!)


(comment
  (start "testing pomodoro for bugs")
  (start "analyze sind")
  (skip)
  (stop)
  (reset)

  @state

  :end)
