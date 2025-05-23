(ns pomodoroclj.protocols
  (:require
   [clojure.java.shell :as shell]
   [pomodoroclj.datetime :as datetime]
   [pomodoroclj.db :as db]
   [pomodoroclj.utils :refer [safe-future]]))

;;; ----------------------------------------------------------------------------
;;; Protocols
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
        (db/create! "settings"
                    {:settings/id "last-session"
                     :last-session/logged-at now
                     :task/name (:task/name state)
                     :last-session/pomodoros-completed 1})
        (db/update!
         "settings" "last-session"
         (fn [m]
           (cond-> m
             (datetime/inst-same-date? (:last-session/logged-at m) now)
             (update :last-session/pomodoros-completed inc)

             (not (datetime/inst-same-date? (:last-session/logged-at m) now))
             (assoc :last-session/pomodoros-completed 1)

             true
             (assoc :last-session/logged-at now
                    :task/name (:task/name state))))))))

  (save-pomodoro! [_ pomodoro]
    (db/create! "pomodoro" pomodoro)))

