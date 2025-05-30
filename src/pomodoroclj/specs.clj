(ns pomodoroclj.specs
  (:require
   [clojure.spec.alpha :as s]))

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
