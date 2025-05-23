(ns pomodoroclj.datetime)

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

