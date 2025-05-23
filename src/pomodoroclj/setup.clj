(ns pomodoroclj.setup
  (:require
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
