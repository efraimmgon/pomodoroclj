(ns pomodoroclj.utils
  (:require
   [clojure.tools.logging :as log]))

;;; ----------------------------------------------------------------------------
;;; Utils
;;; ----------------------------------------------------------------------------

;; Note: we need this because we're lazy and we're using `future`s
;; for concorrency but we don't want to have the trouble of
;; `deref`encing them.
;; Without this we have no access to the error msgs.
(defmacro safe-future
  "Executes body in a future with error handling. Logs any exceptions that occur during execution.
   Takes a name parameter for identification in error messages."
  [name & body]
  `(future
     (try
       ~@body
       (catch Throwable t#
         (log/error t# (str "Exception in future " '~name " -> " (.getMessage t#)))))))

