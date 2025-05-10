(ns user
  (:require
   [clojure.tools.namespace.repl :refer [refresh]]
   clojure+.hashp
   clojure+.error
   clojure+.print))

(defn restart []
  (refresh))

(clojure+.hashp/install!)
(clojure+.error/install! {:reverse? true})
(clojure+.print/install!)