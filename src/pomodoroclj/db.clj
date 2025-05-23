(ns pomodoroclj.db
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.test :as test]))

;;; ----------------------------------------------------------------------------
;;; DB
;;; ----------------------------------------------------------------------------

(defn read-string*
  "Reads an EDN string with custom readers for instant type.
   Used for reading serialized data from files."
  [s]
  (edn/read-string
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
