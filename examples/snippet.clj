(ns examples.snippet
  (:use [clojure.contrib.sql]))

; START: db
; replace "snippet-db" with a full path!
(def db {:classname "org.hsqldb.jdbcDriver"
         :subprotocol "hsqldb"
         :subname "file:snippet-db"})
; END: db

(defn drop-snippets []
  (try
   (drop-table :snippets)
   (catch Exception e)))

; START: create-snippets
(defn create-snippets []
  (create-table :snippets
    [:id :int "IDENTITY" "PRIMARY KEY"]
    [:body :varchar "NOT NULL"]
    [:created_at :datetime]))
; END: create-snippets

; START: insert-snippets
(defn now [] (java.sql.Timestamp. (.getTime (java.util.Date.)))) 
(defn insert-snippets []
  (let [created_at (now)]
    (seq 
     (insert-values :snippets
      [:body :created_at]		     
      ["(println :boo)" created_at]
      ["(defn foo [] 1)" created_at]))))
; END: insert-snippets

(defn sample-snippets []
  (with-connection db
    (transaction
     (drop-snippets)
     (create-snippets)
     (insert-snippets))))

(defn reset-snippets []
  (with-connection db
    (transaction
     (drop-snippets)
     (create-snippets))))

(defn ensure-snippets-table-exists []
  (try
   (with-connection db (create-snippets))
   (catch java.sql.BatchUpdateException _)))
  

; START: print-snippets
(defn print-snippets []
  (with-results res "select * from snippets"
    (println res)))
; END: print-snippets
 
; START: broken-select-snippets
; Broken!
(defn select-snippets []
  (with-results res "select * from snippets" res))
; END: broken-select-snippets
(def broken-select-snippets select-snippets)

(defmulti coerce (fn [dest-class src-inst] [dest-class (class src-inst)]))
(defmethod coerce [Integer String] [_ inst] (Integer/parseInt inst))
(defmethod coerce :default [dest-cls obj] (cast dest-cls obj))

; START: select-snippets
(defn select-snippets []
  (with-connection db
    (with-results res "select * from snippets" (doall res))))
; END: select-snippets

; START: sql-query
(defn sql-query [q]
  (with-results res q (doall res)))
; END: sql-query

(defn select-snippet [id]
  (with-connection db
    (first (sql-query (str "select * from snippets where id = " (coerce Integer id))))))

; START: last-created-id
(defn last-created-id 
  "Extract the last created id. Must be called in a transaction
   that performed an insert. Expects HSQLDB return structure of
   the form [{:@p0 id}]."
  []
  (first (vals (first (sql-query "CALL IDENTITY()")))))
; END: last-created-id

; START: insert-snippet
(defn insert-snippet [body]
  (with-connection db
    (transaction
     (insert-values :snippets
       [:body :created_at]
       [body (now)])
     (last-created-id))))
; END: insert-snippet
