(ns always-turn-left.utils
  (:use clojure.java.io))
 
(def base-res "data/")
 
(defn read-case-lines [case-name]
  (let [res (resource (str base-res case-name))]
    (with-open [rdr (reader res)]
      (doall (line-seq rdr)))))
   
(defn read-cases [case-name]
  (let [lines (read-case-lines (str case-name ".in"))
        case-count (Integer/parseInt (first lines))
        cases (doall (rest lines))]
    {:case-count case-count
     :cases cases}))
   
(defn write-output [output case-name]
  (let [in-res (resource (str base-res case-name ".in"))
        in-file-name (.getFile in-res)
        out-file-name (clojure.string/replace in-file-name #".in$" ".out")]
    (with-open [wr (writer out-file-name)]
      (doseq [[case-no case-out] (map vector (range 1 (inc (count output))) output)]
        (.write wr (str "Case #" case-no ": " case-out "\n"))))))