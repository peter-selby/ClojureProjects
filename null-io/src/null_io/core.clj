(ns null-io.core
  (:use [clojure.java.io :as cjio])
  (:gen-class))

(defn get-current-directory []
  (. (java.io.File. ".") getCanonicalPath))

(defn do-per-line [f]
  (with-open [rdr (cjio/reader "input.txt")]
    (doseq [line (line-seq rdr)]
      (f line))))

(defn -main
  "Basic husk for programming problems."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println (get-current-directory))
  (let [input (slurp "input.txt")]
    (println input))
  (do-per-line println)
  (with-open [wrtr (cjio/writer "output.txt")]
    (do-per-line (fn [l] (.write wrtr l))))
  (slurp "output.txt")
  )
