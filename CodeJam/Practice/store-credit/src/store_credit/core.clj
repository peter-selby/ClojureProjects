(ns store-credit.core
  (:use [clojure.java.io :as cjio])
  (:use [clojure.string  :as cstr])
  (:gen-class))

(defn get-current-directory []
  (. (java.io.File. ".") getCanonicalPath))

(defn do-per-line [f]
  (with-open [rdr (cjio/reader "input.txt")]
    (doseq [line (line-seq rdr)]
      (f line))))

(defn -main
  "Solve codeJam practice problem https://code.google.com/codejam/contest/351101/dashboard#s=p0."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))

  (println (get-current-directory))

  (let [input (slurp "input.txt")
        lines (cstr/split-lines input)]
    
    lines)



  #_(do-per-line println)

  #_(with-open [wrtr (cjio/writer "output.txt")]
    (do-per-line (fn [l] (.write wrtr l))))

  #_(slurp "output.txt")
  )

