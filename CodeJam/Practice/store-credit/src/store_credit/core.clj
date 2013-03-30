(ns store-credit.core
  (:use [clojure.java.io :as cjio])
  (:use [clojure.string  :as cstr :only [split split-lines]])
  (:use [clojure.pprint  :as pp   :only [pprint]])
  (:gen-class))

;;; This is speed-code: very very light on error-checking and testing.

(defn get-current-directory []
  (. (java.io.File. ".") getCanonicalPath))

(defn do-per-line [f]
  (with-open [rdr (cjio/reader "input.txt")]
    (doseq [line (line-seq rdr)]
      (f line))))

(defn case-from-lines [triple]
  {:credit (read-string (first triple))
   :nitems (read-string (nth triple 1))
   :prices (map read-string (cstr/split (nth triple 2) #"\s"))}
  )

(defn -main
  "Solve codeJam practice problem https://code.google.com/codejam/contest/351101/dashboard#s=p0."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))

  (println (get-current-directory))

  (let [input (slurp "input.txt")
        lines (cstr/split-lines input)
        n (read-string (first lines))
        flat-cases (rest lines)
        cases (reduce
               (fn [cases triple] (conj cases (case-from-lines triple)))
               []
               (partition 3 flat-cases))]

    (pp/pprint cases)

    cases

    #_(map
     (fn [line] (println line))
     cases)

    )

  #_(do-per-line println)

  #_(with-open [wrtr (cjio/writer "output.txt")]
    (do-per-line (fn [l] (.write wrtr l))))

  #_(slurp "output.txt")
  )

