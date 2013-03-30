(ns store-credit.core
  (:use [clojure.java.io :as cjio])
  (:use [clojure.string  :as cstr :only [split split-lines]])
  (:use [clojure.pprint  :as pp   :only [pprint]])
  (:gen-class))

;;; This is speed-code: no attention to error-checking or testing.

(defn case-from-lines [triple]
  {:credit (read-string (first triple))
   :nitems (read-string (nth triple 1))
   :prices (map read-string (cstr/split (nth triple 2) #"\s"))}
  )

(defn soln-from-case [cs]
  (let [c    (:credit cs)
        ps   (:prices cs)
        zs   (map-indexed (fn [i price] {price (inc i)}) (:prices cs))
        _    (pp/pprint zs)
        qs   (apply merge zs)
        _    (pp/pprint qs)
        fs   (filter identity (map #(qs (- c %)) ps))
        _    (pp/pprint fs)
        ]
    
    fs))

(defn -main
  "Solve codeJam practice problem https://code.google.com/codejam/contest/351101/dashboard#s=p0."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))

  (let [input (slurp "input.txt")
        lines (cstr/split-lines input)
        n (read-string (first lines))
        flat-cases (rest lines)
        cases (reduce
               (fn [cases triple] (conj cases (case-from-lines triple)))
               []
               (partition 3 flat-cases))
        solns (map soln-from-case cases)
        ready (map-indexed
               (fn [i soln]
                 (str "Case #" (inc i) ": " (first soln) " " (nth soln 1) "\n"))
               solns)]
    (with-open [w (cjio/writer "output.txt")]
      (doseq [line ready] (.write w line)))
    )
  'done
  )

