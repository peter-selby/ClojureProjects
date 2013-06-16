(ns palindromics.core
  (:require [clojure.math.numeric-tower :as math]))

(defmacro dump [x]
  `(let [x# ~x]
     (do (println '~x "~~> ")
       (clojure.pprint/pprint x#)
       (println "")
       x#)))

(def i 1881)
(def is [2 9 11 88 99 181 191 999 1881 1991 9999 18281 18981 19991 99999 189981])
(odd? 42)

(def edward
  (fn [i]
    (letfn [(gary   [n]  (cons n (lazy-seq (gary (*' 10 n)))))
            (heidi  [n]  (take-while pos? (map (partial quot n) (gary 1))))
            (dave   [n]  (map #(mod % 10) (heidi n)))
            (karen  [n]  (count (dave n)))
            (oscar  [n]  (odd? (karen n)))
            (half   [n]  (let [ds (dave n), k (count ds), h (quot k 2), j
                               (if (odd? k) (inc h) h)]
                           (take j ds)))
            (fran   [ds] (apply +' (map *' (reverse ds) (gary 1))))
            (don    [n]  (dave (inc (fran (half n)))))
            (kathy  [n]  (count (don n)))
            ]
      (let [odd (oscar i), bump (> (kathy i) (count (half i)))]
        (fran
         (let [d (don i)]
           (if bump
             (let [r (reverse d)]
               (concat (if odd (butlast r) r) (rest d)))
             (concat (reverse d) (if odd (rest d) d)))))))))

(doseq [i is] (println (edward i)))
(println "")





