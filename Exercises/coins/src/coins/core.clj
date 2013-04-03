(ns coins.core
;  (:use clojure.algo.monads)
  (:gen-class))

(defmacro dbg [x]
  `(let [x# ~x]
     (do (println '~x "~~>" x#)
         x#))) 

(defn bump-first [nums]
  (if (empty? nums) nums (cons (inc (first nums)) (rest nums))))

(defn bump-firsts [coll] (map bump-first coll))

(defn prepends [val coll] (map #(cons val %) coll))

(defn pay [amt species]
  (cond
   (empty? species) [[]]
   (= (count species) 1) (let [f (first species)
                               r [[(quot amt f) (rem amt f)]] ;_ (dbg r)
                               ]
                           r)
   :else (let [resid (- amt (first species))      ;_    (dbg resid)
               rspcs (rest species)               ;_    (dbg rspcs)
               sol0  (prepends 0 (pay amt rspcs)) ;_    (dbg sol0)
               ]
           (if (>= resid 0)
             (let [sol1 (pay resid species)       ;_    (dbg sol1)
                   sol2 (bump-firsts sol1)        ;_    (dbg sol2)                
                   res  (concat sol2 sol0)        ;_    (dbg res)
                   ]
               res)
             sol0))
   ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (pay 40 [25 10 5]))
