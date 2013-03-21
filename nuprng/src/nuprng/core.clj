(ns nuprng.core
  (:use clojure.pprint)
  (:require [clojure.math.numeric-tower :as mathEx]))

;;; This is the form of data returned by "frequencies,"
(def loaded-die {:1 37, :2 0, :3 17, :4 5, :5 12, :6 11})

(defn total [frqs] (apply + (map second frqs)))

(defn N           [frqs] (count frqs))
(defn S           [frqs] (total frqs))
(defn L           [frqs] (mathEx/lcm (N frqs) (S frqs)))
(defn H           [frqs] (/ (L frqs) (N frqs)))
(defn beef-factor [frqs] (/ (L frqs) (S frqs)))
(defn beefed      [frqs] (map #(vector
                                (first %)
                                (* (second %) (beef-factor frqs)))
                              frqs))

(defn fill-shortest [target [filled frqs]]
  (let [sorted   (sort-by second frqs)
        tallest  (last    sorted)
        shortest (first   sorted)
        deficit  (- target (second shortest))
        to-do    (drop 1 sorted)]
    {:filled
     (conj filled {:home shortest, :other [(first tallest) deficit]})
     :remaining
     (if (empty? to-do)
       to-do
       (conj (drop-last to-do)
             [(first tallest) (- (second tallest) deficit)]))}))

(defn redistribute [target frqs] ; TODO: precondition frqs not empty?
  (loop [result (fill-shortest target [[] frqs])]
    (if (empty? (:remaining result))
      (:filled result)
      (recur (fill-shortest target
                            [(:filled result) (:remaining result)])))))

(defn sample-1 [target redistributed]
  (let [bucket (rand-nth redistributed)
        height (rand-int target)]
    (if (< height (second (:home bucket)))
      (first (:home bucket))
      (first (:other bucket))
      )))

(defn sample [n redistributed]
  (let [ansatz (first redistributed)
        target (+ (second (:home ansatz)) (second (:other ansatz)))]
    (map (fn [_] (sample-1 target redistributed))
         (range n))))

(defn -main [] (pprint
                ["loaded-die" loaded-die
                 ,"count" (N loaded-die)
                 ,"total" (S loaded-die)
                 ,"gcd count total" (mathEx/gcd (N loaded-die) (S loaded-die))
                 ,"beefed-up total" (L loaded-die)
                 ,"target height:" (H loaded-die)
                 ,"beefed-up heights" (beefed loaded-die)
                 ,"total beefed-up heights" (total (beefed loaded-die))
                 ,"tallest and shortest" (fill-shortest (H loaded-die) [[] (beefed loaded-die)])
                 ,"redistributed" (redistribute (H loaded-die) (beefed loaded-die))
                 ]))

