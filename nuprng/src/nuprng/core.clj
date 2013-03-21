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

(defn fill-shortest [frqs]
  (let [sorted (sort-by second frqs)
        tallest (last sorted)
        shortest (first sorted)
        deficit (- (second tallest) (second shortest))]
    {}))

(defn -main [] (
                pprint
                {"count" (N loaded-die)
                 ,"total" (S loaded-die)
                 ,"gcd count total" (mathEx/gcd (N loaded-die) (S loaded-die))
                 ,"beefed-up total" (L loaded-die)
                 ,"target height:" (H loaded-die)
                 ,"beefed-up heights" (beefed loaded-die)
                 ,"total beefed-up heights" (total (beefed loaded-die))
                 ,"tallest and shortest" (fill-shortest (beefed loaded-die))
                 }))

