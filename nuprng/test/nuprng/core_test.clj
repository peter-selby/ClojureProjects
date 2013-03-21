(ns nuprng.core-test
  (:use clojure.test)
  (:require [nuprng.core :as core])
  ;; Alternative:
  ;; (:use [nuprng.core :as core :exclude 'loaded-die])

  )

(def ^:private loaded-die {:A 37, :B 0, :C 17, :D 5, :E 12, :F 11})

(deftest a-test
  (testing "Sampling a given distribution."
    (is (= (core/S loaded-die)  82))
    (is (= (core/N loaded-die)   6))
    (is (= (core/L loaded-die) 246))
    (is (= (core/H loaded-die)  41))
    ))
