(ns mini-kanren.core-test
  (:refer-clojure :exclude [==])
  (:require [clojure.test :as test]
            [mini-kanren.core :refer :all :as mk]
            )
  (:use [clojure.core.logic]))

(test/deftest a-test
  (test/is (= 1 1))
  )

