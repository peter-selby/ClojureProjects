(ns mini-kanren.core-test
  (:refer-clojure :exclude [==])
  (:require [clojure.test :as test]
            [mini-kanren.core]
            [mini-kanren.utils]
            )
  (:use [clojure.core.logic]))

(test/deftest a-test
  (test/is (= 1 1))
  (test/is (= '(true) (run* [q] (== q true))))
  )

