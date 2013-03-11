(ns reactive-backplane.test.elem
  (:refer-clojure :exclude (list get delete))
  (:use clojure.test
        [reactive-backplane.elem :as e]))

(deftest test-elem
  (testing "elem operations"
    (is (= (e/list) {}))
    (is (= (e/put 42 [:a 7])))
    (is (= (e/list) {42 {:a 7}}))
    ))
