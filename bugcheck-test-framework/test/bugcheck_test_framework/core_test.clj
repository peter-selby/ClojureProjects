(ns bugcheck-test-framework.core-test
  (:use clojure.test
        bugcheck-test-framework.core))

(deftest a-test
  (testing "Testing test itself."
    (is (= 1 1))
    (is (= v 4))
    ))


