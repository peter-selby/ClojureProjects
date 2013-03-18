(ns reactive-backplane.test.elem
  (:refer-clojure :exclude (list get delete))
  (:use clojure.test
        [reactive-backplane.elem :as e]))

;;; see http://richhickey.github.com/clojure/clojure.test-api.html

(is true "Test presence of the test framework first.")

(deftest test-elem-normal-operations
  (testing "Sequential test of service state as a stateful atom, normal operations."
    (is (= (e/clear           ) {}         ) "Clear the service state.")
    (is (= (e/list            ) {}         ) "Show the service state.")
    (is (= (e/get    42       ) nil        ) "Test missing key.")
    (is (= (e/put    42 [:a 7]) {:a 7}     ) "Key-value vectors converted to maps.")
    (is (= (e/list            ) {42 {:a 7}}) "State with nested maps.")
    (is (= (e/get    42       ) {:a 7}     ) "Value for a key.")
    (is (= (e/get    37       ) nil        ) "Test another missing key.")
    (is (= (e/put    37 [:b 8]) {:b 8}     ) "When putting, produce only the value put.")
    (is (= (e/list            ) {37 {:b 8}, 42 {:a 7}})) "Show entire service state as a map."
    (is (= (e/delete 42       ) {:a 7}     ) "Delete produces the value deleted.") 
    (is (= (e/get    42       ) nil        ) "Key should be missing after delete.")
    (is (= (e/delete 42       ) nil        ) "Deleting a missing key produces nil.")     
    (is (= (e/list            ) {37 {:b 8}}) "Remaining keys and values left by delete.") 
    (is (= (e/clear           ) {}         ) "Clear produces empty map.")
    (is (= (e/delete 42       ) nil        ) "Deleting from empty map is harmless.")     
    (is (= (e/list            ) {}         ) "Empty map should show as empty.")
    ))

(deftest test-elem-abnormal-operations
  (testing "Sequential test of stateful elem atom, abnormal operations."
    (is (thrown? clojure.lang.ArityException (e/put)))
    (is (thrown? clojure.lang.ArityException (e/put 42)))
    (is (thrown? clojure.lang.ArityException (e/list 42)))
    (is (thrown? IllegalArgumentException (e/put 42 5)))
    ))

