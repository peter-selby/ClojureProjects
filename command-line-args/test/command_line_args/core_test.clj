(ns command-line-args.core-test
  (:use clojure.test
        command-line-args.core))

(deftest pair-of-values
  (let [args ["--server" "localhost"
              "--port" "8080"
              "--environment" "production"]]
    (is (= {:server "localhost"
            :port "8080"
            :environment "production"}
           (parse-args args)))))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1
           1))))

(deftest b-test
  (testing "keywordize [\"a\"] = [:a nil] -- missing value defaults to nil"
    (is (= (keywordize ["a"])
           [:a nil]))))

(deftest c-001-test
  (testing "(keywordize [\"a\" 1 \"b\" 2]) -- extra values ignored"
    (is (= (keywordize ["a" 1 "b" 2])
           [:a 1]))))

(deftest c-002-test
  (testing "(keywordize [\"a\" 3 2]) -- extra values ignored"
    (is (= (keywordize ["a" 3 2])
           [:a 3]))))

(deftest d-test
  (testing "dynamic binding and let shadowing"
    (is (= v
           4))))

(deftest e-001-test
  (testing "count on vectors"
    (is (count [19 "yellow" true])
        3)))

(deftest e-002-test
  (testing "reverse a vector"
    (is (= (reverse [2 4 7])
           [7 4 2]))))

(deftest e-003-test
  (testing "map over multiple collections of differing lengths"
    (is (= (map +
                [2 4 7]
                [5 6]
                [1 2 3 4])
           [8 12]))))

(defn parting
  "returns a String containing a parting salutation, an anti-greeting
  if you will, and I am making this documentation extra long so that I
  can test C-c M-q for formatting the documentation."
  ; we don't have an attr-map
  [name] ; these are the params
  ; we don't have a prepost-map
  (str "Goodbye, " name)
  )

(deftest f-test
  (testing "test the parting function"
    (is (= (parting "Mark")
           "Goodbye, Mark"))))

(deftest g-test
  (testing "testing multiary power function"
    (is (= (power 2 3 4)
           4096.0)))) ; definitely NOT 4096 the integer

