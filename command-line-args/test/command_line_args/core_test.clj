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
    (is (= 1 1))))
