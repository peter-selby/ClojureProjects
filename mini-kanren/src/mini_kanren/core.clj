(ns mini-kanren.core
  (:refer-clojure :exclude [==])
  (:require [clojure.test :as test])
  (:gen-class)
  (:use [clojure.core.logic]
        ))

(defmacro pdump [x]
  `(let [x# ~x]
     (do (println "----------------")
         (clojure.pprint/pprint '~x)
         (println "~~>")
         (clojure.pprint/pprint x#)
         (println "----------------")
         x#)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))

(test/deftest foo-test
  (test/is (= '(true)  (run* [q] (== q true))))
  (test/is (= '(true)  (run* [q] s# (== true q))))
  (test/is (= '()      (run* [q] u# (== true q))))
  (test/is (= '(corn)  (run* [q] s# (== 'corn q))))
  (test/is (= '(false) (run* [q] s# (== false q))))
  (test/is (= '()      (run* [q] (== false 'x))))
  (test/is (= '()      (run* [x] (let [x true] (== false x)))))
  (test/is (= '(_0)    (run* [x] (let [x true] (== true x)))))
  (test/is (= '(true)  (run* [q] (fresh [x] (== true x) (== true q)))))
  (test/is (= '(true)  (run* [q] (fresh [x] (== false x) (== true q)))))
  (test/is (= '(_0)    (run* [x] s#)))
  (test/is (= '(_0)    (run* [x])))
  (test/is (= '(_0)    (run* [x] (let [x false] (fresh [x] (== true x))))))
  (test/is (= '((_0 _1))
              (run* [r] (fresh [x y] (== (lcons x (lcons y ())) r)))))
  (test/is (= '((_0 _1 _0))
              (run* [r] (fresh [x y] (== (lcons x (lcons y (lcons x ()))) r)))))
  ;; Variables are reified "late:" in the order in which they appear
  ;; in "ouput-generating" forms when unifying against variables.
  ;; Here, y gets reified first, then x. They're not reified in the
  ;; order they appear in the 'fresh' declaration.
  (test/is (= '((_0 _1 _0))
              (run* [r] (fresh [x y] (== (lcons y (lcons x (lcons y ()))) r)))))

  (test/is (= '(false)
              (run* [r] (== false r) (== r false))))

  (test/is (= '(true)
              (run* [r] (let [x r] (== true r)))))

  (test/is (= '(_0)
              (run* [r] (fresh [x] (== x r)))))

  (test/is (= '(true)
              (run* [r] (fresh [x] (== true x) (== x r)))))

  (test/is (= '(true)
              (run* [q] (fresh [x] (== x q) (== true q)))))

  (test/is (= '(true)
              (run* [q] (fresh [x] (== x q) (== true x)))))

  (test/is (= '(false) (run* [q] (fresh [x] (== (= x q) q)))))
  (test/is (= '(true)  (run* [q] (fresh [x] (== (= x x) q)))))
  (test/is (= '(true)  (run* [q] (fresh [x] (== (= q q) q)))))

  (test/is (= '(false) (run* [q] (let [x q] (fresh [q] (== x (= x q)))))))
  (test/is (= '(_0)    (run* [q] (let [x q] (fresh [q] (== q (= x q)))))))
  
  (test/is (= 2 (cond false 1 true 2)))
  )

