(ns mini-kanren.core
  (:refer-clojure :exclude [==])
  (:gen-class)
  (:use [clojure.core.logic]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!")
  
  (println (run* [q] (== q true)))
)

(-main)
