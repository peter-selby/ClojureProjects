(ns two-files.core
  (:require [two-files.kfile])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))

  (two-files.kfile/foo)
  (println "two-files.core/-main"))
