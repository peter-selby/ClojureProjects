(ns side-files.core
  (:require side-files.side-file)
  (:gen-class))

(defn f [x] (+ x 1))

(defn -main
  "I can call functions defined in the core file, and I can call functions defined in the side-file."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println (f 41))
  (println (g 42))
  (println (h 41))
  )
