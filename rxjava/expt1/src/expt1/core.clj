(ns expt1.core
  (:require [expt1.k2 :as k2])
  (:import [rx Observable Observer Subscription]
           [rx.util AtomicObservableSubscription])
  (:gen-class)
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (->
   (Observable/toObservable ["one" "two" "three"])
   (.take 2)
   (.subscribe println))

  (->
   (k2/existingDataFromNumbers)
   (Observable/filter (fn [x] (= 0 (mod x 2))))
   (.subscribe println))

  )
