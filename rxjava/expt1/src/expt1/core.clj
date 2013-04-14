(ns expt1.core
  (:gen-class)
  ;;(:require [expt1.k2 :as k2])
  (:import [rx Observable Observer Subscription]
           [rx.util AtomicObservableSubscription])
  )

; --------------------------------------------------
; Create Observable from Existing Data
; --------------------------------------------------

(defn existingDataFromNumbers []
  (Observable/toObservable [1 2 3 4 5 6]))

(defn existingDataFromNumbersUsingFrom []
  (Observable/from [1 2 3 4 5 6]))

(defn existingDataFromObjects []
  (Observable/toObservable ["a" "b" "c"]))

(defn existingDataFromObjectsUsingFrom []
  (Observable/from ["a" "b" "c"]))

(defn existingDataFromList []
  (let [list [5, 6, 7, 8]]
    (Observable/toObservable list)))

(defn existingDataFromListUsingFrom []
  (let [list [5, 6, 7, 8]]
    (Observable/from list)))

(defn existingDataWithJust []
  (Observable/just "one object"))

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
   (existingDataFromNumbers)
   (Observable/filter (fn [x] (= 0 (mod x 2))))
   (.subscribe println))

  ;;(Thread/sleep 200)
  (println "Hello, World!"))
