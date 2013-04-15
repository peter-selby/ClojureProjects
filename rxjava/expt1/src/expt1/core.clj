(ns expt1.core
  (:require [expt1.k2 :as k2]
;           [expt1.k1 :as k1]
            )
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

  (.subscribe (k2/customObservableBlocking) println)

  (.subscribe (k2/customObservableNonBlocking) println)

  (->
   (k2/fetchWikipediaArticleAsynchronously ["Tiger" "Elephant"])
   (.subscribe #(println "--- Article ---\n" (subs (:body %) 0 125) "..."))
   )

  (k2/simpleComposition)

  (->
   (k2/getVideoForUser 12345 78965)
   (.subscribe
    (fn [datum] (println "--- Object ---\n" datum))
    (fn [exception] (println "--- Error ---\n" exception))
    (fn [] (println "--- Completed ---\n"))))

  (->
   (k2/fetchWikipediaArticleAsynchronouslyWithErrorHandling ["Tiger" "NonExistentTitle" "Elephant"])
   (.subscribe #(println "--- Article ---\n" (subs (:body %) 0 125) "...")
               #(println "--- Error ---\n" (.getMessage %))
               ))
  )
