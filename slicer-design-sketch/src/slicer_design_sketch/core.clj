(ns slicer-design-sketch.core
  (:gen-class))

(defmacro pdump [x]
  `(let [x# ~x]
     (do (println "----------------")
         (clojure.pprint/pprint '~x)
         (println "~~>")
         (clojure.pprint/pprint x#)
         (println "----------------")
         x#)))

(def driver-table
  (pdump (map (partial zipmap [:chan :pl :driver-name :asin :prime :amount])
              [[1000 23 :prod-cogs "B00012345" :p1 5]
               [1000 23 :prod-cogs "B00012346" :p2 15]
               [1000 23 :prod-cogs "B00012347" :p3 10]
               [1100 24 :prod-rev  "B00012348" :p1 19]
               ])))

(def driver-mapping
  "Maps accounts to drivers."
  (pdump (map (partial zipmap [:chan :pl :account :driver-name :homo?])
              [[1000 23 :prod-cogs :prod-cogs true]
               [1000 23 :inv-val   :prod-cogs false]])))

(defn normalize [amaps]
  (let [total (apply + (map :amount amaps))]
    (map
     (fn [amap] (assoc amap :amount (/ (:amount amap) total)))
     amaps)))

(defn driver [driver-name chan pl]
  (let [ripped-table
        (filter (fn [line] (and (= driver-name (:driver-name line))
                               (= chan        (:chan        line))
                               (= pl          (:pl          line))))
                driver-table)
        ]
    ripped-table))

(def pnls
  (pdump (map (partial zipmap [:chan :pl :account :amount])
              [[1000 23 :prod-cogs 2500]
               [1600 23 :inv-val   5000]
               ])))

(defn driver-spec [driver-name pnl]
  (normalize (driver driver-name (:chan pnl) (:pl pnl))))

(pdump (driver-spec :prod-cogs (first pnls)))



(defn allocate [pnl driver-spec]
  (map (fn [line]
         (assoc line :amount (* (:amount pnl) (:amount line))))
       driver-spec))

(pdump (allocate (first pnls)
                 (driver-spec :prod-cogs (first pnls))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))
