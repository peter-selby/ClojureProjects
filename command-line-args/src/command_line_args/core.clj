(ns command-line-args.core)

(defn -main
  "I am the canonical ideal."
  [& args]
  (println "Hello, World!"))

(defn parse-args [args]
  (into {} (map (fn [[k v]]
                  [(keyword (.replace k "--" "")) v])
                (partition 2 args))))

(defn keywordize [kvp]
  (let [[k v] kvp]
    [(keyword (.replace k "--" ""))
     v]))

;(println "boo")
;(println (keywordize ["a" 1 "b" 2]))

(def ^:dynamic v 1) ; v is global and dynamic

(defn f1 []
  (println "f1: v: " v))

(defn f2 []
  (println "f2: before let v: " v)
  (let [v 2]
    (println "f2: in let, v: " v)
    (f1))
  (println "f2: after let v: " v))

(defn f3 []
  (println "f3: before binding v: " v)
  (binding [v 3]
    (println "f3: in binding v: " v)
    (f1))
  (println "f3: after binding v: " v))

(defn f4 []
  (def v 4))

;; (println "(= v 1) ~~> " (= v 1))
;; (f2)
;; (f3)
;; (f4)
;; (println "(= v 4) ~~> " (= v 4))

(defn power [base & exponents]
  (reduce #(Math/pow %1 %2) base exponents))