(ns milkshakes.core
  (:require clojure.set)
  (:use [clojure.java.io :as cjio])
  (:use [clojure.string  :as cstr :only [split split-lines]])
  (:use [clojure.pprint  :as pp   :only [pprint]])
  (:gen-class))

(defn get-current-directory []
  (. (java.io.File. ".") getCanonicalPath))

(defn do-per-line [f]
  (with-open [rdr (cjio/reader "input.txt")]
    (doseq [line (line-seq rdr)]
      (f line))))

(defmacro dbg [x]
  `(let [x# ~x]
     (do (println '~x "~~>" x#)
         x#))) 

;;; and pretty-printing version

(defmacro ppdbg [x]
  `(let [x# ~x]
     (do (println "----------------")
         (clojure.pprint/pprint '~x)
         (println "~~>")
         (clojure.pprint/pprint x#)
         (println "----------------")
         x#)))

(defn parse-cases [acc ls]
  (let [flavors          (read-string (first ls))
        c                (read-string (fnext ls))
        raw-likes        (take c (drop 2 ls))
        nested-likes     (map #(cstr/split % #"\s")     raw-likes)
        nested-int-likes (map #(map read-string %)      nested-likes)
        likes            (map #(partition 2 (drop 1 %)) nested-int-likes)
        rems             (drop (+ 2 c) ls)
        ]
    (let [ans (conj acc {:flavors flavors :likes (vec likes)})]
      (if (not= '() rems)
        (recur ans rems)
        ans))))

(defn parse-lines [ls]
  (let [ncases (read-string (first ls))]
    (dbg ncases)
    (parse-cases [] (rest ls))
    ))

(defn case->soln [a]
  (let [N      (:flavors a)
        ;; In the batches, 1 means malted and 0 means unmalted.
        bs     (byte-array N)
        ;; A batch is impossible if the only way to satisfy customers
        ;; is to have some flavor both malted and unmalted. As we're
        ;; processing likes
        imposs (byte-array N)
        likes  (:likes a)
        ;; In the likes, each like is a list of a flavor index and a
        ;; malted-or-not.  
        mikes  (map #(map vec %) likes)
        nikes  (map #(map (fn [[flav malt]]
                            {:flav flav, :malt malt})
                          %)
                    mikes)
        ]
    (aset-byte bs 0 1)
    (dbg nikes)
    (vec bs)
    ))

(defn -main
  "Basic husk for programming problems."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println (get-current-directory))

  (let [input (slurp
               ;"/Users/rebcabin/Downloads/C-large-practice.in"
               "input.txt"
               )
        _     (spit "input.txt" input)
        lines (cstr/split-lines input)
        answs (map-indexed
               (fn [i l]
                 (str "Case #" (inc i) ": "
                      (identity l)
                      "\n")
                 )
               (let [parsed 
                     (parse-lines lines)]
                 (map case->soln parsed))
               )
        ]
    (with-open [w (cjio/writer "output.txt")]
      (doseq [line answs] (.write w line)))
    ))

