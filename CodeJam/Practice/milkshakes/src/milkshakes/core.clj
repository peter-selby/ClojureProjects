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

(defn parse-cases [pre-ls]
  (loop [ls pre-ls
         flavors (read-string (first ls))
         c (read-string (fnext ls))
         raw-prefs (take c (drop 2 ls))
         nested-prefs (map #(cstr/split % #"\s") raw-prefs)
         nested-int-prefs (map #(map read-string %) nested-prefs)
         prefs (map #(partition 2 (drop 1 %)) nested-int-prefs)
         rems (drop (+ 2 c) ls)
         ]
    (dbg flavors)
    (dbg c)
    (dbg raw-prefs)
    (dbg (count raw-prefs))
    (dbg (map type raw-prefs))
    (dbg nested-prefs)
    (dbg nested-int-prefs)
    (dbg prefs)
    (dbg rems)
    
    ls)
  )

(defn parse-lines [ls]
  (let [ncases (read-string (first ls))
        acases []
        temp []
        rls (rest ls)]
    (dbg ncases)
    (parse-cases rls)
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
               (parse-lines lines)
               )
        ]
    (with-open [w (cjio/writer "output.txt")]
      (doseq [line answs] (.write w line)))
    ))

