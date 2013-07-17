(ns problem-106.core
  (:gen-class))

;;;  ___         _    _             _  __   __ 
;;; | _ \_ _ ___| |__| |___ _ __   / |/  \ / / 
;;; |  _/ '_/ _ \ '_ \ / -_) '  \  | | () / _ \
;;; |_| |_| \___/_.__/_\___|_|_|_| |_|\__/\___/
                                           

(def __ (letfn [(d [n] (* n 2)) ; double
                (h [n] (/ n 2)) ; halve
                (a [n] (+ n 2)) ; add
                (hop-count [n t m hops-so-far]
                  (cond
                   (= n t)            hops-so-far ; end search
                   (>= hops-so-far m) hops-so-far ; limit depth 
                   :else
                   (let [nh (inc hops-so-far) ; take a hop
                         dh (hop-count (d n) t m nh)
                         ah (hop-count (a n) t m nh)
                         hh (when (even? n) (hop-count (h n) t m nh))]
                     (cond
                      (odd? n)  (min dh ah)
                      (even? n) (min dh ah hh)))))] 
          (fn [s t] (hop-count s t 10 1))))

(def __ #((fn r [n s]
            (if ((set s) %2) n
                (r (+ n 1)
                   (for [f [+ * /] e s] (f e 2)))))
          1 [%]))

(def __ (letfn [(d [n] (* n 2)) ; double
                (h [n] (/ n 2)) ; halve
                (a [n] (+ n 2)) ; add
                (hop-count [n ts hops]
                  (cond
                   (ts n)      hops ; end search
                   :else
                   (hop-count n (set (concat ts (map d ts) (map a ts) (map h ts))) (inc hops))))] 
          (fn [s t] (hop-count t #{s} 1))))

(println (map #(apply __ %) [[1 1] [3 12] [12 3] [5 9] [9 2] [9 12]]))

(defn -main [& args]
  (alter-var-root #'*read-eval* (constantly false)))
