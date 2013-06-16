(ns palindromics.core)

(def i 1881)
(odd? 42)

(letfn [(gary   [n]  (cons n (lazy-seq (gary (*' 10 n)))))
        (fran   [ds] (apply +' (map *' ds (gary 1))))
        (heads  [n]  (take-while pos? (map (partial quot n) (gary 1))))
        (digits [n]  (map #(mod % 10) (heads n)))
        (next   [n]  (let [rigits (reverse (digits n))
                           kount  (count rigits)
                           k2     (quot kount 2)
                           fron   (take k2 rigits)
                           lfron  (last fron)
                           nlfron (when lfron (inc lfron))
                           bfron  (butlast fron)
                           oscar  (odd? kount)
                           mid    (when oscar (nth rigits k2))
                           nmid   (when mid (inc mid))
                           ]
                       (if oscar
                         ;; odd-length
                         (if nmid   1 0)
                         ;; even-length
                         (if nlfron 
                           (if (< nlfron 9)
                             (let [bob (concat bfron [nlfron])]
                               (fran (concat bob (reverse bob))))
                             (let [carol (inc (fran fron))
                                   dave  (digits carol)]
                               (fran (concat (reverse dave) dave))
                               ))
                           (throw (Exception.) "impossible")
                           )
                         )
                       )
          )
        ]
  (next i)
  )



