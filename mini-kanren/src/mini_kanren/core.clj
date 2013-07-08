(ns mini-kanren.core
  (:refer-clojure :exclude [==])
  (:require [clojure.test :as test])
  #_(:gen-class)
  (:use [clojure.core.logic]))

(defmacro pdump [x]
  `(let [x# ~x]
     (do (println "----------------")
         (clojure.pprint/pprint '~x)
         (println "~~>")
         (clojure.pprint/pprint x#)
         (println "----------------")
         x#)))

;;;   ___ _              _             _ 
;;;  / __| |_  __ _ _ __| |_ ___ _ _  / |
;;; | (__| ' \/ _` | '_ \  _/ -_) '_| | |
;;;  \___|_||_\__,_| .__/\__\___|_|   |_|
;;;                |_|                   



(defn teacup [x] (conde
                  ((== 'tea x) s#)
                  ((== 'cup x) s#)
                  (s#          u#)))

(defn teacup2 [x] (conde
                   ((== :tea x) s#)
                   ((== :cup x) s#)
                   (s#          u#)))

(test/deftest foo-test-01-2
  (test/is (= '(tea cup) (run* [x] (teacup x))))

  ;; This next one produces its values in the reverse order predicted
  ;; by the book and by common sense. The top-level conde produces the
  ;; [false true] results (marked A) before producing the nested
  ;; teacup2 results (marked B), no matter the order of A and B in the
  ;; tope-level conde. The wiki explains that clojure.logic's "conde"
  ;; is really the book's "condi", and the order of results is not
  ;; predictable. Clojure.logic does not offer an equivalent to the
  ;; book's "conde".
  
  (test/is (= [[false true] [:tea true] [:cup true]]
              (run* [r]
                    (fresh [x y]
                           (conde
                            ((teacup2 x)  (== true y) s#) ; B
                            ((== false x) (== true y) s#) ; A
                            (s#                       u#))
                           (== [x y] r)))))

  (test/is (= [[false true] [:tea true] [:cup true]]
              (run* [r]
                    (fresh [x y]
                           (conde
                            ((== false x) (== true y) s#) ; A
                            ((teacup2 x)  (== true y) s#) ; B
                            (s#                       u#))
                           (== [x y] r)))))

  ;; We can see this "condi-like" behavior in a simpler case that
  ;; elides the 'fresh':
  (test/is (= [:the-end :tea :cup])
           (run* [x] (conde
                      ((teacup2 x) s#)
                      ((== :the-end x) s#)
                      )))
  
  (test/is (= [:the-end :tea :cup])
           (run* [x] (conde
                      ((== :the-end x) s#)
                      ((teacup2 x) s#)
                      )))

  ;; But, if the last test is a fail, orders are preserved:
  (test/is (= [:tea :cup])
           (run* [x] (conde
                      ((teacup2 x) s#)
                      ((== :the-end x) u#)
                      )))
  
  (test/is (= [:the-end])
           (run* [x] (conde
                      ((== :the-end x) s#)
                      ((teacup2 x) u#)
                      )))

  (test/is (= '([_0 _1] [_0 _1])
              (run* [r]
                    (fresh [x y z]
                           (conde
                            ((== x y) (fresh [x] (== z x)))
                            ((fresh [x] (== y x)) (== z x))
                            (s# u#))
                           (== [y z] r)))))

  (test/is (= '([false _0] [_0 false])
              (run* [r]
                    (fresh [x y z]
                           (conde
                            ((== x y) (fresh [x] (== z x)))
                            ((fresh [x] (== y x)) (== z x))
                            (s# u#))
                           (== [y z] r)
                           (== x false)))))

  (test/is (= [false]
              (run* [q]
                    (let [a (== true q)
                          b (== false q)]
                      b))))

  ;; The folloowing two expressions investigate stopping conditions
  ;; for conde.
  (test/is (= '(true false)
              (run* [q]
                    (conde
                     ((== true q) s#)
                     (s# (== false q))))))
  
  ;; Last clause in a conde gets a default u#:
  (test/is (= '(true false)
              (run* [q]
                    (conde
                     ((== true q) s#)
                     ((== false q) s#)))))

  (test/is (= [false]
              (run* [q]
                    (let [a (== true q) ; This never runs.
                          b (fresh [x]
                                   (== x q)
                                   (== false x))
                          c (conde
                             ((== true q) s#)
                             ((== 42 q))) ; This never runs.
                          ]
                      b))))
  )

;;;   ___ _              _             ___ 
;;;  / __| |_  __ _ _ __| |_ ___ _ _  |_  )
;;; | (__| ' \/ _` | '_ \  _/ -_) '_|  / / 
;;;  \___|_||_\__,_| .__/\__\___|_|   /___|
;;;                |_|

(test/deftest foo-test-02-1
  
  (test/is (= 'c (let [x (fn [a] a)
                       y 'c]
                   (x y))))

  ;; Regular lists, but not quoted when they contain logic variables,
  ;; work in goals:
  (test/is (= '((_0 _1)) (run* [q] (fresh [x y] (== (list x y) q)))))

  ;; Lcons can deliver the values of variables:
  (test/is (= '((_0 _1)) (run* [q] (fresh [x y] (== (lcons x (lcons y ())) q)))))  

  ;; Vectors can get the values of variables out of a goal:
  (test/is (= ['(_0 _1)] (run* [q] (fresh [x y] (== [x y] q)))))

  (test/is (= ['(_0 _1)] (run* [q] (fresh [x y]
                                          (let [u x, v y]
                                            (== [u v] q))))))

  ;; "firsto" works on lists, lcons-lists, and vectors
  (test/is (= '(a)
             (run* [r]
               (firsto (lcons 'a (lcons 'c (lcons 'o (lcons 'r (lcons 'n ()))))) r))))

  (test/is (= '(a) (run* [r] (firsto '(a c o r n) r))))

  (test/is (= '(a) (run* [r] (firsto ['a 'c 'o 'r 'n] r))))

  (test/is (= '(true) (run* [r] (firsto '(a c o r n) 'a) (== true r))))

  ;; No solution to the next one; it fails:
  (test/is (= '() (run* [r] (firsto '(a c o r n) 'z) (== true r))))

  ;; You don't need to use lcons if you're doing internal associations
  (test/is (= '((pear pear _0))
              (run* [r x y]
                    (firsto
                     (lcons r (lcons y ()))
                     x)
                    (== 'pear x))))

  (test/is (= '((pear pear _0))
              (run* [r x y]
                    (firsto
                     (list r y)
                     x)
                    (== 'pear x))))

  (test/is (= '((pear pear _0))
              (run* [r x y]
                    (firsto
                     [r y]
                     x)
                    (== 'pear x))))

  ;; Back to regular lisp for a trice:
  (test/is (= '(grape a) (cons (first '(grape raisin pear))
                               (first '((a) (b) (c))))))

  (test/is (= '((grape a))
              (run* [r]
                    (fresh [x y]
                           (firsto '(grape raisin pear) x)
                           (firsto '((a) (b) (c))       y)
                           ;; LCONS can do improper pairs; that's
                           ;; apparently its main reason for existence:
                           (== r (lcons x y))))))
  
  (test/is (= '(c)
              (run* [r]
                    (fresh [v]
                           (resto '(a c o r n) v)
                           (firsto v r)))))

  (test/is (= '(((raisin pear) a))
              (run* [r]
                    (fresh [x y]
                           (resto '(grape raisin pear) x)
                           (firsto '((a) (b) (c))      y)
                           (== (lcons x y) r)))))

  (test/is (= [true]
              (run* [q]
                    (resto '(a c o r n) '(c o r n))
                    (== true q))))

  ;; works with vectors, lists, and lcons-trains, again:
  (test/is (= '(o)
              (run* [x]
                    (resto '(c o r n)
                           [x 'r 'n]))))

  (test/is (= '(o)
              (run* [x]
                    (resto '(c o r n)
                           (list x 'r 'n)))))

  (test/is (= '(o)
              (run* [x]
                    (resto '(c o r n)
                           (lcons x (lcons 'r (lcons 'n ())))))))

  ;; This does NOT work with llist: NO SOLUTION TODO understand.
  (test/is (= () ;; '(o)
              (run* [x]
                    (resto '(c o r n)
                           (llist x 'r 'n)))))

  (test/is (= '((a c o r n))
              (run* [l]
                    (fresh [x]
                           (resto l '(c o r n))
                           (firsto l x)
                           (== 'a x)))))

  (test/is (= '(((a b c) d e))
              (run* [l] (conso '(a b c) '(d e) l))))

  (test/is (= '(d) (run* [x] (conso x '(a b c) '(d a b c)))))

  (test/is (= '((e a d c))
              (run* [r]
                    (fresh [x y z]
                           (== (list 'e 'a 'd x) r)
                           (conso y (list 'a z 'c) r)))))

  (test/is (= '(d)
              (run* [x]
                    (conso x ['a x 'c] ['d 'a x 'c]))))

  (test/is (= (run* [l]
                    (fresh [x]
                           (== ['d 'a x 'c] l)
                           (conso x ['a x 'c] l)))
              '((d a d c))))

  (test/is (= (run* [l]
                    (fresh [d x y w s]
                           (conso w ['a 'n 's] s)
                           (resto l s)
                           (firsto l x)
                           (== 'b x)
                           (resto l d)
                           (firsto d y)
                           (== 'e y)))
              '((b e a n s))))

  (test/is (= '(_0) (run* [x] (emptyo ()))))

  ;; No solution:
  (test/is (= ()) (run* [x] (emptyo '(grape raisin pear))))

  ;; Solution reaches (== true q) because the empty list is emptyo:
  (test/is (= '(true) (run* [q] (emptyo ()) (== true q))))

  ;; Solution equals the empty list:
  (test/is (= '(()) (run* [x] (emptyo x))))

  (test/is (= 'plum 'plum))

  ;; No solution:
  (test/is (= (run* [q] (== 'pear 'plum) (== 'true q))) '())

  (test/is (= (run* [q] (pairo (lcons 'pear ())) (== 'true q)) '(true)))

  (test/is (= (run* [q] (resto (lcons 'pear ()) q)) '(())))

  (test/is (= (run* [x] (pairo x))

              ;; The notation '(_0 . _1) ends up not being equal to
              ;; the result of (pairo x), even though they look
              ;; exactly the same.

              (run* [q] (fresh [x y] (== (lcons x y) q))))))

;;;   ___ _              _             ____
;;;  / __| |_  __ _ _ __| |_ ___ _ _  |__ /
;;; | (__| ' \/ _` | '_ \  _/ -_) '_|  |_ \
;;;  \___|_||_\__,_| .__/\__\___|_|   |___/
;;;                |_|

(test/deftest foo-test-03-1
  (test/is (= '(true) (run* [q] (== (llist 'a 'b) (lcons 'a 'b)) (== q 'true))))

  (test/is (= '(true) (run* [q] (listo ()) (== q 'true))))

  (test/is (= '([true _0]) (run* [q x] (listo '(a b x d)) (== q 'true))))

  (test/is (= '([true _0]) (run* [q x] (listo (llist 'a 'b x 'd ())) (== q 'true))))

  ;; This next one is not a proper list
  (test/is (= '() (run* [q x] (listo (llist 'a 'b x 'd)) (== q 'true))))

  ;; But it is a pair
  (test/is (= '([true _0]) (run* [q x] (pairo (llist 'a 'b x 'd)) (== q 'true))))

  ;; Don't run* the next one: it finds an infinite number of solutions
  ;; and does not terminate:
  (test/is (= '(() (_0) (_0 _1) (_0 _1 _2) (_0 _1 _2 _3))
              (run 5 [x] (listo (llist 'a 'b 'c x)))))

  ;; One solution: the empty list:
  (test/is (= (run 1 [l] (lolo l)) '(())))

  ;; Frame 22: emptyo always succeeds against a fresh var:
  (test/is (= '(()) (run* [q] (emptyo q))))

  #_(test/is (= '(((a b) (c d))) (run 1 [x] (lolo (llist '(a b) '(c d) x)))))
  )

;;;  __  __      _      
;;; |  \/  |__ _(_)_ _  
;;; | |\/| / _` | | ' \ 
;;; |_|  |_\__,_|_|_||_|
                    


(defn -main []
  (test/run-all-tests #"mini-kanren.core"))

