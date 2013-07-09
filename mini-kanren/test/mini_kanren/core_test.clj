(ns mini-kanren.core-test
  (:refer-clojure :exclude [==])
  (:require [clojure.test :as test]
            [mini-kanren.core :refer :all])
  (:use [clojure.core.logic]))

;;;   ___ _              _             _ 
;;;  / __| |_  __ _ _ __| |_ ___ _ _  / |
;;; | (__| ' \/ _` | '_ \  _/ -_) '_| | |
;;;  \___|_||_\__,_| .__/\__\___|_|   |_|
;;;                |_|                   

(test/deftest foo-test-01-1
  (test/is (= '(true)  (run* [q] (== q true))))
  (test/is (= '(true)  (run* [q] s# (== true q))))
  (test/is (= '()      (run* [q] u# (== true q))))
  (test/is (= '(corn)  (run* [q] s# (== 'corn q))))
  (test/is (= '(false) (run* [q] s# (== false q))))
  (test/is (= '()      (run* [q] (== false 'x))))
  (test/is (= '()      (run* [x] (let [x true] (== false x)))))
  (test/is (= '(_0)    (run* [x] (let [x true] (== true x)))))
  (test/is (= '(true)  (run* [q] (fresh [x] (== true x) (== true q)))))
  (test/is (= '(true)  (run* [q] (fresh [x] (== false x) (== true q)))))
  (test/is (= '(_0)    (run* [x] s#)))
  (test/is (= '(_0)    (run* [x])))
  (test/is (= '(_0)    (run* [x] (let [x false] (fresh [x] (== true x))))))
  (test/is (= '((_0 _1))
              (run* [r] (fresh [x y] (== (lcons x (lcons y ())) r)))))
  (test/is (= '((_0 _1 _0))
              (run* [r] (fresh [x y] (== (lcons x (lcons y (lcons x ()))) r)))))

  ;; Variables are reified "late:" in the order in which they appear
  ;; in "ouput-generating" forms when unifying against variables.
  ;; Here, y gets reified first, then x. They're not reified in the
  ;; order they appear in the 'fresh' declaration.
  (test/is (= '((_0 _1 _0))
              (run* [r] (fresh [x y] (== (lcons y (lcons x (lcons y ()))) r)))))

  (test/is (= '(false)
              (run* [r] (== false r) (== r false))))

  (test/is (= '(true)
              (run* [r] (let [x r] (== true r)))))

  (test/is (= '(_0)
              (run* [r] (fresh [x] (== x r)))))

  (test/is (= '(true)
              (run* [r] (fresh [x] (== true x) (== x r)))))

  (test/is (= '(true)
              (run* [q] (fresh [x] (== x q) (== true q)))))

  (test/is (= '(true)
              (run* [q] (fresh [x] (== x q) (== true x)))))

  (test/is (= '(false) (run* [q] (fresh [x] (== (= x q) q)))))
  (test/is (= '(true)  (run* [q] (fresh [x] (== (= x x) q)))))
  (test/is (= '(true)  (run* [q] (fresh [x] (== (= q q) q)))))

  (test/is (= '(false) (run* [q] (let [x q] (fresh [q] (== x (= x q)))))))
  (test/is (= '(_0)    (run* [q] (let [x q] (fresh [q] (== q (= x q)))))))
  
  (test/is (= 2 (cond false 1 true 2)))

  (test/is (= '(olive oil)
              (run* [q] (conde
                         ((== 'olive q) s#)
                         ((== 'oil   q) s#)
                         (s#            u#)))))

  (test/is (= '(olive)
              (run 1 [q] (conde
                          ((== 'olive q) s#)
                          ((== 'oil   q) s#)
                          (s#            u#)))))
  
  (test/is (= '((split pea))
              (run* [r]
                    (fresh [x y]
                           (== 'split x)
                           (== 'pea   y)
                           (== (lcons x (lcons y ())) r)))))

  (test/is (= '((split pea) (navy bean))
              (run* [r]
                    (fresh [x y]
                           (conde
                            ((== 'split x) (== 'pea   y))
                            ((== 'navy  x) (== 'bean  y))
                            (s#            u#))
                           (== (lcons x (lcons y ())) r)))))
  
  (test/is (= [[:split :pea] [:navy :bean]]
              (run* [r]
                    (fresh [x y]
                           (conde
                            ((== :split x) (== :pea  y))
                            ((== :navy  x) (== :bean y))
                            (s#            u#))
                           (== [x y] r)))))
  
  (test/is (= [[:split :pea :soup] [:navy :bean :soup]]
              (run* [r]
                    (fresh [x y]
                           (conde
                            ((== :split x) (== :pea  y))
                            ((== :navy  x) (== :bean y))
                            (s#            u#))
                           (== [x y :soup] r))))))

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

  (test/is (=
            '(())
            (run 1 [x] (lolo (llist '(a b) '(c d) x)))))

  ;; Clojure.logic's conde really produces interleaved results, so it
  ;; gets around to solutions that TRS's conde never finds. See
  ;; http://bit.ly/1a8QmPJ .
  (test/is
   (= '(()
        (())
        ((_0))
        (() ())
        ((_0 _1))
        (() (_0))
        ((_0) ())
        (() () ())
        ((_0 _1 _2)))
      (run 9 [x] (lolo (llist '(a b) '(c d) x)))))

  ;; Checking an equivalence between conso and llist:
  (test/is
   (= (run 1 [q] (== q (llist 1 2)))
      (run 1 [q] (conso 1 2 q))))

  ;; Frame 3-32
  (test/is
   (= '(true)
      (run* [q]
            (twinso '(tofu tofu))
            (== true q))))

  (test/is
   (= '(true)
      (run* [q]
            (twinso (list 'tofu 'tofu))
            (== true q))))

  (test/is
   (= '(true)
      (run* [q]
            (twinso (llist 'tofu 'tofu ()))
            (== true q))))


  (test/is
   (= '(true)
      (run* [q]
            (twinso ['tofu 'tofu])
            (== true q))))
  
  ;; Frame 3-33
  (test/is
   (= '(tofu)
      (run* [q]
            (twinso (list q 'tofu)))))

  (test/is
   (= '(tofu)
      (run* [q]
            (twinso (list 'tofu q)))))

  ;; Frame 3-37
  (test/is
   (= '(())
      (run 1 [z] (loto (llist '(g g) z)))))

  ;; Frame 3-42 
  (test/is
   (= '(()
        ((_0 _0))
        ((_0 _0) (_1 _1))
        ((_0 _0) (_1 _1) (_2 _2))
        ((_0 _0) (_1 _1) (_2 _2) (_3 _3))
        )
      (run 5 [z] (loto (llist '(g g) z)))))

  ;; Frame 3-45
  (test/is
   (= '((e (_0 _0) ())
        (e (_0 _0) ((_1 _1)))
        (e (_0 _0) ((_1 _1) (_2 _2)))
        (e (_0 _0) ((_1 _1) (_2 _2) (_3 _3)))
        (e (_0 _0) ((_1 _1) (_2 _2) (_3 _3) (_4 _4)))
        )
      (run 5 [r]
           (fresh [w x y z]
                  (loto (llist '(g g) (list 'e w) (list x y) z))
                  (== (list w (list x y) z) r)))
      ))

  ;; Frame 3-49
  (test/is
   (= '(((g g) (e e) (_0 _0))
        ((g g) (e e) (_0 _0) (_1 _1))
        ((g g) (e e) (_0 _0) (_1 _1) (_2 _2))
        ((g g) (e e) (_0 _0) (_1 _1) (_2 _2) (_3 _3))
        ((g g) (e e) (_0 _0) (_1 _1) (_2 _2) (_3 _3) (_4 _4))
        )
      (run 5 [out]
           (fresh [w x y z]
                  (== (llist '(g g) (list 'e w) (list x y) z) out)
                  (listofo twinso out)))))

  ;; Frame 3-57
  (test/is
   (= '(true)
      (run* [q]
            (membero 'olive '(virgin olive oil))
            (== true q))))
)
