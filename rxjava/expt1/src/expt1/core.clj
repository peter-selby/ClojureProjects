(ns expt1.core
  (:require [expt1.k2 :as k2]
             clojure.string
             clojure.pprint)
  (:refer-clojure :exclude [distinct])
  (:import [rx Observable subscriptions.Subscriptions])
  )

;;; Load this file into LightTable (using the workspace tab) and "make
;;; current editor an instarepl" (using the command tab).

;;; The current rx library has none of the co-monadic operators such
;;; as "first" and "last". Let us make atomic, external collectors for
;;; extracting items from an oseq (observable sequence) by mutating
;;; side-effects (horrors!).

(defn- subscribe-collectors [obl]
  (let [onNextCollector      (atom     [])
        onErrorCollector     (atom    nil)
        onCompletedCollector (promise    )]
    (letfn [(collect-next      [item] (swap!   onNextCollector conj item))
            (collect-error     [excp] (reset!  onErrorCollector     excp))
            (collect-completed [    ] (deliver onCompletedCollector true))
            (report-collectors [    ]
              {:onNext      @onNextCollector
               :onError     @onErrorCollector
               :onCompleted (deref onCompletedCollector 1000 false)})]
      (-> obl
          (.subscribe collect-next collect-error collect-completed))
      (report-collectors)
      )))


;;;  ___ _        _      _   _
;;; / __| |_  _ _(_)_ _ | |_(_)_ _  __ _
;;; \__ \ ' \| '_| | ' \| / / | ' \/ _` |
;;; |___/_||_|_| |_|_||_|_\_\_|_||_\__, |
;;;                                |___/


;;; First, let's just take the first two numbers out of a vector of
;;; numbers and turn them into oseq. This illustrates "take", a method
;;; that often shortens sequences.

(->
 (Observable/toObservable [1 2 3])
 (.take 2)
 (subscribe-collectors)
 )

;;;   ___                _
;;;  / __|_ _ _____ __ _(_)_ _  __ _
;;; | (_ | '_/ _ \ V  V / | ' \/ _` |
;;;  \___|_| \___/\_/\_/|_|_||_\__, |
;;;                            |___/


;;; Now, let's transform each number x into a vector of numbers, adding
;;; x to some familiar constants, then flattening the results exactly
;;; one time. This is the way to grow a shorter sequence into a longer
;;; one. Filters typically shorten sequences; maps leave sequences the
;;; same length. All methods that lengthen sequences rely on mapMany,
;;; which is called "SelectMany" in many Rx documents (.e.g.,
;;; http://bit.ly/18Bot23).

(->
 (Observable/toObservable [1 2 3])
 (.take 2)
 (.mapMany
  #(Observable/toObservable (map (partial + %) [42 43 44])))
 (subscribe-collectors)
 )

;;; Let's operate on strings.

(->
 (Observable/toObservable ["one" "two" "three"])
 (.take 2)
 (subscribe-collectors)
 )

;;; "seq" explodes strings into lazy sequences of characters:

(seq "one")

;;; Just for self-documenting code, define an alias

(def string-explode seq)

(->
 (Observable/toObservable ["one" "two" "three"])
 (.mapMany #(Observable/toObservable (string-explode %)))
 (subscribe-collectors)
 )

;;;   __
;;;  / _|_ _ ___ _ __ ___ ___ ___ __ _
;;; |  _| '_/ _ \ '  \___(_-</ -_) _` |
;;; |_| |_| \___/_|_|_|  /__/\___\__, |
;;;                                 |_|


;;; We'd like to clean up the ugly #(Observable/toObservable ...) into
;;; a composition, but we can't (comp Observable/toObservable ...) since
;;; it's a Java method and does not implement Clojure IFn. We fix this
;;; by wrapping it in a function:

(defn from-seq [s] (Observable/toObservable s))

(->
 (from-seq ["one" "two" "three"])
 (.mapMany (comp from-seq string-explode))
 (subscribe-collectors)
 )

;;;          _
;;;  _ _ ___| |_ _  _ _ _ _ _
;;; | '_/ -_)  _| || | '_| ' \
;;; |_| \___|\__|\_,_|_| |_||_|


;;; We notice that the monadic "return" is missing from "rxjava 0.9.0",
;;; so we add it as follows. This is doing some junk-work -- puts the
;;; item in a vector just so we can take it out again into an obl.
;;; A native implementation would be preferable.

(defn return [item] (from-seq [item]))

(->
 (from-seq ["one" "two" "three"])
 (.mapMany (comp from-seq string-explode))
 (.mapMany return)
 (subscribe-collectors)
 )


;;;     _ _    _   _         _
;;;  __| (_)__| |_(_)_ _  __| |_
;;; / _` | (_-<  _| | ' \/ _|  _|
;;; \__,_|_/__/\__|_|_||_\__|\__|


;;; Rx is supposed to have a couple of operators: "disinct" and
;;; "distinctUntilChanged", but RxJava 0.9.0 doesn't seem to
;;; have them yet. We can fake them as follows:

(->
 (Observable/toObservable ["one" "two" "three"])
 (.mapMany (comp from-seq string-explode))

 ;; The following two effect an implementation of "distinct".
 (.reduce #{} conj)
 ;; We now have a singleton obl containing a set of unique characters.
 ;; To promote this back into an obl of chars, we do:
 (.mapMany from-seq)
 ;; This is ok because "distinct" simply MUST consume the entire oseq
 ;; before producing its values. The operator "distinct" simply won't
 ;; work on a non-finite oseq.

 (subscribe-collectors)
 )

;;; Package and test.

(defn distinct [oseq]
  (-> oseq
      (.reduce #{} conj)
      (.mapMany from-seq)))

(->
 (Observable/toObservable ["one" "two" "three"])
 (.mapMany (comp from-seq string-explode))
 (distinct)
 (subscribe-collectors)
 )

;;;     _ _    _   _         _
;;;  __| (_)__| |_(_)_ _  __| |_
;;; / _` | (_-<  _| | ' \/ _|  _|
;;; \__,_|_/__/\__|_|_||_\__|\__|
;;;      _   _     _   _ _  ___ _                          _
;;;     | | | |_ _| |_(_) |/ __| |_  __ _ _ _  __ _ ___ __| |
;;;     | |_| | ' \  _| | | (__| ' \/ _` | ' \/ _` / -_) _` |
;;;      \___/|_||_\__|_|_|\___|_||_\__,_|_||_\__, \___\__,_|
;;;                                           |___/


;;; The following solution is correct but unacceptable because it consumes
;;; the entire source oseq before producing values. Such is not necessary
;;; with distinct-until-changed: we only need to remember one back. Still,
;;; to make the point:

(->
 (Observable/toObservable ["onnnnne" "tttwo" "thhrrrrree"])
 (.mapMany (comp from-seq string-explode))

 ;; The following two effect "distinctUntilChanged".
 (.reduce [] (fn [acc x]
               (let [l (last acc)]
                 (if (and l (= x l)) ; accounts for legit nils
                   acc
                   (conj acc x)))))
 ;; We now have a singleton obl containing representatives of runs of non-
 ;; distinct characters. Slurp it back into the monad:
 (.mapMany from-seq)

 (subscribe-collectors)
 )

;;; Better is to keep a mutable buffer of length one. It could be an atom
;;; if we had the opposite of "compare-and-set!"; an atomic primitive that
;;; sets the value only if it's NOT equal to its current value. "compare-and
;;; set!" sets the atom to a newval if its current value is equal to an
;;; oldval. It's easy enough to get the desired semantics with a Ref and
;;; software-transactional memory, the only wrinkle being that the container
;;; must be defined outside the mapMany and the function that mapMany applies.
;;; However, this solution will not materialize the entire input sequence.

(let [exploded (->
                (Observable/toObservable ["onnnnne" "tttwo" "thhrrrrree"])
                (.mapMany (comp from-seq string-explode))
                )
      ;; Must define this container outside the mapMany and the function
      ;; that napMany applies.
      last-container (ref [])]
  (-> exploded
      (.mapMany (fn [x]
                  (dosync
                   (let [l (last @last-container)]
                     (if (and l (= x l))
                       (Observable/empty)
                       (do
                         (ref-set last-container [x])
                         (return x)))))))
      (subscribe-collectors)))

;;; Package and test:

(defn distinct-until-changed [oseq]
  (let [last-container (ref [])]
    (-> oseq
        (.mapMany (fn [x]
                    (dosync
                     (let [l (last @last-container)]
                       (if (and l (= x l))
                         (Observable/empty)
                         (do
                           (ref-set last-container [x])
                           (return x))))))))))

(->
  (Observable/toObservable ["onnnnne" "tttwo" "thhrrrrree"])
  (.mapMany (comp from-seq string-explode))
  (distinct-until-changed)
  (subscribe-collectors)
)

;;; It's well-behaved on an empty input:

(->
  (Observable/toObservable [])
  (.mapMany (comp from-seq string-explode))
  (distinct-until-changed)
  (subscribe-collectors)
)

;;;   ___  _   _              ___                     _
;;;  / _ \| |_| |_  ___ _ _  | __|_ ____ _ _ __  _ __| |___ ___
;;; | (_) |  _| ' \/ -_) '_| | _|\ \ / _` | '  \| '_ \ / -_|_-<
;;;  \___/ \__|_||_\___|_|   |___/_\_\__,_|_|_|_| .__/_\___/__/
;;;                                             |_|

(defn flip [f2] (fn [x y] (f2 y x)))
(-> (k2/customObservableBlocking)
    (.map (partial (flip clojure.string/split) #"_"))
    (.map (fn [[a b]] [a (Integer/parseInt b)]))
    (.filter (fn [[a b]] (= 0 (mod b 7))))
    (subscribe-collectors))

(-> (k2/customObservableNonBlocking)
    #_(.map (partial (flip clojure.string/split) #"_"))
    #_(.map (fn [[a b]] [a (Integer/parseInt b)]))
    #_(.filter (fn [[a b]] (= 0 (mod b 7))))
    (subscribe-collectors))

(defn -main
  [& args]

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
