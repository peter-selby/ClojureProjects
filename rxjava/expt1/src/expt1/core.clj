;;; Run this file by going to the project directory (the directory
;;; with 'project.clj' in it) and saying 'lein repl'.

(ns expt1.core
  (:require [expt1.k2               :as k2  ]
            [clojure.zip            :as zip ]
            [clojure.xml            :as xml ]
            [net.cgrand.enlive-html :as html]
            [clj-http.client        :as http]
            clojure.string
            clojure.pprint
            )
  (:refer-clojure :exclude [distinct])
  (:import [rx Observable subscriptions.Subscriptions])
  )

(defmacro pdump [x]
  `(let [x# ~x]
     (do (println "----------------")
         (clojure.pprint/pprint '~x)
         (println "~~>")
         (clojure.pprint/pprint x#)
         (println "----------------")
         x#)))

;;;   ___                  _       ___  _
;;;  / __|___ _ _  ___ _ _(_)__   / _ \| |__ ___ ___ _ ___ _____ _ _
;;; | (_ / -_) ' \/ -_) '_| / _| | (_) | '_ (_-</ -_) '_\ V / -_) '_|
;;;  \___\___|_||_\___|_| |_\__|  \___/|_.__/__/\___|_|  \_/\___|_|
;;;

;;; The current rx library has no co-monadic operators such as "first"
;;; and "last". Let us make atomic, external collectors for extracting
;;; items from an oseq (observable sequence) by mutating side-effects
;;; (horrors!).

(defn- subscribe-collectors [obl & optional-wait-time]
  (let [wait-time
        (if optional-wait-time
          (first optional-wait-time)
          1000)
        ;; Keep a sequence of all values sent:
        onNextCollector      (agent    [])
        ;; Only need one value if the observable errors out:
        onErrorCollector     (atom    nil)
        ;; Use a promise for 'completed' so we can wait for it on
        ;; another thread:
        onCompletedCollector (promise    )]
    (letfn [;; When observable sends a value, relay it to our agent:
            (collect-next      [item] (send onNextCollector (fn [state] (conj state item))))
            ;; If observable errors out, just set our exception;
            (collect-error     [excp] (reset!  onErrorCollector     excp))
            ;; When observable completes, deliver on the promise:
            (collect-completed [    ] (deliver onCompletedCollector true))
            ;; In all cases, report out the back end with this:
            (report-collectors [    ]
              (identity ;; pdump
               {;; Wait at most 1 second for the promise to complete;
                ;; if it does not complete, then produce 'false'. We
                ;; must wait on the onCompleted BEFORE waiting on the
                ;; onNext because the agent's await-for in onNext only
                ;; waits for messages sent to the agent from THIS
                ;; thread, and our asynchronous observable may be
                ;; sending messages to the agent from another thread,
                ;; say, a future's thread. The agent's await-for will
                ;; return too quickly, allowing this onCompleted await
                ;; to return, losing some messages. This code depends
                ;; on order-of-evaluation assumptions in the map.
                :onCompleted (deref onCompletedCollector wait-time false)
                ;; Wait for everything that has been sent to the agent
                ;; to drain (presumably internal message queues):
                :onNext      (do (await-for wait-time onNextCollector)
                                 ;; Then produce the results:
                                 @onNextCollector)
                ;; If we ever saw an error, here it is:
                :onError     @onErrorCollector
                }))]
      ;; Recognize that the observable 'obl' may run on another thread:
      (-> obl
          (.subscribe collect-next collect-error collect-completed))
      ;; Therefore, produce results that wait, with timeouts, on both
      ;; the completion event and on the draining of the (presumed)
      ;; message queue to the agent.
      (report-collectors))))

;;;  ___ _        _      _   _
;;; / __| |_  _ _(_)_ _ | |_(_)_ _  __ _
;;; \__ \ ' \| '_| | ' \| / / | ' \/ _` |
;;; |___/_||_|_| |_|_||_|_\_\_|_||_\__, |
;;;                                |___/


;;; There is a class of operators for shrinking a sequence. They
;;; include "take", "takeUntil", etc.; "skip*"; and "filter". To
;;; start, let's just take the first two numbers out of a vector of
;;; numbers and turn them into oseq. This illustrates "take", a method
;;; that often shortens sequences.

(->
 (Observable/toObservable [1 2 3])
 (.take 2)
 (subscribe-collectors)
 (pdump)
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
 (pdump)
 )

;;; Let's operate on strings.

(->
 (Observable/toObservable ["one" "two" "three"])
 (.take 2)
 (subscribe-collectors)
 (pdump)
 )

;;; "seq" explodes strings into lazy sequences of characters:

(seq "one")

;;; Just for self-documenting code, define an alias

(def string-explode seq)

(->
 (Observable/toObservable ["one" "two" "three"])
 (.mapMany #(Observable/toObservable (string-explode %)))
 (subscribe-collectors)
 (pdump)
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
 (pdump)
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
 (pdump)
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
 (pdump)
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
 (pdump)
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
 (pdump)
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
      (subscribe-collectors)
      (pdump)))

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
  (pdump)
)

;;; It's well-behaved on an empty input:

(->
  (Observable/toObservable [])
  (.mapMany (comp from-seq string-explode))
  (distinct-until-changed)
  (subscribe-collectors)
  (pdump)
)

(defn flip [f2] (fn [x y] (f2 y x)))

;;;  ___              _                             
;;; / __|_  _ _ _  __| |_  _ _ ___ _ _  ___ _  _ ___
;;; \__ \ || | ' \/ _| ' \| '_/ _ \ ' \/ _ \ || (_-<
;;; |___/\_, |_||_\__|_||_|_| \___/_||_\___/\_,_/__/
;;;      |__/                                       
;;;   ___  _                         _    _     
;;;  / _ \| |__ ___ ___ _ ___ ____ _| |__| |___ 
;;; | (_) | '_ (_-</ -_) '_\ V / _` | '_ \ / -_)
;;;  \___/|_.__/__/\___|_|  \_/\__,_|_.__/_\___|
;;;                                             

(defn customObservableBlocking []
  "A custom Observable whose 'subscribe' method does not return until
   the observable completes, that is, a 'blocking' observable.

  returns Observable<String>"
  (Observable/create
   (fn [observer]
     (doseq [x (range 50)]
       (-> observer
           (.onNext (str "SynchronousValue_" x))))
     ;; After sending all values, complete the sequence:
     (-> observer .onCompleted)
     ;; Return a NoOpSubsription, since this observable does not
     ;; return from its subscription call until it sends all messages
     ;; and completes. Thus, the thread receiving the "subscription"
     ;; can't unsubscribe until the observable complete. We say that
     ;; this observable "blocks."
     (Subscriptions/empty))))

(-> (customObservableBlocking)
    (.map (partial (flip clojure.string/split) #"_"))
    (.map (fn [[a b]] [a (Integer/parseInt b)]))
    (.filter (fn [[a b]] (= 0 (mod b 7))))
    (subscribe-collectors)
    (pdump)
    )

;;;    _                    _                             
;;;   /_\   ____  _ _ _  __| |_  _ _ ___ _ _  ___ _  _ ___
;;;  / _ \ (_-< || | ' \/ _| ' \| '_/ _ \ ' \/ _ \ || (_-<
;;; /_/ \_\/__/\_, |_||_\__|_||_|_| \___/_||_\___/\_,_/__/
;;;            |__/                                       
;;;   ___  _                         _    _     
;;;  / _ \| |__ ___ ___ _ ___ ____ _| |__| |___ 
;;; | (_) | '_ (_-</ -_) '_\ V / _` | '_ \ / -_)
;;;  \___/|_.__/__/\___|_|  \_/\__,_|_.__/_\___|
;;;                                             

(defn customObservableNonBlocking []
  "A custom Observable whose 'subscribe' method returns immediately
   and whose other actions; that is, onNext, onCompleted, onError;
   occur on another thread.

  returns Observable<String>"
  (Observable/create
   (fn [observer]
     (let [f (future
               (doseq [x (range 50)]
                 (-> observer (.onNext (str "AsynchValue_" x))))
               ;; After sending all values, complete the sequence:
               (-> observer .onCompleted))]
       ;; Return a subscription that cancels the future:
       (Subscriptions/create #(future-cancel f))))))

(-> (customObservableNonBlocking)
    (.map (partial (flip clojure.string/split) #"_"))
    (.map (fn [[a b]] [a (Integer/parseInt b)]))
    (.filter (fn [[a b]] (= 0 (mod b 7))))
    (subscribe-collectors)
    (pdump)
    )

;;;    _                    _     __      __   _      ___                  
;;;   /_\   ____  _ _ _  __| |_   \ \    / /__| |__  | _ \__ _ __ _ ___ ___
;;;  / _ \ (_-< || | ' \/ _| ' \   \ \/\/ / -_) '_ \ |  _/ _` / _` / -_|_-<
;;; /_/ \_\/__/\_, |_||_\__|_||_|   \_/\_/\___|_.__/ |_| \__,_\__, \___/__/
;;;            |__/                                           |___/        

(defn asynchronousWikipediaArticleObservable [names]
  "Fetch a list of Wikipedia articles asynchronously.

   return Observable<String> of HTML"
  (Observable/create
   (fn [observer]
     (let [f (future
               (doseq [name names]
                 (-> observer
                     ;; Use "enlive" to parse & scrape html:
                     (.onNext
                      (html/html-resource
                       (java.net.URL.
                        (str "http://en.wikipedia.org/wiki/" name))))
                     ;; Netflix originally used strings, but...
                     #_(.onNext (http/get
                               (str "http://en.wikipedia.org/wiki/" name)))
                     )
                 )
               ;; After sending response to onNext, complete the
               ;; sequence:
               (-> observer .onCompleted))]
       ;; A subscription that cancels the future if unsubscribed:
       (Subscriptions/create #(future-cancel f))))))

;;; There is something in the "Atom" web page that xml/parse does not
;;; like. Rather than debug that, let's use enlive

(defn zip-str [s]
  (zip/xml-zip 
   (xml/parse 
    (java.io.ByteArrayInputStream. 
     (.getBytes s)))))

(->>
 ((subscribe-collectors
   (asynchronousWikipediaArticleObservable ["Atom" "Molecule"])
   5000)
  :onNext)
 (map #(html/select % [:title]))
 (pdump))

(pdump (+ 4 3))
