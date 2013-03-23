(ns command-line-args.core-test
  (:use clojure.test
        command-line-args.core))

(deftest pair-of-values
  (let [args ["--server"      "localhost"
              "--port"        "8080"
              "--environment" "production"]]
    (is (= {:server "localhost"
            :port "8080"
            :environment "production"}
           (parse-args args)))))

(deftest a-test
  (testing "The test test."
    (is (= 1 1))))

(deftest b-test
  (testing "keywordize [\"a\"] = [:a nil] -- missing value defaults to nil."
    (is (= (keywordize ["a"])         [:a nil]))))

(deftest c-001-test
  (testing "(keywordize [\"a\" 1 \"b\" 2]) -- extra key-value pairs ignored."
    (is (= (keywordize ["a" 1 "b" 2]) [:a 1]))))

(deftest c-002-test
  (testing "(keywordize [\"a\" 3 2]) -- extra values ignored."
    (is (= (keywordize ["a" 3 2])     [:a 3]))))

;; UDIAGNOSED PROBLEM: If I uncomment the following and let it fail,
;; then the nrepl test framework seems to get confused. It continues
;; to report failures, one more for each iteration of the test via C-c
;; C-,. Quitting and restarting the nrepl fixes it -- C-x C-b, switch
;; to nrepl buffer, d on every line beginning with "*nrepl", x, y, y,
;; y, M-x nrepl-jack-in -- but this is really draconian. I do not know
;; if this is a bug with nrepl or with my tests. If I can prove it's a
;; bug in my tests, then "fixtures" might be a good way to fix it.
;; TODO. -- More evidence: the nrepl testing infrastructure tends to
;; go haywire frequently. I haven't decided whether to try upgrading
;; it or just to forget it since leiningen works well.

(deftest d-test
  (testing "dynamic binding and let shadowing"
    (do (f4)
        (is (= v 4)))))

(deftest e-001-test
  (testing "count on vectors"
    (is (count [19 "yellow" true]) 3)))

(deftest e-002-test
  (testing "reverse a vector"
    (is (= (reverse [2 4 7]) [7 4 2]))))

(deftest e-003-test
  (testing "map over multiple collections of differing lengths"
    (is (= (map +
                [2 4 7]
                [5 6]
                [1 2 3 4])
           [8 12]))))

(deftest f-001-test
  (is (= 1 1))
  (is (= 2 2)))

(defn parting
  "returns a String containing a parting salutation, an anti-greeting
  if you will, and I am making this documentation extra long so that I
  can test C-c M-q for formatting the documentation."
  ; we don't have an attr-map
  [name] ; these are the params
  ; we don't have a prepost-map
  (str "Goodbye, " name)
  )

(deftest f-test
  (testing "test the parting function"
    (is (= (parting "Mark")
           "Goodbye, Mark"))))

(deftest g-test
  (testing "testing multiary power function"
    (is (= (power 2 3 4)
           4096.0)))) ; definitely NOT 4096 the integer


(def plays [{:band "Burial",     :plays 979,  :loved 9}
            {:band "Eno",        :plays 2333, :loved 15}
            {:band "Bill Evans", :plays 979,  :loved 9}
            {:band "Magma",      :plays 2665, :loved 31}])

(def sort-by-loved-ratio (partial sort-by #(/ (:plays %) (:loved %))))

(defn columns [column-names]
  (fn [row]
    (vec (map row column-names))))

(defn keys-apply [f ks m]
  "Takes a function, a set of keys, and a map and applies the function 
   to the map on the given keys.  A new map of the results of the function 
   applied to the keyed entries is returned."
  (let [only (select-keys m ks)]
    (zipmap (keys only) (map f (vals only)))))

(defn manip-map [f ks m]
  "Takes a function, a set of keys, and a map and applies the function 
   to the map on the given keys.  A modified version of the original map
   is returned with the results of the function applied to each keyed entry."
  (conj m (keys-apply f ks m)))

;;; It appears to me that "halve!" is indeed a pure function if it
;;; only closes over immutable values, and I will test it as such.
(defn halve! [ks]
  (map (partial manip-map #(int (/ % 2)) ks)
       plays))

(deftest purity-test-001
  (testing "Testing purity of the book's \"halve!\" function if  \"plays\" is immutable."
    (is (= (map :plays plays) '(979 2333, 979, 2665)))
    (is (= (map :plays (halve! [:plays])) '(489 1166 489 1332)))
    (is (= (map :plays plays) '(979 2333, 979, 2665))))
  )

(defn slope-optional
  [& {:keys [p1 p2] :or {p1 [0 0] p2 [1 1]} }]
  (let [dy (- (p2 1) (p1 1))
        dx (- (p2 0) (p1 0))]
    (float (/ dy dx))))

(deftest optional-and-named-arguments-test-001
  (testing "Optional and named arguments."
    (is (= -6.0 (slope-optional :p1 [4 15] :p2 [3 21])))
    (is (= 0.5 (slope-optional :p2 [2 1])))
    (is (= 1.0 (slope-optional)))
    ))

(defn slope
  "Documentation."
  [& {:keys [p1 p2] :or {p1 [0 0] p2 [1 1]} }]
  {:pre [(vector? p1)
         (vector? p2)
         (= 2 (count p1))
         (= 2 (count p2))
         (not= (p1 1) (p2 1))],
   :post (float? %)}
  (let [dy (- (p2 1) (p1 1))
        dx (- (p2 0) (p1 0))]
    (float (/ dy dx)))  
  )

(deftest vectors-are-functions-of-their-indices-001
  (testing "Vectors are functions of their indices."
    (-> 1.0 (= ([1.0] 0)) is)
    (is (thrown? java.lang.IndexOutOfBoundsException ([1.0] 1)))
    (is (thrown? java.lang.IndexOutOfBoundsException ([] 0)))
    (is (thrown? java.lang.IndexOutOfBoundsException ([] -1)))
    (is (thrown? java.lang.IndexOutOfBoundsException ([1.0] -1)))
    ))

(deftest maps-are-functions-of-their-keys-001
  (testing "Maps are functions of their keys."
    (->   1 (= ({:a 1}       :a)) is)
    (->   2 (= ({:a 1, :b 2} :b)) is)
    (-> nil (= ({:a 1, :b 2} :c)) is)
    (-> 'df (= ({} :a 'df      )) is)
    ))

(deftest keys-are-functions-that-look-up-values-in-maps-001
  (testing "Keys are functions that look up values in maps."
    (->   1 (= (:a {:a 1}))           is)
    (->   1 (= (:a {:a 1, :b 2}    )) is)
    (-> nil (= (:c {:a 1, :b 2}    )) is)
    (-> nil (= (:c {}              )) is)
    (-> 'df (= (:c {:a 1, :b 2} 'df)) is)
    (-> 'df (= (:c {}           'df)) is)
    ))

