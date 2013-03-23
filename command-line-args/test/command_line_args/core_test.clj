(ns command-line-args.core-test
  (:use clojure.test
        command-line-args.core))

(deftest pair-of-values
  (let [args ["--server" "localhost"
              "--port" "8080"
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
;; TODO.

(deftest d-test
  (testing "dynamic binding and let shadowing"
    (do (f4)
        (is (= v                       4)))))

(deftest e-001-test
  (testing "count on vectors"
    (is (count [19 "yellow" true]) 3)))

(deftest e-002-test
  (testing "reverse a vector"
    (is (= (reverse [2 4 7])       [7 4 2]))))

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

;;; There appears to be an error in the book, on page 132. The book
;;; implies that "halve!" modifies the data "plays" in-place, but I
;;; cannot reproduce that behavior. It appears to me that "halve!" is
;;; indeed a pure function, and I will test it as such, leaving it
;;; mis-named with the trailing bang "!" character, so as not to
;;; interfere with the names in the book.
(defn halve! [ks]
  (map (partial manip-map #(int (/ % 2)) ks)
       plays))

(deftest purity-test-001
  (testing "Testing purity of the book's \"halve!\" function, most likely mis-named with a trailing bang character, on page 132.")
  (is (= (map :plays plays) '(979 2333, 979, 2665))
      (= (map :plays (halve! [:plays])) '(489 1166 489 1332))))