(ns web-client-trial.core
  (:use clojure.pprint)
  (:require [clj-http.client :as client]
            [clj-json.core   :as json  ]
            ))

(def ^:private site "http://localhost:3000")

(defn- getelems []
  (-> (str site "/elems")
      client/get
      :body
      json/parse-string))

(defn- getelem-light [key]
  (-> (str site "/elems/" key)
      client/get
      :body))

(defn- getelem [key]
  (-> (str site "/elems/" key)
      client/get
      :body
      json/parse-string))

(defn- putelem [key data]
  (let [b (json/generate-string {:data data})]
    (pprint b)
    (-> (str site "/elems/" key)
        (client/put
         {:body b,
          :content-type :json
          })
        :body
        json/parse-string)))

(defn- justget []
  (->  site  client/get))

(defn- justput [nym]
  (-> site
      (client/put
       {:body (json/generate-string {:name nym})
        :content-type :json
        })
      :body
      json/parse-string))

(defn -main
  [& args]
  {}
  (println) (pprint (justget))
  (println) (pprint (justput "jacker"))
  (println) (pprint (getelems))
  (println) (pprint (getelem-light 1))  ; null
  (println) (pprint (getelem 1))        ; nil
  (println) (pprint (putelem 1 {:tag 42}))
  (println) (pprint (getelems))
  )
