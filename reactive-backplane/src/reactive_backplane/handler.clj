(ns reactive-backplane.handler
  (:use     compojure.core)
  (:use     ring.middleware.json-params)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [clj-json.core :as json]))

(defn json-response [data & [status]]
  {:status  (or status 200)
   :headers {"Content-Type" "application/json"}
   :body    (json/generate-string data)})

(defroutes app-routes
  (GET "/" []
       (do (json-response {"hello" "json get"}))
       )
  (PUT "/" [name]
       (println name)
       (do (json-response {"hello" name}))
       )
  (route/not-found "Not Found"))

(def app
  (-> app-routes wrap-json-params)
;;  (handler/site app-routes) ;; <---===/// absolutely does not work
  )
