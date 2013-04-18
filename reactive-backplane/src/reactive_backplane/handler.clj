(ns reactive-backplane.handler
  (:use     compojure.core)
  (:use     clojure.pprint)             ; doesn't work
  (:use     ring.middleware.json-params)
  (:require [compojure.handler               :as handler]
            [compojure.route                 :as route  ]
            [clj-json.core                   :as json   ]
            [reactive-backplane.elem         :as elem   ]
            [clojure.data.json               :as cdjson ]
            [clojure.string                  :as s      ]
            [clojure.pprint                  :as pp     ]
            [reactive-backplane.ring-buffer  :as rb     ]
            )
  (:import  [org.webbitserver         WebServer WebServers WebSocketHandler]
            [org.webbitserver.handler StaticFileHandler                    ]
            [rx                       Observable Observer Subscription     ]
            [rx.subscriptions         Subscriptions                        ]
            [rx.util                  AtomicObservableSubscription         ]
            ))

;;; ================================================================
;;; Mocks for hit rate
;;; ================================================================



;;; ================================================================
;;; compojure REST web server
;;; ================================================================

(defn json-response [data & [status]]
  {:status  (or status 200)
   :headers {"Content-Type" "application/json"}
   :body    (json/generate-string data)})

(defroutes app-routes

  (GET "/" []
       (println "GET /")
       (let [r (json-response {"hello" "json-get"})]
         (println {:json-response r})
         r ))

  (GET "/elems" []
       (println "GET /elems")
       (let [e (elem/list)
             j (json-response e)]
         (println {:elem-list e, :json-response j})
         j ))

  (GET "/elems/:key" [key]
       (println "GET /elems/" key)
       ;; Clojure nil gets jsonized as {..., :body null},
       ;; which is invalid json
       (let [e (elem/get key)
             j (json-response e)]
         (println {:key key, :elem-list e,
                  :json-response j})
         j ))

  (PUT "/elems/:key" [key data]
       (println "PUT /elems/" key)
       (let [e (elem/put key data)
             j (json-response e)]
         (println {:key key, :data data,
                  :elem-put e, :json-response j})
         j ))

  (PUT "/" [name]
       (println "PUT /")
       (println name)
       (println {:name name})
       (json-response {"hello" name}))

  (POST "/messages" [data]
       (println "POST /messages")
       (println data)
       (json-response {"stuff" data})
       )

  (route/not-found "Not Found"))

(def app
  (do
    (let [server (WebServers/createWebServer 8080)]
      (doto server
        (.add "/websocket"
              (proxy [WebSocketHandler] []

                (onOpen    [conn]
                  (println "WEBSOCKET OPENED"  conn)
                  (-> conn
                      (.send (cdjson/write-str {:foo "bar"}))))

                (onClose   [c  ]
                  (println "WEBSOCKET CLOSED"  c)
                  )

                (onMessage [c j]
                  (println "WEBSOCKET MESSAGE" c j)
                  )
                ))
        (.add (StaticFileHandler. "."))
        (.start)))
    (-> app-routes wrap-json-params))

  ;;  (handler/site app-routes) ; <---===/// absolutely does not work

  )
