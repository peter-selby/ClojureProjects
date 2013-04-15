(ns ws-intro.core
  (:require [clojure.data.json :as json]
            [clojure.string    :as s]
            [clojure.pprint    :as pp])
  (:import  [org.webbitserver         WebServer WebServers WebSocketHandler]
            [org.webbitserver.handler StaticFileHandler                    ]
            [rx                       Observable Observer Subscription     ]
            [rx.subscriptions         Subscriptions                        ]
            [rx.util                  AtomicObservableSubscription         ]
            ))

;;; ================================================================

(defn ^Observable observable
  "Create an observable from the given handler. When subscribed to, (handler observer)
  is called at which point, handler can start emitting values, etc."
  [handler]
  (Observable/create handler))

(defn getMock [] (json/read-str (slurp "traffic.json")))

(defn mockObservable [mock]
  (observable
   (fn [observer]
     (let [f (future 
               (-> observer (.onNext "1"))
               (Thread/sleep 1000)
               (-> observer (.onNext "2"))
               (Thread/sleep 1000)
               (-> observer (.onNext "3"))
               (-> observer (.onCompleted)))
           ]
       (Subscriptions/create #(future-cancel f))))))

;;; ================================================================

(defn on-message [conn json-message]
  (println "WEBSOCKET MESSAGE" conn json-message)
  (let [msg (->
             json-message
             json/read-json
             (get-in [:data :message]))]
    (.send conn (json/write-str
                 {:type "upcased"
                  :message (s/upper-case msg)}))
    ))

(defn -main []
  "Thanks to blog.jayfields.com"
  (pp/pprint (getMock))
  (let [server (WebServers/createWebServer 8080)]
    (doto server
      (.add "/websocket"
            (proxy [WebSocketHandler] []

              (onOpen    [conn]
                (println "WEBSOCKET OPENED"  conn)
                (-> (mockObservable (getMock))
                    (.subscribe
                     #_(fn [datum]
                       (-> conn
                           (.send json)))
                     println
                     ))
                )

              (onClose   [c  ]
                (println "WEBSOCKET CLOSED"  c)
                )

              (onMessage [c j]
                ;(println "WEBSOCKET MESSAGE" c j)
                (on-message c j)
                )
              ))
      (.add (StaticFileHandler. "."))
      (.start))))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
