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

;;; This observable represents data produced by real-time streams on
;;; the server. Its observers are proxies for the client that
;;; internally send messages to the client via websockets.

(defn mockObservable [mock]
  (observable
   (fn [observer]
     (let [f (future
               (doseq [i (range 100)]
                 (-> observer (.onNext mock))
                 (Thread/sleep 1000)
                 )
               #_(-> observer (.onNext "1"))
               #_(Thread/sleep 1000)
               #_(-> observer (.onNext "2"))
               #_(Thread/sleep 1000)
               #_(-> observer (.onNext "3"))
               #_(-> observer (.onCompleted))
               )
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
                     (fn [datum]
                       (-> conn
                           (.send (json/write-str
                                    {:type "hitdata"
                                     :message (str datum)}))))
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
