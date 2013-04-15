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

(defn randomizedMock [mock]
  (let [randrate (fn [] (str
                        (/
                         (Math/round (* 10000 (rand)))
                         1000.0)))]
    {"hitRateAggs"
     [{"domain" "vendorinfoportal.amazon.com", "hitsPerSec" (randrate)}
      {"domain" "w.amazon.com",                "hitsPerSec" (randrate)}
      {"domain" "cr.amazon.com",               "hitsPerSec" (randrate)}
      {"domain" "external",                    "hitsPerSec" (randrate)}
      {"domain" "vendormaster.amazon.com",     "hitsPerSec" (randrate)}
      {"domain" "permissions.amazon.com",      "hitsPerSec" (randrate)}
      {"domain" "devcentral.amazon.com",       "hitsPerSec" (randrate)}
      {"domain" "build.amazon.com",            "hitsPerSec" (randrate)}],
     "referralMatrix"
     [{"from" "vendorinfoportal.amazon.com", "to" "vendorinfoportal.amazon.com",
       "refsPerSec" (randrate)}
      {"from" "vendorinfoportal.amazon.com", "to" "w.amazon.com",
       "refsPerSec" (randrate)}
      {"from" "vendorinfoportal.amazon.com", "to" "external",
       "refsPerSec" (randrate)}
      {"from" "vendorinfoportal.amazon.com", "to" "vendormaster.amazon.com",
       "refsPerSec" (randrate)}
      {"from" "w.amazon.com", "to" "w.amazon.com",
       "refsPerSec" (randrate)}
      {"from" "w.amazon.com", "to" "external",
       "refsPerSec" (randrate)}
      {"from" "w.amazon.com", "to" "vendorinfoportal.amazon.com",
       "refsPerSec" (randrate)}
      {"from" "vendormaster.amazon.com", "to" "vendormaster.amazon.com",
       "refsPerSec" (randrate)}
      {"from" "vendormaster.amazon.com", "to" "w.amazon.com",
       "refsPerSec" (randrate)}
      {"from" "vendormaster.amazon.com", "to" "external",
       "refsPerSec" (randrate)}
      {"from" "permissions.amazon.com", "to" "permissions.amazon.com",
       "refsPerSec" (randrate)}
      {"from" "devcentral.amazon.com", "to" "devcentral.amazon.com",
       "refsPerSec" (randrate)}]})
  )

(defn mockObservable [mock]
  (observable
   (fn [observer]
     (let [f (future
               (doseq [i (range 100)]
                 (-> observer (.onNext (randomizedMock mock)))
                 (Thread/sleep 1000)
                 )
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
