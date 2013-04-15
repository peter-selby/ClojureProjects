(ns ws-intro.core
  (:require [clojure.data.json :as json]
            [clojure.string    :as s])
  (:import  [org.webbitserver WebServer WebServers WebSocketHandler]
            [org.webbitserver.handler StaticFileHandler]))

(defn -main []
  "Thanks to blog.jayfields.com"
  (doto (WebServers/createWebServer 8080)
    (.add "/websocket"
          (proxy [WebSocketHandler] []
            (onOpen    [c  ] (println "websocket opened" c))
            (onClose   [c  ] (println "websocket closed" c))
            (onMessage [c j] (println "websocket message" c j))))
    (.add (StaticFileHandler. "."))
    (.start)))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
