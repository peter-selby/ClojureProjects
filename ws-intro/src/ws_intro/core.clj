(ns ws-intro.core
  (:require [clojure.data.json :as json]
            [clojure.string    :as s])
  (:import  [org.webbitserver WebServer WebServers WebSocketHandler]
            [org.webbitserver.handler StaticFileHandler]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
