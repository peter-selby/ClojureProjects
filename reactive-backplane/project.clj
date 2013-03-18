(defproject reactive-backplane "0.1.0-SNAPSHOT"
  :description "Experiments with web service programming via http://mmcgrana.github.com/2010/08/clojure-rest-api.html and https://github.com/weavejester/compojure/wiki/Getting-Started"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure     "1.4.0"]
                 [ring-json-params        "0.1.3"]
                 [ring/ring-jetty-adapter "1.1.0"]
                 [clj-json                "0.5.1"] ;;; 0.5.2 and 0.5.3 verified fail
                 [compojure               "1.1.5"]]
  :plugins [[lein-ring   "0.8.2"]
            [lein-pprint "1.1.1"]]
  :ring {:handler reactive-backplane.handler/app}
  :profiles
  {:dev {:dependencies [[ring-mock "0.1.3"]]}})
