(defproject org-online "0.1.0-SNAPSHOT"
            :description "FIXME: write this!"
            :dependencies [[org.clojure/clojure "1.3.0"]
                           [org.clojure/tools.cli "0.2.1"]
                           [org.clojure/math.numeric-tower "0.0.1"]
                           [org.clojure/algo.generic "0.1.0"]
                           [clj-stacktrace "0.2.4"]
                           [noir "1.2.1"]
                           [org.clojars.ghoseb/cron4j "2.2.1"]
                           [inflections "0.6.5-SNAPSHOT"]
                           [ring-mock "0.1.1"]
                           [midje "1.3.1"]
                           [swank-clojure "1.4.0-SNAPSHOT"]]
            :dev-dependencies [[lein-midje "1.0.8"]] 
            :main org-online.server
            :test src)

