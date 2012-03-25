(ns org-online.server
  (:use org-online.backend
        clojure.tools.cli
        clojure.java.io)
  (:require [clojure.string :as string]
            [swank.swank :as swank]
            [noir.server :as server]
	    [hiccup.core :as hiccup]))

;;; load *all* views and view files
(server/load-views "src/org_online/views/")

(defn wrap-uri-prefix
  "Middleware to place entire website under a subdirectory or prefix,
   ie 'example.com/foo/application' instead of 'example.com/application'"
  [handler prefix]
  (fn [request]
     (handler (assoc request
                     :uri (string/replace-first (:uri request) 
                                                (re-pattern (str "^" prefix "/?"))
                                                "/")))))

(defn- log [msg & vals]
  (let [line (apply format msg vals)]
    (locking System/out (println line))))

(defn wrap-request-logging
  "Middleware to automatically log every request to the server."
  [handler]
  (fn [{:keys [request-method uri] :as req}]
    (let [start  (System/currentTimeMillis)
          resp   (handler req)
          finish (System/currentTimeMillis)
          total  (- finish start)]
      (log "request %s %s (%dms)" request-method uri total)
      resp)))

(defonce ^{:documentation "The server handle through which the program can start or start the internal Jetty server."}
  server nil)

(defonce ^{:documentation "The directory in which to look for config files."}
  config-dir (as-file "./config"))

(defonce ^{:documentation "The directory in which to look for data files."}
  data-dir (as-file "./data"))

(defn -main [& m]
  (let [[{:keys [port mode config directory prefix start-swank]} args banne]
        (cli m
	     ["-p" "--port" "The port to listen on" 
	      :default (Integer. (get (System/getenv) "PORT" "8080")) 
	      :parse-fn #(Integer. %)]
	     ["-m" "--mode" "The run mode" 
	      :default :dev 
	      :parse-fn keyword]
             ["-c" "--config" "The directory to search for config files"
              :default "./config"]
             ["-d" "--directory" "The directory to search for GTD files"
              :default "./"]
	     ["-P" "--prefix" "The url prefix"]
         ["-s" "---no-start-swank" "Don't Start Swank"])]
    (if-not start-swank
      (swank/start-repl "4009"))
    (when prefix
      (server/add-middleware wrap-uri-prefix prefix))
    (server/add-middleware wrap-request-logging)
    (when config
      (def config-dir (as-file config)))
    (when directory
      (add-org-directory directory))
    (load-directories-from-file (file config-dir "directories"))
    (load-passwords-from-file (file data-dir "passwords"))
    (def server (server/start port {:mode mode
                                    :ns 'org-online
                                    :base-url prefix}))))

