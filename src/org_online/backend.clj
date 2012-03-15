(ns org-online.backend
  (:use clojure.tools.cli
        clojure.java.io)
  (:require [clojure.string :as string]))


(def org-directories (atom []))

(defn add-org-directory [directory]
  (swap! org-directories conj (as-file directory)))

(defn get-all-org-files []
  (->> @org-directories
       (map #(.listFiles %))
       (apply concat)
       (filter #(re-seq #"org$" (.getName %)))))

(defn convert-org-file-to-html [file]
  (slurp file))

(def admin (atom nil))

