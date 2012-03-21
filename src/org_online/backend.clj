(ns org-online.backend
  (:use clojure.tools.cli
        clojure.java.io
        hiccup.core
        hiccup.page-helpers
        hiccup.form-helpers)
  (:require [clojure.string :as string])
  (:import java.io.StringWriter))


(defonce org-directories (atom []))

(defn add-org-directory [directory]
  (swap! org-directories conj (as-file directory)))

(defn get-all-org-files []
  (->> @org-directories
       (map #(.listFiles %))
       (apply concat)
       (filter #(re-seq #"org$" (.getName %)))))

(defn count-stars [line]
  (.length (first (re-seq #"\*+" line))))

(defn paste-in-tree [tree paste depth]
  (if (zero? depth)
    (concat tree paste)
    (concat (butlast tree) [(paste-in-tree (last tree) paste (dec depth))])))

(defn header-tree [lines & [tree depth]]
  (let [[prev [header & rest]] (split-with #(not (re-seq #"^\*+ " %)) lines)
        depth (or depth 0)]
    (cond (nil? header)
          (paste-in-tree tree prev depth)
          (> (count-stars header) depth)
          (header-tree rest (paste-in-tree tree (concat prev [[header]]) depth) (inc depth))
          (< (count-stars header) depth)
          (header-tree rest (paste-in-tree (paste-in-tree tree prev depth)
                                           [[header]] (dec (dec depth)))
                       (dec depth))
          (= (count-stars header) depth)
          (header-tree rest (paste-in-tree (paste-in-tree tree prev depth)
                                           [[header]] (dec depth))
                       depth)
          )))

(defn create-links [line]
  (let [[[match prev address description post]]
        (re-seq #"(.*)\[\[(.+)\]\[(.+)\]\](.*)" line)]
    (if match
      (list prev (link-to address description) post)
      line)))

(defn process-lines [lines]
  (if (string? lines)
    [:p (create-links lines)]
    (let [id (gensym "id")]
      [:div 
       [(keyword (str "h" depth)) {:id (str "head-" id) :class "folder"}
        (.substring (first lines) (count-stars (first lines)))]
       [:div.foldable {:id (str "body-" id)}
        (map process-lines (rest lines))]])))

(defn convert-org-file-to-html [file]
  (binding [depth 0]
    (with-open [*in* (reader file)]
      (doall
       (map process-lines (header-tree (line-seq *in*)))))))

(defonce admin (atom nil))

(defn save-state
  [output-file]
  (with-open [*out* (writer output-file)]
    (print-dup @admin *out*)
    (print-dup (vec (map #(.getPath %) @org-directories)) *out*)))

(defn load-state
  [input-file]
  (if (.exists (as-file input-file))
    (with-open [*in* (java.io.PushbackReader. (reader input-file))]
      (dosync
       (swap! admin (constantly (read *in*)))
       (swap! org-directories (constantly (vec (map as-file (read *in*)))))))
    (save-state input-file)))
