(ns org-online.backend
  (:use clojure.tools.cli
        clojure.java.io
        hiccup.core
        hiccup.page-helpers
        hiccup.form-helpers)
  (:require [clojure.string :as string])
  (:import java.io.StringWriter))


(defonce org-directories (atom []))

(def ^{:dynamic true} depth 0)

(defn add-org-directory [directory]
  (swap! org-directories conj (as-file directory)))

(defn load-directories-from-file [file]
  (with-open [*in* (reader file)]
    (doall
     (map add-org-directory (line-seq *in*)))))

(defn get-all-org-files []
  (->> @org-directories
       (map #(.listFiles %))
       (apply concat)
       (filter #(re-seq #"org$" (.getName %)))))

(defonce todos (atom #{}))
(defonce dones (atom #{}))
(defonce tags (atom #{}))

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

(defn markup-tags [line]
  (let [[[match prev tags post]]
        (re-seq (re-pattern (str "(.*):(" (clojure.string/join "|" @tags) "):(.*)"))
                line)]
    (if match
      (let [tags (clojure.string/split tag #":")]
        (list prev (for [tag tags]
                     [:span.tag tag]) post))
        line)))

(defn markup-todos [line]
  (let [[[match prev todo post]]
        (re-seq (re-pattern (str "(.*)("
                                 (clojure.string/join
                                  "|" (clojure.set/union @todos @dones))
                                 ")(.*)"))
                line)]
    (if match
      (if (@todos todo)
        (list prev [:span.todo todo] (markup-tags post))
        (list prev [:span.done todo] (markup-tags post)))
      (markup-tags line))))

(defn process-lines [lines]
  (if (string? lines)
    [:p (create-links lines)]
    (let [id (gensym "id")]
      [:div 
       [(keyword (str "h" (count-stars (first lines))))
        {:id (str "head-" id) :class "folder"}
        (markup-todos (.substring (first lines) (count-stars (first lines))))]
       [:div.foldable {:id (str "body-" id)}
        (map process-lines (rest lines))]])))

(defn process-cfg-lines [lines]
  (swap! todos (constantly #{}))
  (swap! dones (constantly #{}))
  (swap! tags (constantly #{}))
  (doseq [[match key value]
          (remove nil? (map #(re-matches #"\s*\#\+(\w+)\: (.*)" %) lines))]
    (print key)
    (condp re-seq key
      #"TODO" (let [[todo done] (split-with #(not (re-seq #"\|" %))
                                            (clojure.string/split value #" "))]
                       (swap! todos clojure.set/union (set todo))
                       (swap! dones clojure.set/union (set (rest done))))
      #"(TAGS|tags)" (let [tag
                           (remove nil? (map
                                         (comp second #(re-matches #"(\w+).*" %))
                                         (clojure.string/split value #"([ {}]+)")))]
                       (swap! tags clojure.set/union (set tag)))
      "default")))


(defn convert-org-file-to-html [file]
  (with-open [*in* (reader file)]
    (let [lines (line-seq *in*)]
      (process-cfg-lines lines)
      (doall
       (map process-lines (header-tree lines))))))

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
    (save-state input-file)))(
