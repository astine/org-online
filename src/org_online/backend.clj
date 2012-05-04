(ns org-online.backend
  (:use clojure.tools.cli
        clojure.java.io
        clojure.set
        hiccup.core
        hiccup.page-helpers
        hiccup.form-helpers)
  (:require [clojure.string :as string]
            [noir.util.crypt :as crypt])
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
          (header-tree rest (paste-in-tree tree (concat prev [[header]]) depth)
                       (count-stars header))
          (< (count-stars header) depth)
          (header-tree rest (paste-in-tree (paste-in-tree tree prev depth)
                                           [[header]] (dec (count-stars header)))
                       (count-stars header))
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
        (re-seq #"(.* ):([\w:]+): *$" line)]
    (if match
      (let [tags (clojure.string/split tags #":")]
        (list prev (for [tag tags]
                     [:span.tag tag]) post))
      line)))

(defn markup-todos [line]
  (let [[[match prev todo post]]
        (re-seq (re-pattern (str "(.*)("
                                 (clojure.string/join
                                  "|" (union @todos @dones))
                                 ")(.*)"))
                line)]
    (if match
      (if (@todos todo)
        (list prev [:span.todo todo] (markup-tags post))
        (list prev [:span.done todo] (markup-tags post)))
      (markup-tags line))))

;(defn markup-lists [lines]
  ;(for [sublist (partition-by #(re-seq #"^ [-+*] " %) lines)]
    ;(if (re-seq #"^ [-+*] " (first sublist))
      ;[:ul
       ;(for [line sublist]
         ;[:li (.substring line 3)])]
      ;[:p
       ;(for [line sublist]
         ;[line [:br]])])))
             
(defn process-lines [lines]
  (cond (string? lines)
        [:p (create-links lines)]
        (vector? lines)
        lines
        :default
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
    (condp re-seq key
      #"TODO" (let [[todo done] (split-with #(not (re-seq #"\|" %))
                                            (clojure.string/split value #" "))]
                       (swap! todos union (set todo))
                       (swap! dones union (set (rest done))))
      #"(TAGS|tags)" (let [tag
                           (remove nil? (map
                                         (comp second #(re-matches #"(\w+).*" %))
                                         (clojure.string/split value #"([ {}]+)")))]
                       (swap! tags union (set tag)))
      "default")))


(defn convert-org-file-to-html [file]
  (with-open [*in* (reader file)]
    (let [lines (line-seq *in*)]
      (process-cfg-lines lines)
      (doall
       (map process-lines (header-tree lines))))))

(defonce admin (atom nil))


(defn save-passwords-to-file [file]
  (with-open [*out* (writer file)]
    (print-dup @admin *out*)))

(defn load-passwords-from-file [file]
  (when (.exists (as-file file))
    (with-open [*in* (java.io.PushbackReader. (reader file))]
      (dosync
       (swap! admin (constantly (read *in*)))
       (swap! org-directories (constantly (vec (map as-file (read *in*)))))))
    (save-state input-file)))

;; new fully functional version
(declare next-line header-string parse-header)

(defn next-line [lines {:keys [depth ids] :as state}]
  (let [line (first lines)]
    (cond (nil? line)
          (do
            (doseq [index ids]
              (println "</div></div>"))
            (println "</body>")
            (println "</html>"))
          (re-seq #"^\*+ " line)
          (partial parse-header lines state)
          :else
          (do
            (println (html [:p (create-links line)]))
            (partial next-line (rest lines) state)))))

(defn header-string [stars line id]
  (html [(keyword (str "h" stars))
         {:id (str "head-" id) :class "folder"}
         (markup-todos (.substring line stars))]))

(defn parse-header [lines {:keys [depth ids] :as state}]
  (let [line (first lines)
        stars (count-stars line)
        id (gensym)
        closings (inc (- depth stars))]
    (doseq [index (range closings)]
      (println "</div>")
      (println "</div>"))
    (println "<div>")
    (println (header-string stars line id))
    (println (str "<div class=\"foldable\" id=\"body-" id "\">"))
    (partial next-line (rest lines)
             (assoc state
               :depth stars
               :ids (cons id (drop closings ids))))))

(defn first-lines [lines]
  (println "<!DOCTYPE html PUBLIC '-//W3C//DTD XHTML 1.0 Strict//EN' 'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd'>")
  (println "<html xmlns='http://www.w3.org/1999/xhtml'>")
  (println (html [:head
                  [:title "org-online"]
                  (include-css "css/reset.css"
                               "css/org-style.css")
                  (include-js "js/jquery.js"
                              "js/jquery-ui.js"
                              "js/app.js")]))
  (println "<body>")
  (partial next-line lines {:depth 0}))

(defn parse-org-file [file output]
  (binding [*out* output]
    (let [lines (line-seq file)]
      (process-cfg-lines lines)
      (trampoline first-lines lines))))
