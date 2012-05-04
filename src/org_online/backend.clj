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

(defn count-stars [line]
  (.length (first (re-seq #"\*+" line))))

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
      (let [tags (string/split tags #":")]
        (list prev (for [tag tags]
                     [:span.tag tag]) post))
      line)))

(defn markup-todos [line {:keys [todos dones]}]
  (let [[[match prev todo post]]
        (re-seq (re-pattern (str "(.*)("
                                 (string/join
                                  "|" (union todos dones))
                                 ")(.*)"))
                line)]
    (if match
      (if (todos todo)
        (list prev [:span.todo todo] (markup-tags post))
        (list prev [:span.done todo] (markup-tags post)))
      (markup-tags line))))

(defn process-cfg-lines [lines]
  (apply (partial merge-with union)
         (for [[match key value]
               (remove nil? (map #(re-matches #"\s*\#\+(\w+)\: (.*)" %) lines))]
           (condp re-seq key
             #"TODO" (let [[todos dones] (split-with #(not (re-seq #"\|" %))
                                                     (string/split value #" "))]
                       {:todos (set todos) :dones (set (rest dones))})
             #"(TAGS|tags)" (let [tags
                                  (remove nil? (map
                                                (comp second #(re-matches #"(\w+).*" %))
                                                (string/split value #"([ {}]+)")))]
                              {:tags (set tags)})
             {}))))

(defonce admin (atom nil))

(defn save-passwords-to-file [file]
  (with-open [*out* (writer file)]
    (print-dup @admin *out*)))

(defn load-passwords-from-file [file]
  (when (.exists (as-file file))
    (with-open [*in* (java.io.PushbackReader. (reader file))]
      (dosync
       (swap! admin (constantly (read *in*)))))))

;; new fully functional version
(declare next-line header-string parse-header)

(defn properties-line [[line & rest] state]
  (when line
    (condp re-seq line
      #":END:" (partial next-line rest state)
      #":(.+):" (let [[[match label value]] (re-seq #":(.+):\s+(.+)" line)]
                  (println (html [:span.property [:em label] (str ": " value)]))
                  (partial properties-line rest state))
      (partial properties-line rest state))))

(defn next-line [lines {:keys [properties? depth ids] :as state}]
  (let [line (first lines)]
    (cond (nil? line)
          (doseq [index ids]
            (println "</div></div>"))
          (re-seq #":PROPERTIES:" line)
          (partial properties-line (rest lines) state)
          (re-seq #"^\*+ " line)
          (partial parse-header lines state)
          :else
          (do
            (println (html [:p (create-links line)]))
            (partial next-line (rest lines) state)))))

(defn header-string [stars line id cfg]
  (html [(keyword (str "h" stars))
         {:id (str "head-" id) :class "folder"}
         (markup-todos (.substring line stars) cfg)]))

(defn parse-header [lines {:keys [depth ids cfg] :as state}]
  (let [line (first lines)
        stars (count-stars line)
        id (gensym)
        closings (inc (- depth stars))]
    (doseq [index (range closings)]
      (println "</div></div>"))
    (println "<div>")
    (println (header-string stars line id cfg))
    (println (str "<div class=\"foldable\" id=\"body-" id "\">"))
    (partial next-line (rest lines)
             (assoc state
               :depth stars
               :ids (cons id (drop closings ids))))))

(defn first-lines [lines state]
  (partial next-line lines (assoc state :depth 0)))

(defn convert-org-file-to-html [file]
  (with-open [output (StringWriter.)]
    (with-open [input (reader file)]
      (binding [*out* output]
        (let [lines (line-seq input)
              cfg (process-cfg-lines lines)]
          (trampoline first-lines lines {:cfg cfg})))
      (.toString output))))
