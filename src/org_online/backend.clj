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
(declare next-line header-string parse-header list-line)

(defn end-lines [{:keys [ids] :as state}]
  (doseq [id ids]
    (println "</div></div>")))

(defmacro handle-end-lines [[line state] & body]
  `(if-not (nil? ~line)
     (do ~@body)
     (partial end-lines ~state)))

(defn ulist-line [[line & tail] {:keys [indents] :as state}]
  (handle-end-lines [line state]
    (let [[[match spaces bullet text]]
          (re-seq #"^(\s*)([+*-]?)\s*(.*)" line)
          line-indent (count spaces)]
      (cond (and (zero? line-indent)
                 (= bullet "*"))
            (do
              (doseq [i indents]
                (println "</li>")
                (println "</ul>"))
              (partial next-line (cons line tail) (dissoc state :indents)))
            (and (empty? indents)
                 (= bullet ""))
            (partial next-line (cons line tail) state)
            (and (#{"*" "+" "-"} bullet)
                 (or (empty? indents)
                     (> line-indent (first indents))))
            (do
              (println "<ul>")
              (println "<li>")
              (println (html (create-links text)))
              (println "<br/>")
              (partial ulist-line tail (assoc state :indents (cons line-indent indents))))
            (and (#{"*" "+" "-"} bullet)
                 (= line-indent (first indents)))
            (do
              (println "</li>")
              (println "<li>")
              (println (html (create-links text)))
              (println "<br/>")
              (partial ulist-line tail (assoc state :indents (cons line-indent (rest indents)))))
            (<= line-indent (first indents))
            (do
              (println "</li>")
              (println "</ul>")
              (partial ulist-line (cons line tail) (assoc state :indents (rest indents))))
            (> line-indent (first indents))
            (do
              (println (html (create-links text)))
              (println "<br/>")
              (partial ulist-line tail state))))))
 
(defn properties-line [[line & rest] state]
  (handle-end-lines [line state]
    (condp re-seq line
      #":END:" (partial next-line rest state)
      #":(.+):" (let [[[match label value]] (re-seq #":(.+):\s+(.+)" line)]
                  (println (html [:span.property [:em label] (str ": " value)]))
                  (partial properties-line rest state))
      (partial properties-line rest state))))

(defn next-line [[line & rest] {:keys [properties? depth] :as state}]
  (handle-end-lines [line state]
    (condp re-seq line 
      #":PROPERTIES:" (partial properties-line rest state)
      #"^\*+ " (partial parse-header (cons line rest) state)
      #"^(\s+[+*-] |[+-] )" (partial ulist-line (cons line rest) state)
      (do
        (println (html [:p (create-links line)]))
        (partial next-line rest state)))))

(defn header-string [stars line id cfg]
  (html [(keyword (str "h" stars))
         {:id (str "head-" id) :class "folder"}
         (markup-todos (.substring line stars) cfg)]))

(defn parse-header [[line & rest] {:keys [depth ids cfg] :as state}]
  (let [stars (count-stars line)
        id (gensym)
        closings (inc (- depth stars))]
    (doseq [index (range closings)]
      (println "</div></div>"))
    (println "<div>")
    (println (header-string stars line id cfg))
    (println (str "<div class=\"foldable\" id=\"body-" id "\">"))
    (partial next-line rest
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
