(ns org-online.views.org-files
  (:require [org-online.views.common :as common]
            [clojure.string :as string]
            [noir.session :as session]
            [noir.validation :as validation])
  (:use org-online.backend
        [noir.core :only [defpage defpartial render]]
        [noir.response :only [redirect]]
        clojure.java.io
        hiccup.core
        hiccup.form-helpers
        hiccup.page-helpers))

(defpage "/index" []
  (common/layout
   (for [file (get-all-org-files)]
     (let [filename (.getName file)
           filename- (.substring filename 0 (- (count filename) 4))]
       [:div (link-to (str "/org-files/" filename-)
                      filename-)]))))

(defpage org-file [:get "/org-files/:filename"] {filename :filename}
  (convert-org-file-to-html
   (first (filter #(= (str filename ".org") (.getName %))
                  (get-all-org-files)))))
  
