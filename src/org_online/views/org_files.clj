(ns org-online.views.org-files
  (:require [clojure.string :as string]
            [noir.session :as session]
            [noir.validation :as validation])
  (:use org-online.backend
        org-online.views.common
        [noir.core :only [defpage defpartial render]]
        [noir.response :only [redirect]]
        clojure.java.io
        hiccup.core
        hiccup.form-helpers
        hiccup.page-helpers))

(defpage "/index" []
  (ensure-login
   (layout
    [:h4 "Here are your Org files:"]
    (for [file (get-all-org-files)]
      (let [filename (.getName file)
            filename- (.substring filename 0 (- (count filename) 4))]
        [:div (link-to (str "/org-files/" filename-)
                       filename-)]))
    (form-to [:post "/logout"] (submit-button {:class "submit"} "Log Out")))))
  
(defpage org-file [:get "/org-files/:filename"] {filename :filename}
  (ensure-login
   (layout
    (convert-org-file-to-html
     (first (filter #(= (str filename ".org") (.getName %))
                    (get-all-org-files)))))))
  
