(ns org-online.views.common
  (:use [noir.core :only [defpartial]]
        [hiccup.page-helpers :only [include-css include-js html5]]))

(defpartial layout [& content]
            (html5
              [:head
               [:title "org-online"]
               (include-css "/css/reset.css"
                            "/css/org-style.css")
               (include-js "/js/jquery.js"
                           "/js/jquery-ui.js"
                           "/js/app.js")]
              [:body
               [:div#wrapper
                content]]))

(defmacro ensure-login [& content]
  `(if (noir.session/get :username)
     (do ~@content)
     (noir.response/redirect "/login")))
