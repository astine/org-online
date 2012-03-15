(ns org-online.views.welcome
  (:require [org-online.views.common :as common]
            [clojure.string :as string]
            [noir.session :as session]
            [noir.validation :as validation])
  (:use org-online.backend
        [noir.core :only [defpage defpartial render]]
        [noir.response :only [redirect]]
        hiccup.core
        hiccup.form-helpers
        hiccup.page-helpers))

(defpartial error-text [errors]
  [:p.validation-error (string/join "<br/>" errors)])

(defpartial user-fields [{:keys [username password] :as user}]
  (text-field {:placeholder "Username"} :username username)
  (password-field {:placeholder "Password"} :password))

(defpage "/welcome" []
  (if (session/get :username)
    (common/layout
     [:p "Welcome."]
     (form-to [:post "/logout"] (submit-button {:class "submit"} "Log Out")))
    (redirect "/login")))

(defpage "/login" {:as user}
  (common/layout
   (if @admin
     [:p "Please Log In:"]
     [:p "Please Create Admin Account:"])
   (form-to [:post "/login"]
            [:h4 "Login"]
            (validation/on-error :login error-text)
            (user-fields {:username (or (:username user) "") :password ""})
            (submit-button {:class "submit"} "submit"))))

(defpage [:post "/login"] {:as user}
  (when-not @admin
    (if (and (:username user) (:password user))
      (swap! admin (constantly user))
      (validation/set-error :login (str "Invalid user: " user))))
  (if (and (= (:username user) (:username @admin))
           (= (:password user) (:password @admin)))
    (do
      (session/put! :username (:username user))
      (redirect "/welcome"))
    (do
      (validation/set-error :login "Wrong password.")
      (render "/login" user))))

(defpage [:post "/logout"] []
  (session/clear!)
  (render "/login"))
  
    
