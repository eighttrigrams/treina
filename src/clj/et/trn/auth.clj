(ns et.trn.auth
  (:require [buddy.sign.jwt :as jwt]
            [clojure.string :as str]))

(defn jwt-secret []
  (or (System/getenv "ADMIN_PASSWORD") "dev-secret"))

(defn create-token
  ([user-id username is-admin]
   (create-token {:user-id user-id :username username :is-admin is-admin}))
  ([claims]
   (jwt/sign claims (jwt-secret))))

(defn verify-token [token]
  (try
    (jwt/unsign token (jwt-secret))
    (catch Exception _ nil)))

(defn extract-token [req]
  (when-let [auth-header (get-in req [:headers "authorization"])]
    (when (str/starts-with? auth-header "Bearer ")
      (subs auth-header 7))))
