(ns et.trn.ui.state
  (:require [reagent.core :as r]
            [et.trn.ui.api :as api]))

(defonce *app-state
  (r/atom {:auth-required? nil   ;; nil = still loading
           :logged-in? false
           :token nil
           :current-user nil
           :error nil
           :view :trainings      ;; :trainings | :training | :overview
           :trainings []
           :selected-training nil
           :sessions []
           :all-sessions []      ;; overview feed: every session + :training_name
           :editing-training nil ;; the training the edit modal is open for
           :search ""}))

;; ---------------------------------------------------------------------------
;; helpers

(defn auth-headers []
  (if-let [token (:token @*app-state)]
    {"Authorization" (str "Bearer " token)}
    {}))

(defn set-error [msg]
  (swap! *app-state assoc :error msg))

(defn clear-error []
  (swap! *app-state assoc :error nil))

(defn- err-handler [fallback]
  (fn [resp]
    (set-error (get-in resp [:response :error] fallback))))

(defn today-str []
  (let [d (js/Date.)
        pad (fn [n] (if (< n 10) (str "0" n) (str n)))]
    (str (.getFullYear d) "-" (pad (inc (.getMonth d))) "-" (pad (.getDate d)))))

;; ---------------------------------------------------------------------------
;; auth

(defn- save-token! [token user]
  (when token (.setItem js/localStorage "treina-token" token))
  (when user (.setItem js/localStorage "treina-user" (js/JSON.stringify (clj->js user)))))

(defn- clear-token! []
  (.removeItem js/localStorage "treina-token")
  (.removeItem js/localStorage "treina-user"))

(declare fetch-trainings)

(defn fetch-auth-required []
  (api/fetch-json "/api/auth/required" {}
    (fn [{:keys [required]}]
      (swap! *app-state assoc :auth-required? required)
      (if-not required
        (do (swap! *app-state assoc :logged-in? true)
            (fetch-trainings))
        (let [token (.getItem js/localStorage "treina-token")
              user-str (.getItem js/localStorage "treina-user")]
          (when (and token user-str)
            (swap! *app-state assoc
                   :logged-in? true
                   :token token
                   :current-user (js->clj (js/JSON.parse user-str) :keywordize-keys true))
            (fetch-trainings)))))))

(defn login [username password on-success]
  (api/post-json "/api/auth/login" {:username username :password password} {}
    (fn [{:keys [token user]}]
      (swap! *app-state assoc :logged-in? true :token token :current-user user :error nil)
      (save-token! token user)
      (fetch-trainings)
      (when on-success (on-success)))
    (err-handler "Invalid credentials")))

(defn logout []
  (clear-token!)
  (swap! *app-state assoc
         :logged-in? false :token nil :current-user nil
         :view :trainings :trainings [] :selected-training nil :sessions []))

;; ---------------------------------------------------------------------------
;; trainings

(defn fetch-trainings []
  (let [search (:search @*app-state)
        url (if (seq search) (str "/api/trainings?search=" (js/encodeURIComponent search)) "/api/trainings")]
    (api/fetch-json url (auth-headers)
      (fn [trainings] (swap! *app-state assoc :trainings (vec trainings))))))

(defn set-search [s]
  (swap! *app-state assoc :search s)
  (fetch-trainings))

(defn add-training [name description on-success]
  (api/post-json "/api/trainings" {:name name :description (or description "")} (auth-headers)
    (fn [_] (fetch-trainings) (when on-success (on-success)))
    (err-handler "Could not add training")))

(defn update-training [id fields on-success]
  (api/put-json (str "/api/trainings/" id) fields (auth-headers)
    (fn [_] (fetch-trainings) (when on-success (on-success)))
    (err-handler "Could not update training")))

(defn open-edit-training [training]
  (swap! *app-state assoc :editing-training training))

(defn close-edit-training []
  (swap! *app-state assoc :editing-training nil))

(defn delete-training [id]
  (api/delete-simple (str "/api/trainings/" id) (auth-headers)
    (fn [_] (fetch-trainings))
    (err-handler "Could not delete training")))

;; ---------------------------------------------------------------------------
;; navigation + sessions

(declare fetch-sessions)

(defn open-training [training]
  (swap! *app-state assoc :view :training :selected-training training :sessions [])
  (fetch-sessions (:id training)))

(defn back-to-trainings []
  (swap! *app-state assoc :view :trainings :selected-training nil :sessions [])
  (fetch-trainings))

;; ---------------------------------------------------------------------------
;; overview (read-only, all trainings)

(defn fetch-all-sessions []
  (api/fetch-json "/api/sessions" (auth-headers)
    (fn [sessions] (swap! *app-state assoc :all-sessions (vec sessions)))))

(defn show-overview []
  (swap! *app-state assoc :view :overview :selected-training nil)
  (fetch-all-sessions))

(defn show-trainings []
  (swap! *app-state assoc :view :trainings :selected-training nil)
  (fetch-trainings))

(defn fetch-sessions [training-id]
  (api/fetch-json (str "/api/trainings/" training-id "/sessions") (auth-headers)
    (fn [sessions] (swap! *app-state assoc :sessions (vec sessions)))))

(defn- current-training-id []
  (get-in @*app-state [:selected-training :id]))

(defn add-session [date notes on-success]
  (api/post-json (str "/api/trainings/" (current-training-id) "/sessions")
                 {:date date :notes (or notes "")} (auth-headers)
    (fn [_] (fetch-sessions (current-training-id)) (when on-success (on-success)))
    (err-handler "Could not add session")))

(defn update-session [id fields on-success]
  (api/put-json (str "/api/sessions/" id) fields (auth-headers)
    (fn [_] (fetch-sessions (current-training-id)) (when on-success (on-success)))
    (err-handler "Could not update session")))

(defn delete-session [id]
  (api/delete-simple (str "/api/sessions/" id) (auth-headers)
    (fn [_] (fetch-sessions (current-training-id)))
    (err-handler "Could not delete session")))
