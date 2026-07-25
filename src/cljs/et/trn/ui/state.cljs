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
           :program nil          ;; {:id :text :modified_at}
           :program-versions []  ;; history, newest first (newest = current)
           :program-modal nil    ;; nil | :edit | :history
           :search ""

           ;; youtube
           :yt-videos []
           :yt-channels []
           :yt-shelf :inbox      ;; :inbox | :archive
           :yt-config? false     ;; the channels subpage
           :yt-flag-filter nil   ;; nil | "cool" | "supercool"
           :yt-channel-filter nil     ;; channel_id shown exclusively
           :yt-excluded-channels #{}  ;; channel_ids filtered out
           :yt-polling? false}))

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

;; ---------------------------------------------------------------------------
;; program — one text document, every save kept as a version

(defn fetch-program []
  (api/fetch-json "/api/program" (auth-headers)
    (fn [program] (swap! *app-state assoc :program program))))

(defn fetch-program-versions []
  (api/fetch-json "/api/program/history" (auth-headers)
    (fn [{:keys [versions]}] (swap! *app-state assoc :program-versions (vec versions)))))

(defn show-program []
  (swap! *app-state assoc :view :program :selected-training nil)
  (fetch-program))

(defn save-program [text on-success]
  (api/put-json "/api/program"
                {:text text :modified_at (get-in @*app-state [:program :modified_at])}
                (auth-headers)
    (fn [program]
      (swap! *app-state assoc :program program)
      (when on-success (on-success)))
    (err-handler "Could not save the program")))

(defn open-program-modal [mode]
  (swap! *app-state assoc :program-modal mode)
  (when (= :history mode) (fetch-program-versions)))

(defn close-program-modal []
  (swap! *app-state assoc :program-modal nil))

;; ---------------------------------------------------------------------------
;; youtube — channels are polled server-side; the UI only shelves and filters

(defn fetch-yt-videos []
  (let [archived? (= :archive (:yt-shelf @*app-state))]
    (api/fetch-json (str "/api/youtube/videos" (when archived? "?archived=true"))
                    (auth-headers)
      (fn [videos] (swap! *app-state assoc :yt-videos (vec videos))))))

(defn fetch-yt-channels []
  (api/fetch-json "/api/youtube/channels" (auth-headers)
    (fn [channels] (swap! *app-state assoc :yt-channels (vec channels)))))

(defn show-youtube []
  (swap! *app-state assoc :view :youtube :selected-training nil)
  (fetch-yt-videos)
  (fetch-yt-channels))

(defn set-yt-shelf [shelf]
  (swap! *app-state assoc :yt-shelf shelf)
  (fetch-yt-videos))

(defn show-yt-config [show?]
  (swap! *app-state assoc :yt-config? show?)
  (when show? (fetch-yt-channels)))

(defn add-yt-channel [input on-success]
  (api/post-json "/api/youtube/channels" {:input input} (auth-headers)
    (fn [_] (fetch-yt-channels) (when on-success (on-success)))
    (err-handler "Could not resolve that channel")))

(defn delete-yt-channel [id]
  (api/delete-simple (str "/api/youtube/channels/" id) (auth-headers)
    (fn [_] (fetch-yt-channels) (fetch-yt-videos))
    (err-handler "Could not remove the channel")))

(defn poll-yt-now []
  (swap! *app-state assoc :yt-polling? true)
  (api/post-json "/api/youtube/poll" {} (auth-headers)
    (fn [_]
      (swap! *app-state assoc :yt-polling? false)
      (fetch-yt-videos)
      (fetch-yt-channels))
    (fn [resp]
      (swap! *app-state assoc :yt-polling? false)
      (set-error (get-in resp [:response :error] "Polling failed")))))

(defn- update-yt-video [video fields]
  (api/put-json (str "/api/youtube/videos/" (:id video))
                (assoc fields :modified_at (:modified_at video))
                (auth-headers)
    (fn [_] (fetch-yt-videos))
    (err-handler "Could not update the video")))

(defn set-yt-flag
  "Cycle a flag on or off: setting the flag it already has clears it."
  [video flag]
  (update-yt-video video {:flag (when (not= flag (:flag video)) flag)}))

(defn set-yt-archived [video archived?]
  (update-yt-video video {:archived archived?}))

(defn set-yt-flag-filter [flag]
  (swap! *app-state assoc :yt-flag-filter flag))

(defn set-yt-channel-filter [channel-id]
  (swap! *app-state assoc :yt-channel-filter channel-id :yt-excluded-channels #{}))

(defn clear-yt-channel-filter []
  (swap! *app-state assoc :yt-channel-filter nil))

(defn toggle-yt-excluded-channel [channel-id]
  (swap! *app-state update :yt-excluded-channels
         (fn [excluded]
           (if (contains? excluded channel-id)
             (disj excluded channel-id)
             (conj excluded channel-id)))))

(defn clear-yt-excluded-channel [channel-id]
  (swap! *app-state update :yt-excluded-channels disj channel-id))

(defn clear-yt-channel-filters []
  (swap! *app-state assoc :yt-channel-filter nil :yt-excluded-channels #{}))

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
