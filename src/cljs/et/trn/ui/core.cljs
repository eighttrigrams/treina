(ns et.trn.ui.core
  (:require [reagent.dom.client :as rdomc]
            [reagent.core :as r]
            [et.trn.ui.state :as state]
            [et.trn.ui.modals :as modals]
            [et.trn.ui.views.trainings :as trainings]
            [et.trn.ui.views.sessions :as sessions]
            [et.trn.ui.views.overview :as overview]
            [et.trn.ui.views.places :as places]
            [et.trn.ui.views.program :as program]
            [et.trn.ui.views.youtube :as youtube]))

(defn login-form []
  (let [username (r/atom "")
        password (r/atom "")]
    (fn []
      (let [do-login #(state/login @username @password
                                   (fn [] (reset! username "") (reset! password "")))]
        [:div.login-form
         [:h2 "Sign in to Treina"]
         (when-let [error (:error @state/*app-state)]
           [:div.error error [:button.error-dismiss {:on-click state/clear-error} "×"]])
         [:input {:type "text" :auto-complete "off" :placeholder "Username"
                  :value @username
                  :on-change #(reset! username (-> % .-target .-value))
                  :on-key-down #(when (= (.-key %) "Enter") (do-login))}]
         [:input {:type "password" :placeholder "Password"
                  :value @password
                  :on-change #(reset! password (-> % .-target .-value))
                  :on-key-down #(when (= (.-key %) "Enter") (do-login))}]
         [:button {:on-click do-login} "Sign in"]]))))

(defn- top-bar []
  (let [view (:view @state/*app-state)]
    [:div.top-bar
     [:div.brand
      [:span.brand-mark "◈"]
      [:span.brand-name "Treina"]]
     [:div.nav
      [:button.tab {:class (when (contains? #{:trainings :training} view) "active")
                    :on-click state/show-trainings} "Trainings"]
      [:button.tab {:class (when (= :overview view) "active")
                    :on-click state/show-overview} "All sessions"]
      [:button.tab {:class (when (= :places view) "active")
                    :on-click state/show-places} "Places"]
      [:button.tab {:class (when (= :program view) "active")
                    :on-click state/show-program} "Program"]
      [:button.tab {:class (when (= :youtube view) "active")
                    :on-click state/show-youtube} "YouTube"]]
     (when (:token @state/*app-state)
       [:button.secondary {:on-click state/logout} "Sign out"])]))

(defn app []
  (let [{:keys [auth-required? logged-in? view error]} @state/*app-state]
    (cond
      (nil? auth-required?)
      [:div.loading "Loading…"]

      (and auth-required? (not logged-in?))
      [login-form]

      :else
      [:div
       [top-bar]
       (when error
         [:div.error error [:button.error-dismiss {:on-click state/clear-error} "×"]])
       [:div.main-layout
        (case view
          :training [sessions/sessions-tab]
          :overview [overview/overview-tab]
          :places [places/places-tab]
          :program [program/program-tab]
          :youtube [youtube/youtube-tab]
          [trainings/trainings-tab])]
       [modals/modals]])))

(defonce root (rdomc/create-root (.getElementById js/document "app")))

(defn init []
  (state/fetch-auth-required)
  (rdomc/render root [app]))
