(ns et.trn.ui.views.trainings
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [et.trn.ui.state :as state]))

(defn- add-training-form []
  (let [name (r/atom "")
        desc (r/atom "")]
    (fn []
      (let [submit (fn []
                     (when (seq (str/trim @name))
                       (state/add-training @name @desc
                                           (fn [] (reset! name "") (reset! desc "")))))]
        [:div.add-form
         [:input {:type "text"
                  :placeholder "New training name"
                  :value @name
                  :on-change #(reset! name (-> % .-target .-value))
                  :on-key-down #(when (= (.-key %) "Enter") (submit))}]
         [:input {:type "text"
                  :placeholder "Description (optional)"
                  :value @desc
                  :on-change #(reset! desc (-> % .-target .-value))
                  :on-key-down #(when (= (.-key %) "Enter") (submit))}]
         [:button {:on-click submit} "Add"]]))))

(defn- training-card [training]
  (let [editing? (r/atom false)
        name (r/atom (:name training))
        desc (r/atom (:description training))]
    (fn [training]
      (if @editing?
        [:div.card.editing
         [:input {:type "text" :value @name
                  :on-change #(reset! name (-> % .-target .-value))}]
         [:input {:type "text" :placeholder "Description" :value @desc
                  :on-change #(reset! desc (-> % .-target .-value))}]
         [:div.card-actions
          [:button {:on-click #(state/update-training
                                (:id training)
                                {:name @name :description @desc :modified_at (:modified_at training)}
                                (fn [] (reset! editing? false)))} "Save"]
          [:button.secondary {:on-click #(reset! editing? false)} "Cancel"]]]
        [:div.card
         [:div.card-body {:on-click #(state/open-training training)}
          [:h3 (:name training)]
          (when (seq (:description training))
            [:p.desc (:description training)])]
         [:div.card-actions
          [:button.secondary {:on-click #(state/open-training training)} "Open"]
          [:button.secondary {:on-click #(do (reset! name (:name training))
                                             (reset! desc (:description training))
                                             (reset! editing? true))} "Edit"]
          [:button.danger {:on-click #(when (js/confirm (str "Delete training \"" (:name training)
                                                             "\"? Its sessions are removed too."))
                                        (state/delete-training (:id training)))} "Delete"]]]))))

(defn trainings-tab []
  (let [{:keys [trainings search]} @state/*app-state]
    [:div.main-content
     [:div.page-head
      [:h1 "Trainings"]
      [:p.tagline "Name a training, then step in to log its sessions."]]
     [:input.search {:type "text"
                     :placeholder "Search trainings…"
                     :value search
                     :on-change #(state/set-search (-> % .-target .-value))}]
     [add-training-form]
     (if (empty? trainings)
       [:p.empty "No trainings yet — add your first above."]
       [:div.card-list
        (for [t trainings]
          ^{:key (:id t)} [training-card t])])]))
