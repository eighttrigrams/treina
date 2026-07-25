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

(defn- training-edit [training on-done]
  (let [name (r/atom (:name training))
        desc (r/atom (or (:description training) ""))]
    (fn [training _]
      (let [save (fn []
                   (when (seq (str/trim @name))
                     (state/update-training
                      (:id training)
                      {:name (str/trim @name) :description @desc
                       :modified_at (:modified_at training)}
                      on-done)))]
        [:div.card-edit
         [:input {:type "text" :auto-complete "off" :value @name
                  :on-change #(reset! name (-> % .-target .-value))
                  :on-key-down #(case (.-key %)
                                  "Enter" (save)
                                  "Escape" (on-done)
                                  nil)}]
         [:input {:type "text" :auto-complete "off" :placeholder "Description" :value @desc
                  :on-change #(reset! desc (-> % .-target .-value))
                  :on-key-down #(case (.-key %)
                                  "Enter" (save)
                                  "Escape" (on-done)
                                  nil)}]
         [:div.card-edit-buttons
          [:button {:on-click save :disabled (str/blank? @name)} "Save"]
          [:button.secondary {:on-click on-done} "Cancel"]]]))))

(defn- training-card
  "Card behaves like tracker's: the header toggles open/closed, a closed card
  carries the jump-to-sessions icon, and description editing plus delete live
  inside the opened card."
  [_training]
  (let [expanded? (r/atom false)
        editing? (r/atom false)]
    (fn [training]
      [:div.card {:class (when @expanded? "expanded")}
       [:div.card-header {:on-click #(when-not @editing? (swap! expanded? not))}
        [:span.card-title (:name training)]
        (when-not @expanded?
          [:button.icon-btn.jump-btn
           {:title "Sessions"
            :on-click (fn [e]
                        (.stopPropagation e)
                        (state/open-training training))}
           "▸"])]
       (when @expanded?
         (if @editing?
           [training-edit training #(reset! editing? false)]
           [:div.card-details
            (if (seq (:description training))
              [:p.desc {:on-click #(reset! editing? true)} (:description training)]
              [:button.edit-icon {:on-click #(reset! editing? true)} "✎"])
            [:div.card-footer
             [:button.danger
              {:on-click #(when (js/confirm (str "Delete training \"" (:name training)
                                                 "\"? Its sessions are removed too."))
                            (state/delete-training (:id training)))}
              "Delete"]]]))])))

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
