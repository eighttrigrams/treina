(ns et.trn.ui.views.trainings
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [et.trn.ui.markdown :as markdown]
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

(defn- inline-title-edit
  "Tracker's inline rename: commits on Enter or blur, Escape drops it."
  [training on-done]
  (let [value (r/atom (:name training))]
    (fn [training _]
      (let [commit (fn []
                     (let [v (str/trim @value)]
                       (if (and (seq v) (not= v (:name training)))
                         (state/update-training (:id training)
                                                {:name v
                                                 :modified_at (:modified_at training)}
                                                on-done)
                         (on-done))))]
        [:input.inline-title-edit
         {:type "text" :auto-complete "off" :auto-focus true :value @value
          :on-click #(.stopPropagation %)
          :on-change #(reset! value (-> % .-target .-value))
          :on-key-down (fn [e]
                         (case (.-key e)
                           "Enter" (do (.stopPropagation e) (commit))
                           "Escape" (do (.stopPropagation e) (on-done))
                           nil))
          :on-blur (fn [_] (commit))}]))))

(defn- training-card
  "Card behaves like tracker's: the header toggles open/closed, a closed card
  carries the jump-to-sessions icon, ⌥-clicking the title renames it in place,
  and the description (or the pencil, when empty) opens the edit modal. Delete
  lives in the opened card's footer."
  [_training]
  (let [expanded? (r/atom false)
        renaming? (r/atom false)]
    (fn [training]
      [:div.card {:class (when @expanded? "expanded")}
       [:div.card-header {:on-click #(swap! expanded? not)}
        (if @renaming?
          [inline-title-edit training #(reset! renaming? false)]
          [:span.card-title
           {:title "⌥-click to rename"
            :on-click (fn [e]
                        (when (.-altKey e)
                          (.stopPropagation e)
                          (reset! renaming? true)))}
           (:name training)])
        (when-not @expanded?
          [:button.icon-btn.jump-btn
           {:title "Sessions"
            :on-click (fn [e]
                        (.stopPropagation e)
                        (state/open-training training))}
           "▸"])]
       (when @expanded?
         [:div.card-details
          (if (seq (:description training))
            [:div.desc {:on-click #(state/open-edit-training training)}
             [markdown/render (:description training)]]
            [:button.edit-icon {:on-click #(state/open-edit-training training)} "✎"])
          [:div.card-footer
           [:button.danger
            {:on-click #(when (js/confirm (str "Delete training \"" (:name training)
                                               "\"? Its sessions are removed too."))
                          (state/delete-training (:id training)))}
            "Delete"]]])])))

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
