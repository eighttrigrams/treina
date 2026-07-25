(ns et.trn.ui.views.sessions
  (:require [reagent.core :as r]
            [et.trn.ui.state :as state]))

(defn- add-session-form []
  (let [date (r/atom (state/today-str))
        notes (r/atom "")]
    (fn []
      (let [submit (fn []
                     (when (seq @date)
                       (state/add-session @date @notes (fn [] (reset! notes "")))))]
        [:div.add-form
         [:input {:type "date"
                  :value @date
                  :on-change #(reset! date (-> % .-target .-value))}]
         [:input {:type "text"
                  :placeholder "Notes (optional)"
                  :value @notes
                  :on-change #(reset! notes (-> % .-target .-value))
                  :on-key-down #(when (= (.-key %) "Enter") (submit))}]
         [:button {:on-click submit} "Add session"]]))))

(defn- session-row [session]
  (let [editing? (r/atom false)
        date (r/atom (:date session))
        notes (r/atom (:notes session))]
    (fn [session]
      (if @editing?
        [:div.session.editing
         [:input {:type "date" :value @date
                  :on-change #(reset! date (-> % .-target .-value))}]
         [:input {:type "text" :placeholder "Notes" :value @notes
                  :on-change #(reset! notes (-> % .-target .-value))}]
         [:div.card-actions
          [:button {:on-click #(state/update-session
                                (:id session)
                                {:date @date :notes @notes :modified_at (:modified_at session)}
                                (fn [] (reset! editing? false)))} "Save"]
          [:button.secondary {:on-click #(reset! editing? false)} "Cancel"]]]
        [:div.session
         [:div.session-body
          [:span.session-date (:date session)]
          (when (seq (:notes session))
            [:span.session-notes (:notes session)])]
         [:div.card-actions
          [:button.secondary {:on-click #(do (reset! date (:date session))
                                             (reset! notes (:notes session))
                                             (reset! editing? true))} "Edit"]
          [:button.danger {:on-click #(when (js/confirm "Delete this session?")
                                        (state/delete-session (:id session)))} "Delete"]]]))))

(defn sessions-tab []
  (let [{:keys [selected-training sessions]} @state/*app-state]
    [:div.main-content
     [:div.detail-head
      [:button.secondary.back-btn {:on-click state/back-to-trainings} "← Trainings"]
      [:h1 (:name selected-training)]
      (when (seq (:description selected-training))
        [:p.desc (:description selected-training)])]
     [add-session-form]
     (if (empty? sessions)
       [:p.empty "No sessions yet — log your first above."]
       [:div.session-list
        (for [s sessions]
          ^{:key (:id s)} [session-row s])])]))
