(ns et.trn.ui.views.sessions
  (:require [reagent.core :as r]
            [et.trn.ui.state :as state]
            ["marked" :refer [marked]]))

(defn markdown [text]
  [:div.markdown-content
   {:dangerouslySetInnerHTML (r/unsafe-html (marked (or text "")))}])

(defn- submit-key? [e]
  (and (= (.-key e) "Enter") (or (.-metaKey e) (.-ctrlKey e))))

(defn- add-session-form []
  (let [date (r/atom (state/today-str))
        notes (r/atom "")]
    (fn []
      (let [submit (fn []
                     (when (seq @date)
                       (state/add-session @date @notes (fn [] (reset! notes "")))))]
        [:div.note-add
         [:div.note-add-head
          [:input {:type "date"
                   :value @date
                   :on-change #(reset! date (-> % .-target .-value))}]
          [:button {:on-click submit} "Add session"]]
         [:textarea.note-editor
          {:placeholder "Notes — markdown supported (⌘↵ to add)"
           :rows 3
           :value @notes
           :on-change #(reset! notes (-> % .-target .-value))
           :on-key-down #(when (submit-key? %) (submit))}]]))))

(defn- session-note [session]
  (let [editing? (r/atom false)
        date (r/atom (:date session))
        notes (r/atom (:notes session))
        start-edit (fn [s]
                     (reset! date (:date s))
                     (reset! notes (:notes s))
                     (reset! editing? true))]
    (fn [session]
      (if @editing?
        (let [save #(state/update-session
                     (:id session)
                     {:date @date :notes @notes :modified_at (:modified_at session)}
                     (fn [] (reset! editing? false)))]
          [:div.note.editing
           [:div.note-head
            [:input {:type "date"
                     :value @date
                     :on-change #(reset! date (-> % .-target .-value))}]
            [:div.note-actions
             [:button {:on-click save} "Save"]
             [:button.secondary {:on-click #(reset! editing? false)} "Cancel"]]]
           [:textarea.note-editor
            {:auto-focus true
             :rows 8
             :value @notes
             :on-change #(reset! notes (-> % .-target .-value))
             :on-key-down #(when (submit-key? %) (save))}]])
        [:div.note
         [:div.note-head
          [:span.note-date (:date session)]
          [:div.note-actions
           [:button.icon-btn {:title "Edit" :on-click #(start-edit session)} "✎"]
           [:button.icon-btn.danger {:title "Delete"
                                     :on-click #(when (js/confirm "Delete this session?")
                                                  (state/delete-session (:id session)))} "×"]]]
         (if (seq (:notes session))
           [:div.note-body {:on-click #(start-edit session)}
            [markdown (:notes session)]]
           [:button.note-placeholder {:on-click #(start-edit session)} "✎ add notes"])]))))

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
       [:div.notes
        (for [s sessions]
          ^{:key (:id s)} [session-note s])])]))
