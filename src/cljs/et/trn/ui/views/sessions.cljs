(ns et.trn.ui.views.sessions
  (:require [reagent.core :as r]
            [et.trn.ui.state :as state]
            [et.trn.ui.components.cm-textarea :refer [cm-textarea]]
            ["marked" :refer [marked]]))

(defn markdown [text]
  [:div.markdown-content
   {:dangerouslySetInnerHTML (r/unsafe-html (marked (or text "")))}])

(defn- save-key?
  "⌘9 — the scheme's save chord (see et.trn.ui.codemirror). Enter stays a plain
  newline inside the editor."
  [e]
  (and (.-metaKey e) (= "Digit9" (.-code e))))

(defn- on-save-key
  "Document-level listener, since the keystroke may land inside CodeMirror rather
  than on our own element."
  [save]
  (let [handler (fn [e] (when (save-key? e) (.preventDefault e) (save)))]
    (r/create-class
     {:display-name "save-key"
      :component-did-mount #(.addEventListener js/document "keydown" handler)
      :component-will-unmount #(.removeEventListener js/document "keydown" handler)
      :reagent-render (fn [_] nil)})))

(defn- add-session-form []
  (let [date (r/atom (state/today-str))
        notes (r/atom "")
        open? (r/atom false)]
    (fn []
      (let [submit (fn []
                     (when (seq @date)
                       (state/add-session @date @notes
                                          (fn [] (reset! notes "") (reset! open? false)))))]
        [:div.note-add
         [:div.note-add-head
          [:input {:type "date"
                   :value @date
                   :on-change #(reset! date (-> % .-target .-value))}]
          [:button {:on-click submit} "Add session"]
          [:span.key-hint "⌘9"]]
         (if @open?
           [:<>
            [on-save-key submit]
            [cm-textarea {:value notes :on-change #(reset! notes %) :class "cm-host cm-host-add"}]]
           [:button.note-placeholder {:on-click #(reset! open? true)}
            "✎ notes — markdown supported"])]))))

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
           [on-save-key save]
           [:div.note-head
            [:input {:type "date"
                     :value @date
                     :on-change #(reset! date (-> % .-target .-value))}]
            [:div.note-actions
             [:span.key-hint "⌘9"]
             [:button {:on-click save} "Save"]
             [:button.secondary {:on-click #(reset! editing? false)} "Cancel"]]]
           [cm-textarea {:value notes :on-change #(reset! notes %)}]])
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
