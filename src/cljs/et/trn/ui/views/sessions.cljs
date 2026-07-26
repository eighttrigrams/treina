(ns et.trn.ui.views.sessions
  "One training's sessions: the head repeats the training's name and description —
  clicking that description opens the same edit modal the trainings list uses —
  then the always-open editor for a new session, then the sessions themselves."
  (:require [reagent.core :as r]
            [et.trn.ui.markdown :as markdown]
            [et.trn.ui.state :as state]
            [et.trn.ui.components.cm-textarea :refer [cm-textarea]]))

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

(defn- entity-select
  "An optional reference on a session — its place, its trainer. Empty option
  means none. Hidden entirely while that list is empty, so the form stays as it
  was for anyone who doesn't use them."
  [items none-label value on-change]
  (when (seq items)
    [:select.place-select
     {:value (or value "")
      :on-change #(let [v (-> % .-target .-value)]
                    (on-change (when (seq v) (js/parseInt v))))}
     [:option {:value ""} none-label]
     (for [i items]
       ^{:key (:id i)} [:option {:value (:id i)} (:name i)])]))

(defn- place-select [value on-change]
  [entity-select (:places @state/*app-state) "— no place —" value on-change])

(defn- trainer-select [value on-change]
  [entity-select (:trainers @state/*app-state) "— no trainer —" value on-change])

(defn- add-session-form
  "The editor is always open so a new session can be typed straight away. ⌘9 is
  scoped to this block (not the document) so it can't collide with the save
  chord of a session being edited further down the page. The chosen place and
  trainer stick across adds — training usually repeats in the same place, with
  the same person."
  []
  (let [date (r/atom (state/today-str))
        notes (r/atom "")
        place-id (r/atom nil)
        trainer-id (r/atom nil)
        ;; Bumped after every add: the editor seeds its doc at mount, so a fresh
        ;; key is what empties it again.
        generation (r/atom 0)]
    (fn []
      (let [submit (fn []
                     (when (seq @date)
                       (state/add-session @date @notes @place-id @trainer-id
                                          (fn [] (reset! notes "") (swap! generation inc)))))]
        [:div.note-add {:on-key-down #(when (save-key? %) (.preventDefault %) (submit))}
         [:div.note-add-head
          [:input {:type "date"
                   :value @date
                   :on-change #(reset! date (-> % .-target .-value))}]
          [place-select @place-id #(reset! place-id %)]
          [trainer-select @trainer-id #(reset! trainer-id %)]
          [:button {:on-click submit} "Add session"]
          [:span.key-hint "⌘9"]]
         ^{:key @generation}
         [cm-textarea {:value notes
                       :on-change #(reset! notes %)
                       :placeholder "Training notes"
                       ;; No focus grab when the page opens, but back into the
                       ;; editor after an add.
                       :focus? (pos? @generation)
                       :class "cm-host cm-host-add"}]]))))

(defn- session-note [session]
  (let [editing? (r/atom false)
        date (r/atom (:date session))
        notes (r/atom (:notes session))
        place-id (r/atom (:place_id session))
        trainer-id (r/atom (:trainer_id session))
        start-edit (fn [s]
                     (when-not (state/narrow-viewport?)
                       (reset! date (:date s))
                       (reset! notes (:notes s))
                       (reset! place-id (:place_id s))
                       (reset! trainer-id (:trainer_id s))
                       (reset! editing? true)))]
    (fn [session]
      (if @editing?
        (let [save #(state/update-session
                     (:id session)
                     {:date @date :notes @notes :place_id @place-id
                      :trainer_id @trainer-id
                      :modified_at (:modified_at session)}
                     (fn [] (reset! editing? false)))]
          [:div.note.editing
           [on-save-key save]
           [:div.note-head
            [:input {:type "date"
                     :value @date
                     :on-change #(reset! date (-> % .-target .-value))}]
            [place-select @place-id #(reset! place-id %)]
            [trainer-select @trainer-id #(reset! trainer-id %)]
            [:div.note-actions
             [:span.key-hint "⌘9"]
             [:button {:on-click save} "Save"]
             [:button.secondary {:on-click #(reset! editing? false)} "Cancel"]]]
           [cm-textarea {:value notes :on-change #(reset! notes %)}]])
        [:div.note
         [:div.note-head
          [:span.note-date (:date session)]
          (when-let [place (:place_name session)]
            [:span.note-place place])
          (when-let [trainer (:trainer_name session)]
            [:span.note-trainer trainer])
          [:div.note-actions
           [:button.icon-btn {:title "Edit" :on-click #(start-edit session)} "✎"]
           [:button.icon-btn.danger {:title "Delete"
                                     :on-click #(when (js/confirm "Delete this session?")
                                                  (state/delete-session (:id session)))} "×"]]]
         (if (seq (:notes session))
           [:div.note-body {:on-click #(start-edit session)}
            [markdown/render (:notes session)]]
           [:button.note-placeholder {:on-click #(start-edit session)} "✎ add notes"])]))))

(defn sessions-tab []
  (let [{:keys [selected-training sessions]} @state/*app-state]
    [:div.main-content
     [:div.detail-head
      [:button.secondary.back-btn {:on-click state/back-to-trainings} "← Trainings"]
      [:h1 (:name selected-training)]
      (if (seq (:description selected-training))
        [:div.desc.editable {:title "Click to edit"
                             :on-click #(state/open-edit-training selected-training)}
         [markdown/render (:description selected-training)]]
        [:button.edit-icon {:on-click #(state/open-edit-training selected-training)} "✎"])]
     [add-session-form]
     (if (empty? sessions)
       [:p.empty "No sessions yet — log your first above."]
       [:div.notes
        (for [s sessions]
          ^{:key (:id s)} [session-note s])])]))
