(ns et.trn.ui.modals
  "Modal editing, as in tracker: a card's body opens the edit modal, ⌘9 saves,
  Escape closes. Rendered once at the app root and driven from app state."
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [et.trn.ui.keys :as keys]
            [et.trn.ui.state :as state]
            [et.trn.ui.components.cm-textarea :refer [cm-textarea]]
            [et.trn.ui.components.diff :refer [diff-browser]]))

(defn modal-keys
  "Shortcuts for an open modal: ⌘9 confirms, Escape closes. Listens on the
  document because the focus normally sits inside the modal's CodeMirror."
  [_props]
  (let [props (atom nil)
        handler (fn [e]
                  (let [{:keys [on-confirm on-cancel enabled?]} @props]
                    (cond
                      (and enabled? (keys/save-key? e))
                      (do (.preventDefault e) (on-confirm))

                      (= "Escape" (.-code e))
                      (do (.preventDefault e) (on-cancel)))))]
    (r/create-class
     {:display-name "modal-keys"
      :component-did-mount (fn [this]
                             (reset! props (second (r/argv this)))
                             (.addEventListener js/document "keydown" handler))
      :component-did-update (fn [this _] (reset! props (second (r/argv this))))
      :component-will-unmount (fn [_] (.removeEventListener js/document "keydown" handler))
      :reagent-render (fn [_props] nil)})))

(defn edit-training-modal
  "Name + description of one training. Mounted keyed by training id, so the
  local atoms are always seeded from the training being edited."
  [training]
  (let [name (r/atom (:name training))
        desc (r/atom (or (:description training) ""))]
    (fn [training]
      (let [valid? (not (str/blank? @name))
            cancel! state/close-edit-training
            save! (fn []
                    (when valid?
                      (state/update-training (:id training)
                                             {:name (str/trim @name)
                                              :description @desc
                                              :modified_at (:modified_at training)}
                                             state/close-edit-training)))]
        [:div.modal-overlay {:on-click cancel!}
         [modal-keys {:on-confirm save! :on-cancel cancel! :enabled? valid?}]
         [:div.modal.edit-modal {:on-click #(.stopPropagation %)}
          [:div.modal-header "Edit training"]
          [:div.modal-body
           [:div.item-edit-form
            [:input {:type "text" :auto-complete "off" :placeholder "Name" :value @name
                     :on-change #(reset! name (-> % .-target .-value))}]
            [cm-textarea {:value desc
                          :on-change #(reset! desc %)
                          :class "cm-host cm-host-modal"}]]]
          [:div.modal-footer
           [:span.key-hint "⌘9 save · esc close"]
           [:button.cancel {:on-click cancel!} "Cancel"]
           [:button.confirm {:on-click save! :disabled (not valid?)} "Save"]]]]))))

(defn- program-modal
  "Two views of the program: the editor, and the history as diffs between
  consecutive versions. Saving from here creates the next version."
  [initial-text]
  (let [text (r/atom (or initial-text ""))]
    (fn [_initial-text]
      (let [mode (:program-modal @state/*app-state)
            versions (:program-versions @state/*app-state)
            cancel! state/close-program-modal
            save! #(state/save-program @text state/close-program-modal)]
        [:div.modal-overlay {:on-click cancel!}
         [modal-keys {:on-confirm save! :on-cancel cancel! :enabled? (= :edit mode)}]
         [:div.modal.program-modal {:on-click #(.stopPropagation %)}
          [:div.modal-header
           [:span "Program"]
           [:div.modal-tabs
            ;; .edit-tab so the phone layout can drop it — editing isn't offered
            ;; there (css/mobile.css).
            [:button.tab.edit-tab {:class (when (= :edit mode) "active")
                                   :on-click #(state/open-program-modal :edit)} "Edit"]
            [:button.tab {:class (when (= :history mode) "active")
                          :on-click #(state/open-program-modal :history)} "History"]]]
          [:div.modal-body
           (if (= :history mode)
             [diff-browser versions]
             [cm-textarea {:value text
                           :on-change #(reset! text %)
                           :placeholder "Your training philosophy"
                           :class "cm-host cm-host-program"}])]
          [:div.modal-footer
           (when (= :edit mode) [:span.key-hint "⌘9 save · esc close"])
           [:button.cancel {:on-click cancel!} (if (= :history mode) "Close" "Cancel")]
           (when (= :edit mode)
             [:button.confirm {:on-click save!} "Save"])]]]))))

(defn modals []
  (let [{:keys [editing-training program]} @state/*app-state
        mode (:program-modal @state/*app-state)]
    [:<>
     (when editing-training
       ^{:key (:id editing-training)} [edit-training-modal editing-training])
     ;; Keyed on modified_at so a save re-seeds the editor from what the server
     ;; stored, and re-opening never shows a stale draft.
     (when mode
       ^{:key (:modified_at program)} [program-modal (:text program)])]))
