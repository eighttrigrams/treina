(ns et.trn.ui.modals
  "Modal editing, as in tracker: a card's body opens the edit modal, ⌘9 saves,
  Escape closes. Rendered once at the app root and driven from app state."
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [et.trn.ui.state :as state]
            [et.trn.ui.components.cm-textarea :refer [cm-textarea]]))

(defn modal-keys
  "Shortcuts for an open modal: ⌘9 confirms, Escape closes. Listens on the
  document because the focus normally sits inside the modal's CodeMirror."
  [_props]
  (let [props (atom nil)
        handler (fn [e]
                  (let [{:keys [on-confirm on-cancel enabled?]} @props]
                    (cond
                      (and enabled? (.-metaKey e) (= "Digit9" (.-code e)))
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

(defn modals []
  (when-let [training (:editing-training @state/*app-state)]
    ^{:key (:id training)} [edit-training-modal training]))
