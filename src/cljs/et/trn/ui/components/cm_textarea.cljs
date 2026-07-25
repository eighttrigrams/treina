(ns et.trn.ui.components.cm-textarea
  "Multi-line text input backed by CodeMirror so it picks up treina's keyboard
  scheme (see et.trn.ui.codemirror). Takes `value` as a reagent atom: the doc is
  seeded from it at mount and `on-change` pushes every edit back, so the atom
  stays the single source of truth for saving."
  (:require [reagent.core :as r]
            [et.trn.ui.codemirror :as cm]))

(defn cm-textarea [{:keys [value on-change]}]
  (let [editor-view (atom nil)
        container-el (atom nil)]
    (r/create-class
     {:display-name "cm-textarea"

      :component-did-mount
      (fn [_]
        (when-let [el @container-el]
          (let [view (cm/create-editor el {:doc (or @value "")
                                           :on-change (fn [text]
                                                        (when on-change
                                                          (on-change text)))})]
            (reset! editor-view view)
            (.focus view))))

      :component-will-unmount
      (fn [_]
        (when-let [view @editor-view]
          (.destroy view)
          (reset! editor-view nil)))

      :reagent-render
      (fn [{:keys [class]}]
        [:div {:class (or class "cm-host")
               :ref #(when % (reset! container-el %))}])})))
