(ns et.trn.ui.views.program
  "The program: one text document holding the training philosophy. Rendered as a
  page of prose; clicking it opens the modal where it is edited and where the
  history can be stepped through as diffs."
  (:require [et.trn.ui.markdown :as markdown]
            [et.trn.ui.state :as state]))

(defn program-tab []
  (let [{:keys [program]} @state/*app-state
        text (:text program)]
    [:div.main-content
     [:div.page-head
      [:h1 "Program"]
      [:p.tagline "The training philosophy. Every save keeps the previous state."]]
     [:div.program-page {:on-click #(state/open-program-modal :edit)
                         :title "Click to edit"}
      (if (seq text)
        [markdown/render text]
        [:p.empty "Nothing written yet — click here to start."])]
     [:div.program-foot
      [:button.secondary {:on-click #(state/open-program-modal :history)} "History"]]]))
