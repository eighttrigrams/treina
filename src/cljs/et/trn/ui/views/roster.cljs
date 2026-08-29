(ns et.trn.ui.views.roster
  "The page shape shared by places and trainers: a small named list a session can
  point at. Cards in the trainings idiom — the header opens the card, the
  description renders as markdown, and editing happens in place rather than in a
  modal. A card says how many sessions reference it, and deleting one leaves
  those sessions standing, just without the reference.

  Both pages differ only in their labels and in which state fns they call, so
  they pass those in rather than each keeping a copy of this."
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [et.trn.ui.keys :as keys]
            [et.trn.ui.markdown :as markdown]
            [et.trn.ui.state :as state]))

(defn- add-form [{:keys [name-placeholder on-add]}]
  (let [name (r/atom "")
        description (r/atom "")]
    (fn [_]
      (let [submit (fn []
                     (when (seq (str/trim @name))
                       (on-add (str/trim @name) @description
                               (fn [] (reset! name "") (reset! description "")))))]
        [:div.add-form {:on-key-down (keys/on-save submit)}
         [:input {:type "text"
                  :placeholder name-placeholder
                  :value @name
                  :on-change #(reset! name (-> % .-target .-value))
                  :on-key-down #(when (= "Enter" (.-key %)) (submit))}]
         [:input {:type "text"
                  :placeholder "Description (optional)"
                  :value @description
                  :on-change #(reset! description (-> % .-target .-value))
                  :on-key-down #(when (= "Enter" (.-key %)) (submit))}]
         [:button {:on-click submit} "Add"]
         [:span.key-hint "⌘9"]]))))

(defn- session-count-label [n]
  (case n
    0 "no sessions yet"
    1 "1 session"
    (str n " sessions")))

(defn- entry-card [_entry _opts]
  (let [expanded? (r/atom false)
        editing? (r/atom false)
        name (r/atom "")
        description (r/atom "")
        start-edit (fn [entry]
                     (reset! name (:name entry))
                     (reset! description (:description entry))
                     (reset! editing? true))]
    (fn [entry {:keys [on-update on-delete delete-prompt]}]
      [:div.card {:class (when @expanded? "expanded")}
       [:div.card-header {:on-click #(swap! expanded? not)}
        [:span.card-title (:name entry)]
        [:span.card-date (session-count-label (:session_count entry))]]
       (when @expanded?
         [:div.card-details
          (if @editing?
            (let [save #(when (seq (str/trim @name))
                          (on-update entry
                                     {:name (str/trim @name) :description @description}
                                     (fn [] (reset! editing? false))))]
              [:div.place-edit {:on-key-down (keys/on-save save)}
               [:input {:type "text"
                        :value @name
                        :on-change #(reset! name (-> % .-target .-value))
                        :on-key-down #(when (= "Enter" (.-key %)) (save))}]
               ;; The name above keeps Enter-saves — it is one line by nature.
               ;; This one cannot: the field renders as markdown, and a
               ;; paragraph needs its newline. ⌘9 on the block saves it.
               [:textarea {:placeholder "Description"
                           :rows 5
                           :value @description
                           :on-change #(reset! description (-> % .-target .-value))}]
               [:div.card-footer
                [:span.key-hint "⌘9"]
                [:button {:on-click save} "Save"]
                [:button.secondary {:on-click #(reset! editing? false)} "Cancel"]]])
            [:div
             (when (seq (:description entry))
               [:div.desc [markdown/render (:description entry)]])
             [:div.card-footer.split
              [:button.secondary {:on-click #(start-edit entry)} "Edit"]
              [:button.danger
               {:on-click #(when (js/confirm (delete-prompt entry))
                             (on-delete (:id entry)))}
               "Delete"]]])])])))

(defn roster-tab
  "One roster page. `opts`:
  :title :tagline :name-placeholder :empty-label — the words,
  :items-key — where the list sits in app state,
  :on-add :on-update :on-delete :delete-prompt — the state fns to drive."
  [{:keys [title tagline name-placeholder empty-label items-key] :as opts}]
  (let [entries (get @state/*app-state items-key)]
    [:div.main-content
     [:div.page-head
      [:h1 title]
      [:p.tagline tagline]]
     [add-form (assoc opts :name-placeholder name-placeholder)]
     (if (empty? entries)
       [:p.empty empty-label]
       [:div.card-list
        (for [e entries]
          ^{:key (:id e)} [entry-card e opts])])]))
