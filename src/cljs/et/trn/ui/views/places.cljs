(ns et.trn.ui.views.places
  "Places a session can happen at — a gym, a park, home. Cards in the trainings
  idiom: the header opens the card, the description renders as markdown, and
  editing happens in place rather than in a modal. A place that has sessions says
  so, and deleting it leaves those sessions standing, just place-less."
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [et.trn.ui.markdown :as markdown]
            [et.trn.ui.state :as state]))

(defn- add-place-form []
  (let [name (r/atom "")
        description (r/atom "")]
    (fn []
      (let [submit (fn []
                     (when (seq (str/trim @name))
                       (state/add-place (str/trim @name) @description
                                        (fn [] (reset! name "") (reset! description "")))))]
        [:div.add-form
         [:input {:type "text"
                  :placeholder "New place name"
                  :value @name
                  :on-change #(reset! name (-> % .-target .-value))
                  :on-key-down #(when (= "Enter" (.-key %)) (submit))}]
         [:input {:type "text"
                  :placeholder "Description (optional)"
                  :value @description
                  :on-change #(reset! description (-> % .-target .-value))
                  :on-key-down #(when (= "Enter" (.-key %)) (submit))}]
         [:button {:on-click submit} "Add"]]))))

(defn- session-count-label [n]
  (case n
    0 "no sessions yet"
    1 "1 session"
    (str n " sessions")))

(defn- place-card [_place]
  (let [expanded? (r/atom false)
        editing? (r/atom false)
        name (r/atom "")
        description (r/atom "")
        start-edit (fn [place]
                     (reset! name (:name place))
                     (reset! description (:description place))
                     (reset! editing? true))]
    (fn [place]
      [:div.card {:class (when @expanded? "expanded")}
       [:div.card-header {:on-click #(swap! expanded? not)}
        [:span.card-title (:name place)]
        [:span.card-date (session-count-label (:session_count place))]]
       (when @expanded?
         [:div.card-details
          (if @editing?
            (let [save #(when (seq (str/trim @name))
                          (state/update-place place
                                              {:name (str/trim @name) :description @description}
                                              (fn [] (reset! editing? false))))]
              [:div.place-edit
               [:input {:type "text"
                        :value @name
                        :on-change #(reset! name (-> % .-target .-value))
                        :on-key-down #(when (= "Enter" (.-key %)) (save))}]
               [:input {:type "text"
                        :placeholder "Description"
                        :value @description
                        :on-change #(reset! description (-> % .-target .-value))
                        :on-key-down #(when (= "Enter" (.-key %)) (save))}]
               [:div.card-footer
                [:button {:on-click save} "Save"]
                [:button.secondary {:on-click #(reset! editing? false)} "Cancel"]]])
            [:div
             (when (seq (:description place))
               [:div.desc [markdown/render (:description place)]])
             [:div.card-footer.split
              [:button.secondary {:on-click #(start-edit place)} "Edit"]
              [:button.danger
               {:on-click #(when (js/confirm (str "Delete place \"" (:name place)
                                                  "\"? Sessions there keep their notes, "
                                                  "they just lose the place."))
                             (state/delete-place (:id place)))}
               "Delete"]]])])])))

(defn places-tab []
  (let [places (:places @state/*app-state)]
    [:div.main-content
     [:div.page-head
      [:h1 "Places"]
      [:p.tagline "Where you train. Assigning one to a session is optional."]]
     [add-place-form]
     (if (empty? places)
       [:p.empty "No places yet — add one above."]
       [:div.card-list
        (for [p places]
          ^{:key (:id p)} [place-card p])])]))
