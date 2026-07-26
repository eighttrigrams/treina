(ns et.trn.ui.views.places
  "Places a session can happen at — a gym, a park, home. The page itself is
  et.trn.ui.views.roster; this only supplies the words and the state fns."
  (:require [et.trn.ui.views.roster :as roster]
            [et.trn.ui.state :as state]))

(defn places-tab []
  [roster/roster-tab
   {:title "Places"
    :tagline "Where you train. Assigning one to a session is optional."
    :name-placeholder "New place name"
    :empty-label "No places yet — add one above."
    :items-key :places
    :on-add state/add-place
    :on-update state/update-place
    :on-delete state/delete-place
    :delete-prompt (fn [place]
                     (str "Delete place \"" (:name place) "\"? Sessions there keep "
                          "their notes, they just lose the place."))}])
