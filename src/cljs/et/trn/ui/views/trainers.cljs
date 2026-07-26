(ns et.trn.ui.views.trainers
  "People who took a session — a coach, a training partner. Same page as places
  (et.trn.ui.views.roster), different words."
  (:require [et.trn.ui.views.roster :as roster]
            [et.trn.ui.state :as state]))

(defn trainers-tab []
  [roster/roster-tab
   {:title "Trainers"
    :tagline "Who trains you. Assigning one to a session is optional."
    :name-placeholder "New trainer name"
    :empty-label "No trainers yet — add one above."
    :items-key :trainers
    :on-add state/add-trainer
    :on-update state/update-trainer
    :on-delete state/delete-trainer
    :delete-prompt (fn [trainer]
                     (str "Delete trainer \"" (:name trainer) "\"? Their sessions keep "
                          "their notes, they just lose the trainer."))}])
