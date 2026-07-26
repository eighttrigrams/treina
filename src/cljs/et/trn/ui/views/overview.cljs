(ns et.trn.ui.views.overview
  "Read-only overview: a calendar of training days plus every session in one
  feed, newest first. Nothing here mutates — editing lives in the per-training
  view."
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [et.trn.ui.markdown :as markdown]
            [et.trn.ui.state :as state]))

;; ---------------------------------------------------------------------------
;; date helpers (plain js/Date — no extra dependency)

(defn- pad [n] (if (< n 10) (str "0" n) (str n)))

(defn- date-str [y m d] (str y "-" (pad (inc m)) "-" (pad d)))

(defn- days-in-month [y m]
  ;; day 0 of the next month is the last day of this one
  (.getDate (js/Date. y (inc m) 0)))

(defn- first-weekday
  "Index of the 1st within a Monday-first week (Mon=0 … Sun=6)."
  [y m]
  (mod (+ 6 (.getDay (js/Date. y m 1))) 7))

(def ^:private month-names
  ["January" "February" "March" "April" "May" "June"
   "July" "August" "September" "October" "November" "December"])

(def ^:private weekday-initials ["M" "T" "W" "T" "F" "S" "S"])

;; ---------------------------------------------------------------------------
;; calendar

(defn- calendar
  "Month grid. Days carrying sessions are highlighted; hovering one names the
  training(s) trained that day — the training, not the session."
  [sessions-by-date y m on-prev on-next]
  (let [total (days-in-month y m)
        lead (first-weekday y m)
        cells (concat (repeat lead nil) (range 1 (inc total)))]
    [:div.calendar
     [:div.calendar-head
      [:button.icon-btn {:on-click on-prev :title "Previous month"} "‹"]
      [:span.calendar-title (str (get month-names m) " " y)]
      [:button.icon-btn {:on-click on-next :title "Next month"} "›"]]
     [:div.calendar-grid
      (for [[i w] (map-indexed vector weekday-initials)]
        ^{:key (str "wd" i)} [:div.calendar-weekday w])
      (for [[i d] (map-indexed vector cells)]
        (if (nil? d)
          ^{:key (str "blank" i)} [:div.calendar-day.empty]
          (let [ds (date-str y m d)
                names (get sessions-by-date ds)]
            ^{:key ds}
            [:div.calendar-day {:class (when (seq names) "has-session")}
             d
             ;; Own tooltip rather than `title`: the native one is slow to appear
             ;; and can't be styled.
             (when (seq names)
               [:span.calendar-tip (str/join ", " names)])])))]]))

;; ---------------------------------------------------------------------------

(defn- session-entry [session]
  [:div.note.readonly
   [:div.note-head
    [:span.note-date (:date session)]
    [:span.note-training {:title "Open this training"
                          :on-click #(state/open-training-by-id (:training_id session)
                                                                (:training_name session))}
     (:training_name session)]
    (when-let [place (:place_name session)]
      [:span.note-place place])
    (when-let [trainer (:trainer_name session)]
      [:span.note-trainer trainer])]
   (when (seq (:notes session))
     [:div.note-body.readonly
      [markdown/render (:notes session)]])])

(defn overview-tab []
  (let [today (js/Date.)
        month (r/atom {:y (.getFullYear today) :m (.getMonth today)})]
    (fn []
      (let [sessions (:all-sessions @state/*app-state)
            ;; date -> distinct training names trained that day
            by-date (reduce (fn [acc s]
                              (update acc (:date s)
                                      (fn [names]
                                        (vec (distinct (conj (or names []) (:training_name s)))))))
                            {} sessions)
            {:keys [y m]} @month
            prev-month #(swap! month (fn [{:keys [y m]}]
                                       (if (zero? m) {:y (dec y) :m 11} {:y y :m (dec m)})))
            next-month #(swap! month (fn [{:keys [y m]}]
                                       (if (= m 11) {:y (inc y) :m 0} {:y y :m (inc m)})))]
        [:div.main-content
         [:div.page-head
          [:h1 "All sessions"]
          [:p.tagline "Everything you've logged, newest first. Read-only."]]
         [calendar by-date y m prev-month next-month]
         (if (empty? sessions)
           [:p.empty "Nothing logged yet."]
           [:div.notes
            (for [s sessions]
              ^{:key (:id s)} [session-entry s])])]))))
