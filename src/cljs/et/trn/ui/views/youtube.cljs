(ns et.trn.ui.views.youtube
  "Videos polled from the subscribed channels, as cards in tracker's idiom:
  click the header to open a card (which embeds the video), flag it cool or
  supercool, archive it onto the other shelf.

  A single video can also be thrown in by URL from the inbox, without following
  its channel: its uploader shows up on the Channels subpage with polling off,
  which the checkbox there can switch on later. An opened card can also delete
  the video outright, from either shelf.

  Filtering follows tracker's inbox: clicking a channel chip narrows to that
  channel, ⇧-clicking filters it out, and the active filters show as removable
  badges. Channels themselves are managed on the Channels subpage."
  (:require [reagent.core :as r]
            [clojure.string :as str]
            [et.trn.ui.state :as state]))

(def ^:private flag-labels {"cool" "★" "supercool" "★★"})

;; ---------------------------------------------------------------------------
;; filtering

(defn- flag-matches? [filter-flag flag]
  (case filter-flag
    nil true
    "cool" (contains? #{"cool" "supercool"} flag)
    "supercool" (= "supercool" flag)
    true))

(defn- visible-videos [{:keys [yt-videos yt-flag-filter yt-channel-filter yt-excluded-channels]}]
  (->> yt-videos
       (filter #(flag-matches? yt-flag-filter (:flag %)))
       (filter (fn [{:keys [channel_id]}]
                 (if yt-channel-filter
                   (= yt-channel-filter channel_id)
                   (not (contains? yt-excluded-channels channel_id)))))))

(defn- flag-filter []
  (let [current (:yt-flag-filter @state/*app-state)]
    [:div.toggle-group.flag-filter
     [:button {:class (when (nil? current) "active")
               :title "All videos"
               :on-click #(state/set-yt-flag-filter nil)} "○"]
     [:button {:class (str "cool" (when (= "cool" current) " active"))
               :title "Cool and above"
               :on-click #(state/set-yt-flag-filter "cool")} "★"]
     [:button {:class (str "supercool" (when (= "supercool" current) " active"))
               :title "Supercool only"
               :on-click #(state/set-yt-flag-filter "supercool")} "★★"]]))

(defn- channel-filter-badges []
  (let [{:keys [yt-channel-filter yt-excluded-channels yt-channels]} @state/*app-state
        channel-name (fn [cid]
                       (or (some #(when (= cid (:channel_id %)) (:name %)) yt-channels) cid))]
    (when (or yt-channel-filter (seq yt-excluded-channels))
      [:div.filter-badges
       (when yt-channel-filter
         [:span.filter-item-label.included
          (channel-name yt-channel-filter)
          [:button.remove-item {:on-click state/clear-yt-channel-filter} "×"]])
       (for [cid yt-excluded-channels]
         ^{:key cid}
         [:span.filter-item-label.excluded
          (channel-name cid)
          [:button.remove-item {:on-click #(state/clear-yt-excluded-channel cid)} "×"]])
       (when (>= (count yt-excluded-channels) 2)
         [:button.secondary.clear-filters {:on-click state/clear-yt-channel-filters} "Clear"])])))

;; ---------------------------------------------------------------------------
;; cards

(defn- channel-chip [{:keys [channel_id channel_name]}]
  [:span.channel-chip
   {:title "Click to filter · ⇧-click to filter out"
    :on-click (fn [e]
                (.stopPropagation e)
                (if (and (.-shiftKey e) (nil? (:yt-channel-filter @state/*app-state)))
                  (state/toggle-yt-excluded-channel channel_id)
                  (state/set-yt-channel-filter channel_id)))}
   (or channel_name channel_id)])

(defn- flag-selector [video]
  [:div.toggle-group.compact.flag-selector
   (for [[flag label] [["cool" "★"] ["supercool" "★★"]]]
     ^{:key flag}
     [:button {:class (str flag (when (= flag (:flag video)) " active"))
               :title (str "Mark " flag)
               :on-click (fn [e]
                           (.stopPropagation e)
                           (state/set-yt-flag video flag))}
      label])])

(defn- video-embed [video-id]
  [:div.video-embed
   [:iframe {:src (str "https://www.youtube.com/embed/" video-id)
             :allowFullScreen true
             :frameBorder "0"}]])

(defn- video-card [_video]
  (let [expanded? (r/atom false)]
    (fn [video]
      (let [archived? (= 1 (:archived video))]
        [:div.card {:class (when @expanded? "expanded")}
         [:div.card-header {:on-click #(swap! expanded? not)}
          [:span.card-title
           (when-let [flag (:flag video)] [:span.flag-badge {:class flag} (flag-labels flag)])
           (or (:title video) (:video_id video))]
          [:a.icon-btn.jump-btn {:href (str "https://www.youtube.com/watch?v=" (:video_id video))
                                 :target "_blank"
                                 :rel "noopener noreferrer"
                                 :title "Watch on YouTube"
                                 :on-click #(.stopPropagation %)} "↗"]]
         [:div.card-meta
          [channel-chip video]
          [:span.card-date (some-> (:created_at video) (str/split #" ") first)]]
         (when @expanded?
           [:div.card-details
            [video-embed (:video_id video)]
            [:div.card-footer.split
             [flag-selector video]
             [:div.card-footer-actions
              [:button.secondary {:on-click #(state/set-yt-archived video (not archived?))}
               (if archived? "Move to inbox" "Archive")]
              [:button.danger
               {:title "Remove this video for good"
                :on-click #(when (js/confirm (str "Delete \""
                                                  (or (:title video) (:video_id video))
                                                  "\"? It won't come back on the next check."))
                             (state/delete-yt-video video))}
               "Delete"]]]])]))))

;; ---------------------------------------------------------------------------
;; adding a single video

(defn- add-video-form []
  (let [input (r/atom "")]
    (fn []
      (let [submit (fn []
                     (when (seq (str/trim @input))
                       (state/add-yt-video (str/trim @input) #(reset! input ""))))]
        [:div.add-form
         [:input {:type "text"
                  :placeholder "Throw in a single video — URL or id, no subscription"
                  :value @input
                  :on-change #(reset! input (-> % .-target .-value))
                  :on-key-down #(when (= "Enter" (.-key %)) (submit))}]
         [:button {:on-click submit} "Add"]]))))

;; ---------------------------------------------------------------------------
;; channels subpage

(defn- poll-toggle [channel]
  [:label.poll-toggle {:title "Pick up this channel's new videos automatically"}
   [:input {:type "checkbox"
            :checked (= 1 (:polled channel))
            :on-change #(state/set-yt-channel-polled channel (-> % .-target .-checked))}]
   "Poll"])

(defn- channels-subpage []
  (let [input (r/atom "")]
    (fn []
      (let [channels (:yt-channels @state/*app-state)
            submit (fn []
                     (when (seq (str/trim @input))
                       (state/add-yt-channel (str/trim @input) #(reset! input ""))))]
        [:div.main-content
         [:div.page-head
          [:button.secondary.back-btn {:on-click #(state/show-yt-config false)} "← Videos"]
          [:h1 "Channels"]
          [:p.tagline
           "Channels added here are polled. Uploaders of thrown-in single videos "
           "sit here unpolled until you tick them."]]
         [:div.add-form
          [:input {:type "text"
                   :placeholder "Channel id (UC…), channel URL, @handle, or a video URL"
                   :value @input
                   :on-change #(reset! input (-> % .-target .-value))
                   :on-key-down #(when (= "Enter" (.-key %)) (submit))}]
          [:button {:on-click submit} "Add"]]
         (if (empty? channels)
           [:p.empty "No channels yet — add one above."]
           [:div.card-list
            (for [c channels]
              ^{:key (:id c)}
              [:div.card
               [:div.card-header.static
                [:span.card-title (or (:name c) (:channel_id c))]
                [:button.icon-btn.danger
                 {:title "Remove"
                  :on-click #(when (js/confirm (str "Remove \"" (or (:name c) (:channel_id c))
                                                    "\"? Its videos go too."))
                               (state/delete-yt-channel (:id c)))}
                 "×"]]
               [:div.card-meta
                [poll-toggle c]
                [:span.channel-id (:channel_id c)]]])])]))))

;; ---------------------------------------------------------------------------

(defn youtube-tab []
  (let [app @state/*app-state]
    (if (:yt-config? app)
      [channels-subpage]
      (let [{:keys [yt-shelf yt-polling?]} app
            videos (visible-videos app)]
        [:div.main-content
         [:div.page-head
          [:h1 "YouTube"]
          [:p.tagline "Videos from your channels, newest first."]]
         [:div.yt-bar
          [:div.toggle-group.shelf-toggle
           [:button {:class (when (= :inbox yt-shelf) "active")
                     :on-click #(state/set-yt-shelf :inbox)} "Inbox"]
           [:button {:class (when (= :archive yt-shelf) "active")
                     :on-click #(state/set-yt-shelf :archive)} "Archive"]]
          [flag-filter]
          [:div.yt-bar-right
           [:button.secondary {:on-click state/poll-yt-now :disabled yt-polling?}
            (if yt-polling? "Checking…" "Check now")]
           [:button.secondary {:on-click #(state/show-yt-config true)} "Channels"]]]
         [channel-filter-badges]
         (when (= :inbox yt-shelf) [add-video-form])
         (if (empty? videos)
           [:p.empty (if (= :archive yt-shelf)
                       "Nothing archived yet."
                       "Nothing here — throw in a video above, or add a channel and check for videos.")]
           [:div.card-list
            (for [v videos]
              ^{:key (:id v)} [video-card v])])]))))
