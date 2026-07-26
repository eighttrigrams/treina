(ns et.trn.db.youtube
  "Subscribed channels and the videos polled from them.

  A video row is created the first time it shows up in a channel's feed, titled
  right away from that feed. `flag` marks it cool / supercool, `archived` moves
  it out of the inbox, so an archived video stays findable.

  Deleting a video sets `deleted` instead of dropping the row. The row is what
  tells the poller it has seen that video, so a dropped one would simply come
  back on the next tick — the flag makes a delete stick. Deleted videos are
  invisible everywhere; throwing the same video in again revives its row.

  A single video can also be thrown in on its own. Its uploader still gets a
  channel row — videos always hang off one, and the channel chip keeps working —
  but that row is `polled` 0, so the poller ignores it and nothing else from that
  channel ever arrives."
  (:require [next.jdbc :as jdbc]
            [honey.sql :as sql]
            [taoensso.telemere :as tel]
            [et.trn.db :as db]))

(def flags #{"cool" "supercool"})

;; ---------------------------------------------------------------------------
;; channels

(def channel-columns [:id :channel_id :name :polled :created_at])

(defn list-channels [ds user-id]
  (jdbc/execute! (db/get-conn ds)
    (sql/format {:select channel-columns
                 :from [:youtube_channels]
                 :where (db/user-id-where-clause user-id)
                 :order-by [[[:lower [:coalesce :name :channel_id]] :asc]]})
    db/jdbc-opts))

(defn list-all-channels
  "Every polled channel of every user — what the poller iterates. Channels that
  only exist to hold a thrown-in single video are left out."
  [ds]
  (jdbc/execute! (db/get-conn ds)
    (sql/format {:select (conj channel-columns :user_id)
                 :from [:youtube_channels]
                 :where [:= :polled 1]
                 :order-by [[:id :asc]]})
    db/jdbc-opts))

(defn get-channel-by-channel-id [ds user-id channel-id]
  (jdbc/execute-one! (db/get-conn ds)
    (sql/format {:select channel-columns
                 :from [:youtube_channels]
                 :where [:and [:= :channel_id channel-id] (db/user-id-where-clause user-id)]})
    db/jdbc-opts))

(defn set-channel-polled
  "Turn polling for one channel on or off. Returns the updated row, nil when the
  channel isn't the user's."
  [ds user-id id polled?]
  (let [result (jdbc/execute-one! (db/get-conn ds)
                 (sql/format {:update :youtube_channels
                              :set {:polled (if polled? 1 0)}
                              :where [:and [:= :id id] (db/user-id-where-clause user-id)]}))]
    (when (pos? (:next.jdbc/update-count result))
      (jdbc/execute-one! (db/get-conn ds)
        (sql/format {:select channel-columns :from [:youtube_channels] :where [:= :id id]})
        db/jdbc-opts))))

(defn add-channel
  "Add `channel-id`, polled unless told otherwise. Returns the existing row when
  the channel is already there — and switches polling on for it, since asking for
  a channel by name is asking to follow it."
  ([ds user-id channel-id name] (add-channel ds user-id channel-id name true))
  ([ds user-id channel-id name polled?]
   (if-let [existing (get-channel-by-channel-id ds user-id channel-id)]
     (if (and polled? (not= 1 (:polled existing)))
       (set-channel-polled ds user-id (:id existing) true)
       existing)
     (do (jdbc/execute-one! (db/get-conn ds)
           (sql/format {:insert-into :youtube_channels
                        :values [{:channel_id channel-id :name name :user_id user-id
                                  :polled (if polled? 1 0)}]}))
         (tel/log! {:level :info :data {:channel-id channel-id :user-id user-id :polled polled?}}
                   "YouTube channel added")
         (get-channel-by-channel-id ds user-id channel-id)))))

(defn set-channel-name [ds id name]
  (jdbc/execute-one! (db/get-conn ds)
    (sql/format {:update :youtube_channels :set {:name name} :where [:= :id id]})))

(defn delete-channel
  "Unsubscribe and drop the channel's videos. Explicit rather than by FK cascade,
  which needs a per-connection PRAGMA the pooled connections don't guarantee."
  [ds user-id id]
  (let [conn (db/get-conn ds)]
    (when (jdbc/execute-one! conn
            (sql/format {:select [:id] :from [:youtube_channels]
                         :where [:and [:= :id id] (db/user-id-where-clause user-id)]})
            db/jdbc-opts)
      (jdbc/execute-one! conn (sql/format {:delete-from :youtube_videos
                                           :where [:= :channel_pk id]}))
      (let [result (jdbc/execute-one! conn (sql/format {:delete-from :youtube_channels
                                                        :where [:= :id id]}))]
        (tel/log! {:level :info :data {:channel-pk id :user-id user-id}} "YouTube channel deleted")
        {:success (pos? (:next.jdbc/update-count result))}))))

;; ---------------------------------------------------------------------------
;; videos

(def ^:private video-select
  [:v.id :v.video_id :v.title :v.published_at :v.flag :v.archived :v.deleted
   :v.created_at :v.modified_at :v.channel_pk
   [:c.name :channel_name] [:c.channel_id :channel_id]])

(defn list-videos
  "Videos of one shelf — inbox (`archived` false) or archive — newest arrival
  first, deleted ones left out. Flag and channel filtering happens in the UI;
  ordering is purely by when a video came in."
  [ds user-id archived?]
  (jdbc/execute! (db/get-conn ds)
    (sql/format {:select video-select
                 :from [[:youtube_videos :v]]
                 :join [[:youtube_channels :c] [:= :c.id :v.channel_pk]]
                 :where [:and
                         (if user-id [:= :v.user_id user-id] [:is :v.user_id nil])
                         [:= :v.deleted 0]
                         [:= :v.archived (if archived? 1 0)]]
                 :order-by [[:v.created_at :desc] [:v.id :desc]]})
    db/jdbc-opts))

(defn get-video [ds user-id id]
  (jdbc/execute-one! (db/get-conn ds)
    (sql/format {:select video-select
                 :from [[:youtube_videos :v]]
                 :join [[:youtube_channels :c] [:= :c.id :v.channel_pk]]
                 :where [:and [:= :v.id id]
                         (if user-id [:= :v.user_id user-id] [:is :v.user_id nil])]})
    db/jdbc-opts))

(defn get-video-by-video-id [ds user-id video-id]
  (jdbc/execute-one! (db/get-conn ds)
    (sql/format {:select video-select
                 :from [[:youtube_videos :v]]
                 :join [[:youtube_channels :c] [:= :c.id :v.channel_pk]]
                 :where [:and [:= :v.video_id video-id]
                         (if user-id [:= :v.user_id user-id] [:is :v.user_id nil])]})
    db/jdbc-opts))

(defn video-known?
  "Whether this video has ever been recorded — deleted ones included, so the
  poller does not bring a deleted video back."
  [ds user-id video-id]
  (some? (jdbc/execute-one! (db/get-conn ds)
           (sql/format {:select [:id] :from [:youtube_videos]
                        :where [:and [:= :video_id video-id]
                                (db/user-id-where-clause user-id)]})
           db/jdbc-opts)))

(defn add-video
  "Record a video that just appeared in a feed, with the title the feed gave us.
  Also how a single thrown-in video lands, its channel then being an unpolled one."
  [ds user-id channel-pk {:keys [video-id title published]}]
  (jdbc/execute-one! (db/get-conn ds)
    (sql/format {:insert-into :youtube_videos
                 :values [{:video_id video-id
                           :channel_pk channel-pk
                           :title title
                           :published_at published
                           :user_id user-id
                           :modified_at [:raw "datetime('now')"]}]}))
  (tel/log! {:level :info :data {:video-id video-id :channel-pk channel-pk}} "YouTube video added")
  (get-video-by-video-id ds user-id video-id))

(defn update-video
  "Set `flag` and/or `archived`. Returns nil when the row is gone or was changed
  elsewhere (optimistic concurrency on modified_at, as everywhere else here)."
  [ds user-id id fields expected-modified-at]
  (let [result (jdbc/execute-one! (db/get-conn ds)
                 (sql/format {:update :youtube_videos
                              :set (assoc fields :modified_at [:raw "datetime('now')"])
                              :where (db/update-where id user-id expected-modified-at)}))]
    (when (pos? (:next.jdbc/update-count result))
      (get-video ds user-id id))))

(defn delete-video
  "Take a video off both shelves for good. Works from the inbox or the archive."
  [ds user-id id]
  (let [result (jdbc/execute-one! (db/get-conn ds)
                 (sql/format {:update :youtube_videos
                              :set {:deleted 1 :modified_at [:raw "datetime('now')"]}
                              :where [:and [:= :id id] [:= :deleted 0]
                                      (db/user-id-where-clause user-id)]}))]
    (when (pos? (:next.jdbc/update-count result))
      (tel/log! {:level :info :data {:video-pk id :user-id user-id}} "YouTube video deleted")
      {:success true})))

(defn revive-video
  "Undo a delete, back into the inbox — what throwing a deleted video in again
  means."
  [ds user-id id]
  (jdbc/execute-one! (db/get-conn ds)
    (sql/format {:update :youtube_videos
                 :set {:deleted 0 :archived 0 :modified_at [:raw "datetime('now')"]}
                 :where [:and [:= :id id] (db/user-id-where-clause user-id)]}))
  (get-video ds user-id id))
