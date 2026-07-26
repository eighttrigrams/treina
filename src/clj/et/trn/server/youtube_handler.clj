(ns et.trn.server.youtube-handler
  (:require [et.trn.server.common :as common]
            [et.trn.db.youtube :as db.youtube]
            [et.trn.youtube.feed :as feed]
            [et.trn.youtube.poll :as poll]))

(defn list-channels-handler
  "GET /api/youtube/channels — the channels, by name, each with `polled` (0/1).
  Unpolled ones are uploaders of thrown-in single videos."
  [req]
  {:status 200 :body (db.youtube/list-channels (common/ensure-ds) (common/get-user-id req))})

(defn add-channel-handler
  "POST /api/youtube/channels — subscribe to a channel. Takes {:input} which may
  be a channel id (UC…), a channel URL, an @handle or a video URL; the id is
  resolved from it. The channel's own name is filled in from its feed. Added this
  way a channel is polled, and an existing unpolled one starts being polled."
  [req]
  (let [user-id (common/get-user-id req)
        input (get-in req [:body :input])]
    (if-let [channel-id (feed/resolve-channel-id input)]
      (let [name (:title (feed/fetch-channel channel-id))
            channel (db.youtube/add-channel (common/ensure-ds) user-id channel-id name)]
        {:status 201 :body channel})
      {:status 400 :body {:error "Could not resolve a channel from that input"}})))

(defn update-channel-handler
  "PUT /api/youtube/channels/:id — set `polled` (boolean): whether the poller
  picks up this channel's new videos. 404 when it isn't your channel."
  [req]
  (let [user-id (common/get-user-id req)
        id (common/parse-int-opt (get-in req [:params :id]))
        polled (get-in req [:body :polled])]
    (cond
      (nil? id) {:status 404 :body {:error "Channel not found"}}
      (nil? polled) {:status 400 :body {:error "polled must be true or false"}}
      :else
      (if-let [channel (db.youtube/set-channel-polled (common/ensure-ds) user-id id (boolean polled))]
        {:status 200 :body channel}
        {:status 404 :body {:error "Channel not found"}}))))

(defn delete-channel-handler
  "DELETE /api/youtube/channels/:id — unsubscribe; the channel's videos go too."
  [req]
  (let [user-id (common/get-user-id req)
        id (common/parse-int-opt (get-in req [:params :id]))
        result (when id (db.youtube/delete-channel (common/ensure-ds) user-id id))]
    (if (:success result)
      {:status 200 :body result}
      {:status 404 :body {:error "Channel not found"}})))

(defn list-videos-handler
  "GET /api/youtube/videos — polled videos, newest arrival first, each with its
  `channel_name`, `flag` (null | cool | supercool) and `archived`. Pass
  `?archived=true` for the archive instead of the inbox."
  [req]
  (let [user-id (common/get-user-id req)
        archived? (= "true" (get-in req [:query-params "archived"]))]
    {:status 200 :body (db.youtube/list-videos (common/ensure-ds) user-id archived?)}))

(defn add-video-handler
  "POST /api/youtube/videos — throw in one video without subscribing to anything.
  Takes {:input}: a watch URL, a youtu.be link, an /embed/, /shorts/ or /live/
  URL, or the bare 11-character id. Title and uploader come from YouTube's oEmbed
  endpoint; the uploader gets an unpolled channel row so the video has a chip and
  nothing else of theirs ever arrives. 201 with the video, 200 when it is already
  in (a deleted one comes back into the inbox), 400 when the input names no
  reachable video."
  [req]
  (let [ds (common/ensure-ds)
        user-id (common/get-user-id req)
        input (get-in req [:body :input])
        video-id (feed/resolve-video-id input)]
    (cond
      (nil? video-id)
      {:status 400 :body {:error "Could not resolve a video from that input"}}

      (db.youtube/video-known? ds user-id video-id)
      (let [existing (db.youtube/get-video-by-video-id ds user-id video-id)]
        {:status 200 :body (if (= 1 (:deleted existing))
                             (db.youtube/revive-video ds user-id (:id existing))
                             existing)})

      :else
      (if-let [{:keys [title author]} (feed/fetch-video video-id)]
        (if-let [channel-id (feed/resolve-channel-id (str "https://www.youtube.com/watch?v=" video-id))]
          (let [channel (db.youtube/add-channel ds user-id channel-id author false)]
            {:status 201 :body (db.youtube/add-video ds user-id (:id channel)
                                                     {:video-id video-id :title title})})
          {:status 400 :body {:error "Could not resolve the video's channel"}})
        {:status 400 :body {:error "Could not read that video from YouTube"}}))))

(defn update-video-handler
  "PUT /api/youtube/videos/:id — set `flag` (null, \"cool\" or \"supercool\") and/or
  `archived` (boolean). Pass `modified_at` from the last read to be told (409)
  when it changed elsewhere."
  [req]
  (let [user-id (common/get-user-id req)
        id (common/parse-int-opt (get-in req [:params :id]))
        {:keys [flag archived modified_at]} (:body req)
        flag-given? (contains? (:body req) :flag)
        fields (cond-> {}
                 flag-given? (assoc :flag (when (contains? db.youtube/flags flag) flag))
                 (some? archived) (assoc :archived (if archived 1 0)))]
    (cond
      (nil? id) {:status 404 :body {:error "Video not found"}}
      (and flag-given? (some? flag) (not (contains? db.youtube/flags flag)))
      {:status 400 :body {:error "flag must be cool, supercool or null"}}
      (empty? fields) {:status 400 :body {:error "nothing to update"}}
      :else
      (if-let [result (db.youtube/update-video (common/ensure-ds) user-id id fields modified_at)]
        {:status 200 :body result}
        (common/conflict-or-not-found
         (db.youtube/get-video (common/ensure-ds) user-id id)
         "Video not found")))))

(defn delete-video-handler
  "DELETE /api/youtube/videos/:id — remove a video from the inbox or the archive.
  It stays gone: the poller won't bring it back, and only throwing the same video
  in again by URL revives it. 404 when it isn't yours or is already gone."
  [req]
  (let [user-id (common/get-user-id req)
        id (common/parse-int-opt (get-in req [:params :id]))
        result (when id (db.youtube/delete-video (common/ensure-ds) user-id id))]
    (if (:success result)
      {:status 200 :body result}
      {:status 404 :body {:error "Video not found"}})))

(defn poll-now-handler
  "POST /api/youtube/poll — poll every subscribed channel right now instead of
  waiting for the next tick. Returns {:new <count>}."
  [_req]
  {:status 200 :body {:new (poll/poll-once! (common/ensure-ds))}})
