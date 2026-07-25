(ns et.trn.server.youtube-handler
  (:require [et.trn.server.common :as common]
            [et.trn.db.youtube :as db.youtube]
            [et.trn.youtube.feed :as feed]
            [et.trn.youtube.poll :as poll]))

(defn list-channels-handler
  "GET /api/youtube/channels — the subscribed channels, by name."
  [req]
  {:status 200 :body (db.youtube/list-channels (common/ensure-ds) (common/get-user-id req))})

(defn add-channel-handler
  "POST /api/youtube/channels — subscribe to a channel. Takes {:input} which may
  be a channel id (UC…), a channel URL, an @handle or a video URL; the id is
  resolved from it. The channel's own name is filled in from its feed."
  [req]
  (let [user-id (common/get-user-id req)
        input (get-in req [:body :input])]
    (if-let [channel-id (feed/resolve-channel-id input)]
      (let [name (:title (feed/fetch-channel channel-id))
            channel (db.youtube/add-channel (common/ensure-ds) user-id channel-id name)]
        {:status 201 :body channel})
      {:status 400 :body {:error "Could not resolve a channel from that input"}})))

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

(defn poll-now-handler
  "POST /api/youtube/poll — poll every subscribed channel right now instead of
  waiting for the next tick. Returns {:new <count>}."
  [_req]
  {:status 200 :body {:new (poll/poll-once! (common/ensure-ds))}})
