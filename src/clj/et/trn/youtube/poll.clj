(ns et.trn.youtube.poll
  "Background polling of the subscribed channels, after rhizome's youtube.poll:
  every channel's Atom feed is read on a fixed interval and unseen videos are
  recorded — title and publish date included, straight from the feed.

  A channel that fails is logged and skipped; one bad feed never stops the tick."
  (:require [taoensso.telemere :as tel]
            [et.trn.db.youtube :as db.youtube]
            [et.trn.youtube.feed :as feed])
  (:import [java.util.concurrent Executors TimeUnit]))

(defn poll-channel!
  "Record the channel's unseen videos. Returns how many were new."
  [ds {:keys [id channel_id name user_id]}]
  (if-let [{:keys [title videos]} (feed/fetch-channel channel_id)]
    (do
      (when (and (seq title) (not= title name))
        (db.youtube/set-channel-name ds id title))
      (count (for [{:keys [video-id] :as video} videos
                   :when (not (db.youtube/video-known? ds user_id video-id))]
               (do (db.youtube/add-video ds user_id id video) 1))))
    (do (tel/log! {:level :warn :data {:channel-id channel_id}} "YouTube feed unreadable")
        0)))

(defn poll-once!
  "One pass over every channel of every user. Returns the number of new videos."
  [ds]
  (reduce (fn [acc channel]
            (+ acc (try (poll-channel! ds channel)
                        (catch Exception e
                          (tel/log! {:level :error :data {:channel (:channel_id channel)}}
                                    (str "YouTube poll failed: " (.getMessage e)))
                          0))))
          0
          (db.youtube/list-all-channels ds)))

(defonce ^:private scheduler (atom nil))

(defn start-scheduler!
  "Poll every `interval-minutes`, first run shortly after boot. `interval-minutes`
  of 0 (or less) disables polling entirely — the manual trigger still works."
  [ds interval-minutes]
  (when (and (pos? interval-minutes) (not @scheduler))
    (let [exec (Executors/newSingleThreadScheduledExecutor)]
      (.scheduleAtFixedRate exec
                            ^Runnable (fn []
                                        (try
                                          (let [n (poll-once! ds)]
                                            (when (pos? n)
                                              (tel/log! {:level :info :data {:new-videos n}}
                                                        "YouTube poll picked up videos")))
                                          (catch Throwable e
                                            (tel/log! :error (str "YouTube poll tick failed: "
                                                                  (.getMessage e))))))
                            (long 30)
                            (long (* 60 interval-minutes))
                            TimeUnit/SECONDS)
      (reset! scheduler exec)
      (tel/log! :info (str "YouTube poller started (every " interval-minutes " min)")))))

(defn stop-scheduler! []
  (when-let [exec @scheduler]
    (.shutdownNow exec)
    (reset! scheduler nil)))
