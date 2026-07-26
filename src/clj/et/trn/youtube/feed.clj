(ns et.trn.youtube.feed
  "Talking to YouTube, modelled on rhizome's scraper: the public per-channel Atom
  feed at /feeds/videos.xml, no API key and no quota.

  The feed already carries each entry's title and publish date, so a video is
  fully named the moment it appears — nothing has to be fetched later.

  A single video that never came through a feed is named the same way, from the
  public oEmbed endpoint — also key-free, and it hands us the uploader's name
  alongside the title.

  HTTP goes through the JDK client rather than a new dependency."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.xml :as xml]
            [taoensso.telemere :as tel])
  (:import [java.io ByteArrayInputStream]
           [java.net URI URLEncoder]
           [java.net.http HttpClient HttpClient$Redirect HttpRequest HttpResponse$BodyHandlers]
           [java.nio.charset StandardCharsets]
           [java.time Duration]))

(def ^:private feed-url "https://www.youtube.com/feeds/videos.xml?channel_id=")

(def ^:private oembed-url "https://www.youtube.com/oembed?format=json&url=")

(def ^:private watch-url "https://www.youtube.com/watch?v=")

;; YouTube serves a stripped-down page to unknown agents, and the channel-id
;; lookup needs the full one.
(def ^:private user-agent "Mozilla/5.0")

(defonce ^:private client
  (delay (-> (HttpClient/newBuilder)
             (.connectTimeout (Duration/ofSeconds 10))
             (.followRedirects HttpClient$Redirect/NORMAL)
             (.build))))

(defn- http-get
  "Body of a 200 response, nil for anything else — a channel that 404s or a
  network blip must not break a poll of the other channels."
  [url]
  (try
    (let [req (-> (HttpRequest/newBuilder (URI/create url))
                  (.header "User-Agent" user-agent)
                  (.timeout (Duration/ofSeconds 15))
                  (.GET)
                  (.build))
          resp (.send @client req (HttpResponse$BodyHandlers/ofString))]
      (when (= 200 (.statusCode resp))
        (.body resp)))
    (catch Exception e
      (tel/log! {:level :warn :data {:url url}} (str "youtube fetch failed: " (.getMessage e)))
      nil)))

(defn- tag-name [el] (when (map? el) (name (:tag el))))

(defn- tag-ends? [suffix el] (boolean (some-> (tag-name el) (str/ends-with? suffix))))

(defn- child-text [children pred]
  (some (fn [el] (when (pred el) (first (:content el)))) children))

(defn- entry->video [entry]
  (let [children (:content entry)
        video-id (child-text children #(tag-ends? "videoId" %))]
    (when (seq video-id)
      {:video-id video-id
       :title (child-text children #(= "title" (tag-name %)))
       :published (child-text children #(= "published" (tag-name %)))})))

(defn parse-feed [xml-string]
  (let [parsed (xml/parse (ByteArrayInputStream. (.getBytes ^String xml-string "UTF-8")))]
    {:title (child-text (:content parsed) #(= "title" (tag-name %)))
     :videos (->> (:content parsed)
                  (filter #(= "entry" (tag-name %)))
                  (keep entry->video)
                  vec)}))

(defn fetch-channel
  "{:title <channel name> :videos [{:video-id :title :published} …]}, newest
  first as YouTube serves it. nil when the feed can't be read."
  [channel-id]
  (some-> (http-get (str feed-url channel-id)) parse-feed))

(def ^:private id-pattern "UC[\\w-]{20,}")

(def ^:private channel-id-re (re-pattern id-pattern))

(def ^:private page-id-patterns
  "Ways a page names a channel, most trustworthy first.

  On a channel or @handle page the alternate-feed link and `externalId` both name
  *that* channel, while the first `\"channelId\"` in the payload is often some
  recommended channel instead — asking for @JeffNippard and getting his podcast
  channel is exactly that trap. On a video page only `channelId` exists, and
  there it is the uploader, so it stays as the last resort."
  [(re-pattern (str "channel_id=(" id-pattern ")"))
   (re-pattern (str "\"externalId\":\"(" id-pattern ")\""))
   (re-pattern (str "\"channelId\":\"(" id-pattern ")\""))])

(defn- channel-id-from-page [url]
  (when-let [body (http-get url)]
    (some (fn [re] (second (re-find re body))) page-id-patterns)))

(defn resolve-channel-id
  "Accept what a person has at hand — a channel id, a channel URL, an @handle, or
  even a video URL — and return the UC… id. As in rhizome, anything that isn't
  already an id is resolved by loading the page and reading the id out of it."
  [input]
  (let [input (str/trim (or input ""))
        channel-path (second (re-find (re-pattern (str "channel/(" id-pattern ")")) input))]
    (cond
      (re-matches channel-id-re input) input
      channel-path channel-path
      (str/starts-with? input "@") (channel-id-from-page (str "https://www.youtube.com/" input))
      (re-find #"youtube\.com|youtu\.be" input)
      (channel-id-from-page (if (str/starts-with? input "http") input (str "https://" input)))
      :else nil)))

;; ---------------------------------------------------------------------------
;; single videos

(def ^:private video-id-pattern "[\\w-]{11}")

(def ^:private video-id-re (re-pattern video-id-pattern))

(def ^:private video-url-patterns
  "The shapes a video URL comes in — watch page, share link, embed, short."
  (mapv #(re-pattern (str % "(" video-id-pattern ")"))
        ["[?&]v=" "youtu\\.be/" "/embed/" "/shorts/" "/live/"]))

(defn resolve-video-id
  "The 11-character video id out of whatever is at hand — a watch URL, a youtu.be
  share link, an /embed/, /shorts/ or /live/ URL, or the bare id. nil when the
  input names no video."
  [input]
  (let [input (str/trim (or input ""))]
    (if (re-matches video-id-re input)
      input
      (some (fn [re] (second (re-find re input))) video-url-patterns))))

(defn fetch-video
  "{:title <video title> :author <uploader name>} from the oEmbed endpoint, nil
  when YouTube won't describe the video (private, deleted, or a bad id)."
  [video-id]
  (let [url (str oembed-url (URLEncoder/encode (str watch-url video-id) StandardCharsets/UTF_8))]
    (when-let [body (http-get url)]
      (try
        (let [{:strs [title author_name]} (json/parse-string body)]
          (when (seq title) {:title title :author author_name}))
        (catch Exception e
          (tel/log! {:level :warn :data {:video-id video-id}}
                    (str "youtube oembed unparseable: " (.getMessage e)))
          nil)))))
