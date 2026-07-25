(ns et.trn.server
  (:require [ring.adapter.jetty9 :as jetty]
            [et.trn.db :as db]
            [et.trn.server.common :as common]
            [et.trn.server.user-handler :as user-handler]
            [et.trn.server.training-handler :as training-handler]
            [et.trn.server.session-handler :as session-handler]
            [et.trn.server.program-handler :as program-handler]
            [et.trn.server.youtube-handler :as youtube-handler]
            [et.trn.youtube.poll :as youtube-poll]
            [et.trn.auth :as auth]
            [et.trn.server.recording-mode :as recording-mode]
            [et.trn.middleware.rate-limit :as rate-limit :refer [wrap-rate-limit]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [compojure.core :refer [defroutes GET POST PUT DELETE context]]
            [compojure.route :as route]
            [ring.middleware.json :refer [wrap-json-response wrap-json-body]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.cors :refer [wrap-cors]]
            [nrepl.server :as nrepl]
            [taoensso.telemere :as tel])
  (:gen-class))

(defn- env-int [name default]
  (if-let [v (System/getenv name)]
    (try (Integer/parseInt v) (catch Exception _ default))
    default))

(defn- reset-test-db-handler [_]
  (if (common/prod-mode?)
    {:status 403 :body {:error "Not available in production"}}
    (do (db/reset-all-data! (common/ensure-ds))
        (rate-limit/reset-rate-limit!)
        {:status 200 :body {:success true}})))

;; Prod uses a constant captured at JVM start; containers restart on each
;; deploy, so startup time doubles as a deploy-keyed cache buster.
(def ^:private prod-cache-bust (System/currentTimeMillis))

(defn- cache-bust []
  (if (common/prod-mode?)
    prod-cache-bust
    (let [js-file (io/file (io/resource "public/treina/js/main.js"))]
      (if (and js-file (.exists js-file))
        (.lastModified js-file)
        (System/currentTimeMillis)))))

(defn- serve-index [_]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (-> (io/resource "public/treina/index.html")
             slurp
             (str/replace "__CACHE_BUST__" (str (cache-bust))))})

(defn- serve-styles [_]
  {:status 200
   :headers {"Content-Type" "text/css"}
   :body (-> (io/resource "public/treina/styles.css")
             slurp
             (str/replace "__CACHE_BUST__" (str (cache-bust))))})

(def ^:private describe-namespaces
  "Namespaces whose public vars back HTTP routes. The /api/describe endpoint
  walks these to enumerate the API surface from var metadata, so the docstring
  on each handler *is* the API documentation."
  '[et.trn.server
    et.trn.server.user-handler
    et.trn.server.training-handler
    et.trn.server.session-handler
    et.trn.server.program-handler
    et.trn.server.youtube-handler])

(def ^:private route-doc-re
  "Route handlers document themselves as `METHOD /path — explanation`. Matching
  on that keeps non-route helpers (build-app etc.) out of /api/describe, so the
  listing only ever advertises things you can actually call."
  #"(?s)^(GET|POST|PUT|DELETE|PATCH)\s+(\S+)\s")

(defn describe-handler
  "GET /api/describe — enumerate the API surface: every route handler with its
  method, path and docstring. Read-only and unauthenticated; lets an agent
  discover the endpoints before calling them."
  [_req]
  {:status 200
   :body (->> describe-namespaces
              (mapcat (fn [ns-sym] (when-let [n (find-ns ns-sym)] (ns-publics n))))
              (keep (fn [[sym v]]
                      (let [doc (:doc (meta v))]
                        (when-let [[_ method path] (some->> doc (re-find route-doc-re))]
                          {:name (str sym)
                           :ns (str (ns-name (.ns ^clojure.lang.Var v)))
                           :method method
                           :path path
                           :arglists (pr-str (:arglists (meta v)))
                           :doc doc}))))
              (sort-by (juxt :path :method))
              vec)})

(defn recording-mode-handler
  "GET /api/recording-mode — whether machine-token writes are currently allowed.
  Machine tokens are read-only unless recording is on."
  [_req]
  {:status 200 :body {:recording (recording-mode/enabled?)}})

(defn toggle-recording-mode-handler
  "POST /api/recording-mode/toggle — flip the machine-write gate on/off."
  [_req]
  (let [now (recording-mode/toggle!)]
    (tel/log! {:level :info :data {:recording now}}
              (str "RECORDING MODE " (if now "ON" "OFF")))
    {:status 200 :body {:recording now}}))

(defroutes api-routes
  (context "/api" []
    (GET  "/describe" [] describe-handler)
    (GET  "/recording-mode" [] recording-mode-handler)
    (POST "/recording-mode/toggle" [] toggle-recording-mode-handler)

    (context "/auth" []
      (GET  "/required" [] user-handler/password-required-handler)
      (GET  "/me"       [] user-handler/me-handler)
      (POST "/login"    [] user-handler/login-handler))

    (context "/trainings" []
      (GET    "/"             [] training-handler/list-trainings-handler)
      (POST   "/"             [] training-handler/add-training-handler)
      (GET    "/:id"          [] training-handler/get-training-handler)
      (PUT    "/:id"          [] training-handler/update-training-handler)
      (DELETE "/:id"          [] training-handler/delete-training-handler)
      (GET    "/:tid/sessions" [] session-handler/list-sessions-handler)
      (POST   "/:tid/sessions" [] session-handler/add-session-handler))

    (context "/sessions" []
      (GET    "/"    [] session-handler/list-all-sessions-handler)
      (PUT    "/:id" [] session-handler/update-session-handler)
      (DELETE "/:id" [] session-handler/delete-session-handler))

    (context "/program" []
      (GET "/"        [] program-handler/get-program-handler)
      (PUT "/"        [] program-handler/update-program-handler)
      (GET "/history" [] program-handler/program-history-handler))

    (context "/youtube" []
      (GET    "/channels"     [] youtube-handler/list-channels-handler)
      (POST   "/channels"     [] youtube-handler/add-channel-handler)
      (DELETE "/channels/:id" [] youtube-handler/delete-channel-handler)
      (GET    "/videos"       [] youtube-handler/list-videos-handler)
      (PUT    "/videos/:id"   [] youtube-handler/update-video-handler)
      (POST   "/poll"         [] youtube-handler/poll-now-handler))

    (context "/test" []
      (POST "/reset" [] reset-test-db-handler))))

(defroutes app-routes
  api-routes
  (GET "/" [] serve-index)
  (GET "/item/*" [] serve-index)
  (GET "/styles.css" [] serve-styles)
  (route/resources "/" {:root "public/treina"})
  (route/not-found {:status 404 :body {:error "Not found"}}))

(defn- mutating-request? [req]
  (#{:post :put :delete} (:request-method req)))

(defn- public-endpoint? [req]
  (= (:uri req) "/api/auth/login"))

(defn- wrap-auth [handler prod?]
  (fn [req]
    (if (and prod?
             (mutating-request? req)
             (str/starts-with? (or (:uri req) "") "/api")
             (not (public-endpoint? req)))
      (if-let [token (auth/extract-token req)]
        (if (auth/verify-token token)
          (handler req)
          {:status 401 :headers {"Content-Type" "application/json"} :body "{\"error\":\"Invalid token\"}"})
        {:status 401 :headers {"Content-Type" "application/json"} :body "{\"error\":\"Authentication required\"}"})
      (handler req))))

(defn- app [prod?]
  (-> app-routes
      (wrap-params)
      (wrap-json-body {:keywords? true})
      ;; Threaded before wrap-auth so it runs *after* it: the token is already
      ;; verified, and the body is still an unread stream for the drop log.
      (recording-mode/wrap-machine-write-guard)
      (wrap-auth prod?)
      (wrap-json-response)
      (wrap-cors :access-control-allow-origin [#".*"]
                 :access-control-allow-methods [:get :post :put :delete])
      (wrap-rate-limit (env-int "RATE_LIMIT_MAX_REQUESTS" (if prod? 180 720))
                       (env-int "RATE_LIMIT_WINDOW_SECONDS" 60))))

(defn- run-server [port prod?]
  (let [host (or (System/getenv "HOST") "127.0.0.1")]
    (tel/log! :info (str "Binding to " host ":" port))
    (jetty/run-jetty (app prod?) {:port port :host host :join? false})))

(defn- setup-file-logging [path]
  (let [log-dir (.getParentFile (io/file path))]
    (.mkdirs log-dir)
    (tel/add-handler! :file (tel/handler:file {:path path}))))

(defn build-app
  "Initialise treina (config, datasource, optional file logging) and return a
  ring handler. Does not start jetty or nREPL — the caller (e.g. plurama) owns
  those."
  [config]
  (reset! common/*config config)
  (let [prod? (common/prod-mode?)]
    (when (and (true? (:dangerously-skip-logins? @common/*config)) prod?)
      (throw (ex-info "Cannot use :dangerously-skip-logins? in production mode" {})))
    (when-let [logfile (and (not prod?) (:logfile @common/*config))]
      (setup-file-logging logfile))
    (let [ds (common/ensure-ds)]
      (youtube-poll/start-scheduler! ds (env-int "YOUTUBE_POLL_MINUTES" 15)))
    (app prod?)))

(defn -main [& _args]
  (reset! common/*config (common/load-config))
  (let [prod? (common/prod-mode?)]
    (when-let [logfile (and (not prod?) (:logfile @common/*config))]
      (setup-file-logging logfile))
    (when (and (true? (:dangerously-skip-logins? @common/*config)) prod?)
      (throw (ex-info "Cannot use :dangerously-skip-logins? in production mode" {})))
    (tel/log! :info (str "Starting treina in " (if prod? "production" "development") " mode"))
    (youtube-poll/start-scheduler! (common/ensure-ds) (env-int "YOUTUBE_POLL_MINUTES" 15))
    (when-not prod?
      (when-let [nrepl-port (:nrepl-port @common/*config)]
        (nrepl/start-server :port nrepl-port)
        (spit ".nrepl-port" nrepl-port)
        (tel/log! :info (str "nREPL server started on port " nrepl-port))))
    (if-let [port (:port @common/*config)]
      (do
        (tel/log! :info (str "Starting server on port " port))
        (run-server port prod?)
        @(promise))
      (throw (ex-info "No port defined" {})))))
