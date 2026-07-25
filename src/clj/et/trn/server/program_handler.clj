(ns et.trn.server.program-handler
  (:require [et.trn.server.common :as common]
            [et.trn.db.program :as db.program]))

(defn get-program-handler
  "GET /api/program — the current program text (the training philosophy), with
  `modified_at` to pass back when saving."
  [req]
  (let [user-id (common/get-user-id req)]
    {:status 200 :body (db.program/get-program (common/ensure-ds) user-id)}))

(defn update-program-handler
  "PUT /api/program — save {:text}. The outgoing text becomes a new version in
  the history, so nothing is ever overwritten in place. Pass `modified_at` from
  the last read to be told (409) when someone else saved in between."
  [req]
  (let [user-id (common/get-user-id req)
        {:keys [text modified_at]} (:body req)]
    (if (nil? text)
      {:status 400 :body {:error "text is required"}}
      (if-let [result (db.program/update-program (common/ensure-ds) user-id text modified_at)]
        {:status 200 :body result}
        {:status 409 :body {:error "Program was modified elsewhere"
                            :current (db.program/get-program (common/ensure-ds) user-id)}}))))

(defn program-history-handler
  "GET /api/program/history — every version of the program, newest first, each
  with its `version`, `text` and `created_at`. The newest entry is the current
  text and carries `current: true`. Diffing consecutive entries shows how the
  philosophy changed over time."
  [req]
  (let [user-id (common/get-user-id req)]
    {:status 200 :body (db.program/list-versions (common/ensure-ds) user-id)}))
