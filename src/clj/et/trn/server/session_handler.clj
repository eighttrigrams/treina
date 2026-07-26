(ns et.trn.server.session-handler
  (:require [et.trn.server.common :as common]
            [et.trn.db.session :as db.session]
            [et.trn.db.place :as db.place]
            [et.trn.db.training :as db.training]))

(defn- unknown-place?
  "A place must exist and belong to the caller. nil means no place, which is fine."
  [user-id place-id]
  (and (some? place-id)
       (not (db.place/place-owned-by-user? (common/ensure-ds) place-id user-id))))

(defn list-sessions-handler
  "GET /api/trainings/:tid/sessions — sessions of one training, newest first."
  [req]
  (let [user-id (common/get-user-id req)
        tid (common/parse-int-opt (get-in req [:params :tid]))]
    (if (and tid (db.training/training-owned-by-user? (common/ensure-ds) tid user-id))
      {:status 200 :body (db.session/list-sessions (common/ensure-ds) user-id tid)}
      {:status 404 :body {:error "Training not found"}})))

(defn list-all-sessions-handler
  "GET /api/sessions — every session across all trainings, newest first, each
  carrying its parent training's `training_name` and, when set, its
  `place_name`. Feeds the read-only overview."
  [req]
  (let [user-id (common/get-user-id req)]
    {:status 200 :body (db.session/list-all-sessions (common/ensure-ds) user-id)}))

(defn add-session-handler
  "POST /api/trainings/:tid/sessions — add a session {:date :notes :place_id}.
  `place_id` is optional; when given it must be one of your places (400 if not)."
  [req]
  (let [user-id (common/get-user-id req)
        tid (common/parse-int-opt (get-in req [:params :tid]))
        {:keys [date notes place_id]} (:body req)]
    (cond
      (not (and tid (db.training/training-owned-by-user? (common/ensure-ds) tid user-id)))
      {:status 404 :body {:error "Training not found"}}

      (not (common/valid-date-format? date))
      {:status 400 :body {:error "date must be YYYY-MM-DD"}}

      (unknown-place? user-id place_id)
      {:status 400 :body {:error "Place not found"}}

      :else
      {:status 201 :body (db.session/add-session (common/ensure-ds) user-id tid
                                                 date (or notes "") place_id)})))

(defn update-session-handler
  "PUT /api/sessions/:id — update date, notes and/or place. Send `place_id` as
  null to take a session's place off it again."
  [req]
  (let [user-id (common/get-user-id req)
        id (common/parse-int-opt (get-in req [:params :id]))
        {:keys [date notes place_id modified_at]} (:body req)
        place-given? (contains? (:body req) :place_id)
        fields (cond-> {}
                 (some? date) (assoc :date date)
                 (some? notes) (assoc :notes notes)
                 place-given? (assoc :place_id place_id))]
    (cond
      (nil? id) {:status 404 :body {:error "Session not found"}}
      (not (common/valid-date-format? date)) {:status 400 :body {:error "date must be YYYY-MM-DD"}}
      (unknown-place? user-id place_id) {:status 400 :body {:error "Place not found"}}
      (empty? fields) {:status 400 :body {:error "nothing to update"}}
      :else
      (let [result (db.session/update-session (common/ensure-ds) user-id id fields modified_at)]
        (if result
          {:status 200 :body result}
          (common/conflict-or-not-found
           (db.session/get-session (common/ensure-ds) user-id id)
           "Session not found"))))))

(defn delete-session-handler
  "DELETE /api/sessions/:id — delete a session."
  [req]
  (let [user-id (common/get-user-id req)
        id (common/parse-int-opt (get-in req [:params :id]))
        result (when id (db.session/delete-session (common/ensure-ds) user-id id))]
    (if (:success result)
      {:status 200 :body result}
      {:status 404 :body {:error "Session not found"}})))
