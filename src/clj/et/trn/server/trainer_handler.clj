(ns et.trn.server.trainer-handler
  (:require [et.trn.server.common :as common]
            [et.trn.db.trainer :as db.trainer]
            [clojure.string :as str]))

(defn list-trainers-handler
  "GET /api/trainers — the user's trainers, alphabetically, each with how many
  sessions they took (`session_count`)."
  [req]
  {:status 200 :body (db.trainer/list-trainers (common/ensure-ds) (common/get-user-id req))})

(defn get-trainer-handler
  "GET /api/trainers/:id — a single trainer. 404 when it isn't yours."
  [req]
  (let [user-id (common/get-user-id req)
        id (common/parse-int-opt (get-in req [:params :id]))
        trainer (when id (db.trainer/get-trainer (common/ensure-ds) user-id id))]
    (if trainer
      {:status 200 :body trainer}
      {:status 404 :body {:error "Trainer not found"}})))

(defn add-trainer-handler
  "POST /api/trainers — create a trainer {:name :description}. 400 without a name."
  [req]
  (let [user-id (common/get-user-id req)
        {:keys [name description]} (:body req)]
    (if (str/blank? name)
      {:status 400 :body {:error "name is required"}}
      {:status 201 :body (db.trainer/add-trainer (common/ensure-ds) user-id
                                                 (str/trim name) (or description ""))})))

(defn update-trainer-handler
  "PUT /api/trainers/:id — update name and/or description. Pass `modified_at`
  from the last read to be told (409) when it changed elsewhere."
  [req]
  (let [user-id (common/get-user-id req)
        id (common/parse-int-opt (get-in req [:params :id]))
        {:keys [name description modified_at]} (:body req)
        fields (cond-> {}
                 (some? name) (assoc :name (str/trim name))
                 (some? description) (assoc :description description))]
    (cond
      (nil? id) {:status 404 :body {:error "Trainer not found"}}
      (and (some? name) (str/blank? name)) {:status 400 :body {:error "name is required"}}
      (empty? fields) {:status 400 :body {:error "nothing to update"}}
      :else
      (if-let [result (db.trainer/update-trainer (common/ensure-ds) user-id id fields modified_at)]
        {:status 200 :body result}
        (common/conflict-or-not-found
         (db.trainer/get-trainer (common/ensure-ds) user-id id)
         "Trainer not found")))))

(defn delete-trainer-handler
  "DELETE /api/trainers/:id — delete a trainer. Sessions they took stay, they
  just lose the trainer."
  [req]
  (let [user-id (common/get-user-id req)
        id (common/parse-int-opt (get-in req [:params :id]))
        result (when id (db.trainer/delete-trainer (common/ensure-ds) user-id id))]
    (if (:success result)
      {:status 200 :body result}
      {:status 404 :body {:error "Trainer not found"}})))
