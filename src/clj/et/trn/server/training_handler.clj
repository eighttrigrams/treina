(ns et.trn.server.training-handler
  (:require [et.trn.server.common :as common]
            [et.trn.db.training :as db.training]
            [clojure.string :as str]))

(defn list-trainings-handler
  "GET /api/trainings — the user's trainings, optionally filtered by ?search."
  [req]
  (let [user-id (common/get-user-id req)
        search (get-in req [:query-params "search"])]
    {:status 200 :body (db.training/list-trainings (common/ensure-ds) user-id {:search-term search})}))

(defn get-training-handler
  "GET /api/trainings/:id — a single training."
  [req]
  (let [user-id (common/get-user-id req)
        id (common/parse-int-opt (get-in req [:params :id]))
        training (when id (db.training/get-training (common/ensure-ds) user-id id))]
    (if training
      {:status 200 :body training}
      {:status 404 :body {:error "Training not found"}})))

(defn add-training-handler
  "POST /api/trainings — create a training {:name :description}."
  [req]
  (let [user-id (common/get-user-id req)
        {:keys [name description]} (:body req)]
    (if (str/blank? name)
      {:status 400 :body {:error "name is required"}}
      {:status 201 :body (db.training/add-training (common/ensure-ds) user-id
                                                   (str/trim name) (or description ""))})))

(defn update-training-handler
  "PUT /api/trainings/:id — update name and/or description."
  [req]
  (let [user-id (common/get-user-id req)
        id (common/parse-int-opt (get-in req [:params :id]))
        {:keys [name description modified_at]} (:body req)
        fields (cond-> {}
                 (some? name) (assoc :name (str/trim name))
                 (some? description) (assoc :description description))]
    (cond
      (nil? id) {:status 404 :body {:error "Training not found"}}
      (empty? fields) {:status 400 :body {:error "nothing to update"}}
      :else
      (let [result (db.training/update-training (common/ensure-ds) user-id id fields modified_at)]
        (if result
          {:status 200 :body result}
          (common/conflict-or-not-found
           (db.training/get-training (common/ensure-ds) user-id id)
           "Training not found"))))))

(defn delete-training-handler
  "DELETE /api/trainings/:id — delete a training (its sessions cascade)."
  [req]
  (let [user-id (common/get-user-id req)
        id (common/parse-int-opt (get-in req [:params :id]))
        result (when id (db.training/delete-training (common/ensure-ds) user-id id))]
    (if (:success result)
      {:status 200 :body result}
      {:status 404 :body {:error "Training not found"}})))
