(ns et.trn.server.place-handler
  (:require [et.trn.server.common :as common]
            [et.trn.db.place :as db.place]
            [clojure.string :as str]))

(defn list-places-handler
  "GET /api/places — the user's places, alphabetically, each with how many
  sessions happened there (`session_count`)."
  [req]
  {:status 200 :body (db.place/list-places (common/ensure-ds) (common/get-user-id req))})

(defn get-place-handler
  "GET /api/places/:id — a single place. 404 when it isn't yours."
  [req]
  (let [user-id (common/get-user-id req)
        id (common/parse-int-opt (get-in req [:params :id]))
        place (when id (db.place/get-place (common/ensure-ds) user-id id))]
    (if place
      {:status 200 :body place}
      {:status 404 :body {:error "Place not found"}})))

(defn add-place-handler
  "POST /api/places — create a place {:name :description}. 400 without a name."
  [req]
  (let [user-id (common/get-user-id req)
        {:keys [name description]} (:body req)]
    (if (str/blank? name)
      {:status 400 :body {:error "name is required"}}
      {:status 201 :body (db.place/add-place (common/ensure-ds) user-id
                                             (str/trim name) (or description ""))})))

(defn update-place-handler
  "PUT /api/places/:id — update name and/or description. Pass `modified_at` from
  the last read to be told (409) when it changed elsewhere."
  [req]
  (let [user-id (common/get-user-id req)
        id (common/parse-int-opt (get-in req [:params :id]))
        {:keys [name description modified_at]} (:body req)
        fields (cond-> {}
                 (some? name) (assoc :name (str/trim name))
                 (some? description) (assoc :description description))]
    (cond
      (nil? id) {:status 404 :body {:error "Place not found"}}
      (and (some? name) (str/blank? name)) {:status 400 :body {:error "name is required"}}
      (empty? fields) {:status 400 :body {:error "nothing to update"}}
      :else
      (if-let [result (db.place/update-place (common/ensure-ds) user-id id fields modified_at)]
        {:status 200 :body result}
        (common/conflict-or-not-found
         (db.place/get-place (common/ensure-ds) user-id id)
         "Place not found")))))

(defn delete-place-handler
  "DELETE /api/places/:id — delete a place. Sessions that happened there stay,
  they just lose their place."
  [req]
  (let [user-id (common/get-user-id req)
        id (common/parse-int-opt (get-in req [:params :id]))
        result (when id (db.place/delete-place (common/ensure-ds) user-id id))]
    (if (:success result)
      {:status 200 :body result}
      {:status 404 :body {:error "Place not found"}})))
