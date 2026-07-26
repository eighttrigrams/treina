(ns et.trn.db.place
  "Places a session can happen at — a gym, a park, home. A session's place is
  optional, so nothing here is required to log training."
  (:require [next.jdbc :as jdbc]
            [honey.sql :as sql]
            [taoensso.telemere :as tel]
            [et.trn.db :as db]))

(def select-columns [:id :name :description :created_at :modified_at])

(def ^:private session-count
  "Correlated subquery: how many sessions happened at a place. Cheap enough to
  carry on the list, and it is the one number that says whether a place is in use."
  [{:select [[[:count :s.id]]]
    :from [[:sessions :s]]
    :where [:= :s.place_id :places.id]}
   :session_count])

(defn add-place
  ([ds user-id name] (add-place ds user-id name ""))
  ([ds user-id name description]
   (let [result (jdbc/execute-one! (db/get-conn ds)
                  (sql/format {:insert-into :places
                               :values [{:name name
                                         :description (or description "")
                                         :user_id user-id
                                         :modified_at [:raw "datetime('now')"]}]
                               :returning select-columns})
                  db/jdbc-opts)]
     (tel/log! {:level :info :data {:place-id (:id result) :user-id user-id}} "Place added")
     result)))

(defn list-places
  "Alphabetically, each with its `session_count`."
  [ds user-id]
  (jdbc/execute! (db/get-conn ds)
    (sql/format {:select (conj select-columns session-count)
                 :from [:places]
                 :where (db/user-id-where-clause user-id)
                 :order-by [[[:lower :name] :asc]]})
    db/jdbc-opts))

(defn get-place [ds user-id place-id]
  (jdbc/execute-one! (db/get-conn ds)
    (sql/format {:select select-columns
                 :from [:places]
                 :where [:and [:= :id place-id] (db/user-id-where-clause user-id)]})
    db/jdbc-opts))

(defn place-owned-by-user? [ds place-id user-id]
  (some? (get-place ds user-id place-id)))

(defn update-place
  ([ds user-id place-id fields] (update-place ds user-id place-id fields nil))
  ([ds user-id place-id fields expected-modified-at]
   (jdbc/execute-one! (db/get-conn ds)
     (sql/format {:update :places
                  :set (assoc fields :modified_at [:raw "datetime('now')"])
                  :where (db/update-where place-id user-id expected-modified-at)
                  :returning select-columns})
     db/jdbc-opts)))

(defn delete-place
  "Deletes a place and forgets it on every session that happened there; the
  sessions themselves stay. Cleared explicitly rather than by FK, which needs a
  per-connection PRAGMA the pooled per-op connections don't guarantee."
  [ds user-id place-id]
  (when (place-owned-by-user? ds place-id user-id)
    (let [conn (db/get-conn ds)]
      (jdbc/execute-one! conn (sql/format {:update :sessions
                                           :set {:place_id nil}
                                           :where [:= :place_id place-id]}))
      (let [result (jdbc/execute-one! conn (sql/format {:delete-from :places
                                                        :where [:= :id place-id]}))]
        (tel/log! {:level :info :data {:place-id place-id :user-id user-id}} "Place deleted")
        {:success (pos? (:next.jdbc/update-count result))}))))
