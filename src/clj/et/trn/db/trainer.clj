(ns et.trn.db.trainer
  "People who trained you — a coach, a training partner. A session's trainer is
  optional, exactly like its place (see et.trn.db.place)."
  (:require [next.jdbc :as jdbc]
            [honey.sql :as sql]
            [taoensso.telemere :as tel]
            [et.trn.db :as db]))

(def select-columns [:id :name :description :created_at :modified_at])

(def ^:private session-count
  [{:select [[[:count :s.id]]]
    :from [[:sessions :s]]
    :where [:= :s.trainer_id :trainers.id]}
   :session_count])

(defn add-trainer
  ([ds user-id name] (add-trainer ds user-id name ""))
  ([ds user-id name description]
   (let [result (jdbc/execute-one! (db/get-conn ds)
                  (sql/format {:insert-into :trainers
                               :values [{:name name
                                         :description (or description "")
                                         :user_id user-id
                                         :modified_at [:raw "datetime('now')"]}]
                               :returning select-columns})
                  db/jdbc-opts)]
     (tel/log! {:level :info :data {:trainer-id (:id result) :user-id user-id}} "Trainer added")
     result)))

(defn list-trainers
  "Alphabetically, each with its `session_count`."
  [ds user-id]
  (jdbc/execute! (db/get-conn ds)
    (sql/format {:select (conj select-columns session-count)
                 :from [:trainers]
                 :where (db/user-id-where-clause user-id)
                 :order-by [[[:lower :name] :asc]]})
    db/jdbc-opts))

(defn get-trainer [ds user-id trainer-id]
  (jdbc/execute-one! (db/get-conn ds)
    (sql/format {:select select-columns
                 :from [:trainers]
                 :where [:and [:= :id trainer-id] (db/user-id-where-clause user-id)]})
    db/jdbc-opts))

(defn trainer-owned-by-user? [ds trainer-id user-id]
  (some? (get-trainer ds user-id trainer-id)))

(defn update-trainer
  ([ds user-id trainer-id fields] (update-trainer ds user-id trainer-id fields nil))
  ([ds user-id trainer-id fields expected-modified-at]
   (jdbc/execute-one! (db/get-conn ds)
     (sql/format {:update :trainers
                  :set (assoc fields :modified_at [:raw "datetime('now')"])
                  :where (db/update-where trainer-id user-id expected-modified-at)
                  :returning select-columns})
     db/jdbc-opts)))

(defn delete-trainer
  "Deletes a trainer and forgets them on every session they took; the sessions
  themselves stay. Cleared explicitly rather than by FK, which needs a
  per-connection PRAGMA the pooled per-op connections don't guarantee."
  [ds user-id trainer-id]
  (when (trainer-owned-by-user? ds trainer-id user-id)
    (let [conn (db/get-conn ds)]
      (jdbc/execute-one! conn (sql/format {:update :sessions
                                           :set {:trainer_id nil}
                                           :where [:= :trainer_id trainer-id]}))
      (let [result (jdbc/execute-one! conn (sql/format {:delete-from :trainers
                                                        :where [:= :id trainer-id]}))]
        (tel/log! {:level :info :data {:trainer-id trainer-id :user-id user-id}} "Trainer deleted")
        {:success (pos? (:next.jdbc/update-count result))}))))
