(ns et.trn.db.training
  (:require [next.jdbc :as jdbc]
            [honey.sql :as sql]
            [taoensso.telemere :as tel]
            [et.trn.db :as db]))

(def select-columns [:id :name :description :created_at :modified_at])

(defn add-training
  ([ds user-id name] (add-training ds user-id name ""))
  ([ds user-id name description]
   (let [result (jdbc/execute-one! (db/get-conn ds)
                  (sql/format {:insert-into :trainings
                               :values [{:name name
                                         :description (or description "")
                                         :user_id user-id
                                         :modified_at [:raw "datetime('now')"]}]
                               :returning select-columns})
                  db/jdbc-opts)]
     (tel/log! {:level :info :data {:training-id (:id result) :user-id user-id}} "Training added")
     result)))

(def ^:private last-session-date
  "Correlated subquery: the newest session date of a training, NULL when it has
  none. Dates are `YYYY-MM-DD` text, so max() orders them correctly."
  [{:select [[[:max :s.date]]]
    :from [[:sessions :s]]
    :where [:= :s.training_id :trainings.id]}
   :last_session_date])

(defn list-trainings
  "Most recently trained first — the training whose last session is the newest
  sits topmost. Trainings without sessions fall to the bottom (SQLite sorts NULL
  last under DESC), alphabetically among themselves."
  ([ds user-id] (list-trainings ds user-id {}))
  ([ds user-id {:keys [search-term]}]
   (let [user-where (db/user-id-where-clause user-id)
         search-clause (db/build-search-clause search-term [:name :description])
         where-clause (into [:and user-where] (filter some? [search-clause]))]
     (jdbc/execute! (db/get-conn ds)
       (sql/format {:select (conj select-columns last-session-date)
                    :from [:trainings]
                    :where where-clause
                    :order-by [[:last_session_date :desc] [[:lower :name] :asc]]})
       db/jdbc-opts))))

(defn training-owned-by-user? [ds training-id user-id]
  (some? (jdbc/execute-one! (db/get-conn ds)
           (sql/format {:select [:id]
                        :from [:trainings]
                        :where [:and [:= :id training-id] (db/user-id-where-clause user-id)]})
           db/jdbc-opts)))

(defn get-training [ds user-id training-id]
  (jdbc/execute-one! (db/get-conn ds)
    (sql/format {:select select-columns
                 :from [:trainings]
                 :where [:and [:= :id training-id] (db/user-id-where-clause user-id)]})
    db/jdbc-opts))

(defn update-training
  ([ds user-id training-id fields] (update-training ds user-id training-id fields nil))
  ([ds user-id training-id fields expected-modified-at]
   (let [set-map (assoc fields :modified_at [:raw "datetime('now')"])]
     (jdbc/execute-one! (db/get-conn ds)
       (sql/format {:update :trainings
                    :set set-map
                    :where (db/update-where training-id user-id expected-modified-at)
                    :returning select-columns})
       db/jdbc-opts))))

(defn delete-training
  "Deletes a training and its sessions. Sessions are removed explicitly rather
  than relying on the FK cascade, which needs a per-connection PRAGMA that the
  pooled per-op connections don't guarantee."
  [ds user-id training-id]
  (when (training-owned-by-user? ds training-id user-id)
    (let [conn (db/get-conn ds)]
      (jdbc/execute-one! conn (sql/format {:delete-from :sessions
                                           :where [:= :training_id training-id]}))
      (let [result (jdbc/execute-one! conn
                     (sql/format {:delete-from :trainings
                                  :where [:= :id training-id]}))]
        (tel/log! {:level :info :data {:training-id training-id :user-id user-id}} "Training deleted")
        {:success (pos? (:next.jdbc/update-count result))}))))
