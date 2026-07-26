(ns et.trn.db.session
  "Sessions: one logged training on one date. `place_id` and `trainer_id` are both
  optional — a session may name where it happened and who took it — and reads
  carry those names along."
  (:require [next.jdbc :as jdbc]
            [honey.sql :as sql]
            [taoensso.telemere :as tel]
            [et.trn.db :as db]))

(def select-columns
  [:id :training_id :place_id :trainer_id :date :notes :created_at :modified_at])

(def ^:private read-columns
  "What a listing returns: the session's own columns plus the place's and
  trainer's names, which the cards show instead of the bare ids. Both LEFT
  JOINed, since both are optional."
  [:s.id :s.training_id :s.place_id :s.trainer_id :s.date :s.notes
   :s.created_at :s.modified_at
   [:p.name :place_name] [:tr.name :trainer_name]])

(defn add-session
  ([ds user-id training-id date] (add-session ds user-id training-id date "" nil nil))
  ([ds user-id training-id date notes] (add-session ds user-id training-id date notes nil nil))
  ([ds user-id training-id date notes place-id] (add-session ds user-id training-id date notes place-id nil))
  ([ds user-id training-id date notes place-id trainer-id]
   (let [result (jdbc/execute-one! (db/get-conn ds)
                  (sql/format {:insert-into :sessions
                               :values [{:training_id training-id
                                         :place_id place-id
                                         :trainer_id trainer-id
                                         :date date
                                         :notes (or notes "")
                                         :user_id user-id
                                         :modified_at [:raw "datetime('now')"]}]
                               :returning select-columns})
                  db/jdbc-opts)]
     (tel/log! {:level :info :data {:session-id (:id result) :training-id training-id :user-id user-id}} "Session added")
     result)))

(defn list-sessions
  "Sessions for a single training, newest date first."
  [ds user-id training-id]
  (jdbc/execute! (db/get-conn ds)
    (sql/format {:select read-columns
                 :from [[:sessions :s]]
                 :left-join [[:places :p] [:= :p.id :s.place_id]
                             [:trainers :tr] [:= :tr.id :s.trainer_id]]
                 :where [:and
                         [:= :s.training_id training-id]
                         (if user-id [:= :s.user_id user-id] [:is :s.user_id nil])]
                 :order-by [[:s.date :desc] [:s.id :desc]]})
    db/jdbc-opts))

(defn list-all-sessions
  "Every session the user owns, across all trainings, newest date first and
  decorated with the parent training's name (the overview feed needs the name,
  not just the id)."
  [ds user-id]
  (jdbc/execute! (db/get-conn ds)
    (sql/format {:select (conj read-columns [:t.name :training_name])
                 :from [[:sessions :s]]
                 :join [[:trainings :t] [:= :t.id :s.training_id]]
                 :left-join [[:places :p] [:= :p.id :s.place_id]
                             [:trainers :tr] [:= :tr.id :s.trainer_id]]
                 :where (if user-id [:= :s.user_id user-id] [:is :s.user_id nil])
                 :order-by [[:s.date :desc] [:s.id :desc]]})
    db/jdbc-opts))

(defn session-owned-by-user? [ds session-id user-id]
  (some? (jdbc/execute-one! (db/get-conn ds)
           (sql/format {:select [:id]
                        :from [:sessions]
                        :where [:and [:= :id session-id] (db/user-id-where-clause user-id)]})
           db/jdbc-opts)))

(defn get-session [ds user-id session-id]
  (jdbc/execute-one! (db/get-conn ds)
    (sql/format {:select select-columns
                 :from [:sessions]
                 :where [:and [:= :id session-id] (db/user-id-where-clause user-id)]})
    db/jdbc-opts))

(defn update-session
  ([ds user-id session-id fields] (update-session ds user-id session-id fields nil))
  ([ds user-id session-id fields expected-modified-at]
   (let [set-map (assoc fields :modified_at [:raw "datetime('now')"])]
     (jdbc/execute-one! (db/get-conn ds)
       (sql/format {:update :sessions
                    :set set-map
                    :where (db/update-where session-id user-id expected-modified-at)
                    :returning select-columns})
       db/jdbc-opts))))

(defn delete-session [ds user-id session-id]
  (when (session-owned-by-user? ds session-id user-id)
    (let [result (jdbc/execute-one! (db/get-conn ds)
                   (sql/format {:delete-from :sessions
                                :where [:= :id session-id]}))]
      (tel/log! {:level :info :data {:session-id session-id :user-id user-id}} "Session deleted")
      {:success (pos? (:next.jdbc/update-count result))})))
