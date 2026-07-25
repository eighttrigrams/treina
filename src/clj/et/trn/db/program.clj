(ns et.trn.db.program
  "The program: one text document per user holding the training philosophy, with
  every earlier state kept.

  History model follows rhizome's: `program` always holds the *current* text and
  `program_history` holds the superseded ones, keyed (program_id, version). A
  save pushes the outgoing text into history first, so the newest history row is
  the state just before the current one. Version numbers therefore grow with
  each save and never change afterwards, which is what makes 'how did this read
  back then' answerable."
  (:require [next.jdbc :as jdbc]
            [honey.sql :as sql]
            [taoensso.telemere :as tel]
            [et.trn.db :as db]))

(def select-columns [:id :text :created_at :modified_at])

(defn- find-program [conn user-id]
  (jdbc/execute-one! conn
    (sql/format {:select select-columns
                 :from [:program]
                 :where (db/user-id-where-clause user-id)})
    db/jdbc-opts))

(defn get-program
  "The user's program, created empty on first access so the page and the API
  always have a row to talk about."
  [ds user-id]
  (let [conn (db/get-conn ds)]
    (or (find-program conn user-id)
        (do (jdbc/execute-one! conn
              (sql/format {:insert-into :program
                           :values [{:text "" :user_id user-id
                                     :modified_at [:raw "datetime('now')"]}]}))
            (find-program conn user-id)))))

(defn- next-version [conn program-id]
  (-> (jdbc/execute-one! conn
        (sql/format {:select [[[:coalesce [:max :version] 0] :max_version]]
                     :from [:program_history]
                     :where [:= :program_id program-id]})
        db/jdbc-opts)
      :max_version
      inc))

(defn- archive-current! [conn program]
  (jdbc/execute-one! conn
    (sql/format {:insert-into :program_history
                 :values [{:program_id (:id program)
                           :version (next-version conn (:id program))
                           :text (:text program)}]})))

(defn update-program
  "Save `text` as the new current state and archive the outgoing one. Returns nil
  when `expected-modified-at` no longer matches (someone else saved meanwhile).
  A save that doesn't change the text is a no-op: identical versions would only
  add empty diffs to step through."
  [ds user-id text expected-modified-at]
  (let [conn (db/get-conn ds)
        program (get-program ds user-id)]
    (cond
      (and expected-modified-at (not= expected-modified-at (:modified_at program)))
      nil

      (= text (:text program))
      program

      :else
      (do
        (archive-current! conn program)
        (let [result (jdbc/execute-one! conn
                       (sql/format {:update :program
                                    :set {:text text
                                          :modified_at [:raw "datetime('now')"]}
                                    :where [:= :id (:id program)]
                                    :returning select-columns})
                       db/jdbc-opts)]
          (tel/log! {:level :info :data {:program-id (:id program) :user-id user-id}}
                    "Program saved")
          result)))))

(defn list-versions
  "Every state of the program, newest first. The current text is included as the
  newest version (flagged `:current true`) so the diff viewer can step from
  today's text back through history in one uniform list."
  [ds user-id]
  (let [conn (db/get-conn ds)
        program (get-program ds user-id)
        history (jdbc/execute! conn
                  (sql/format {:select [:version :text :created_at]
                               :from [:program_history]
                               :where [:= :program_id (:id program)]
                               :order-by [[:version :desc]]})
                  db/jdbc-opts)]
    {:versions (into [{:version (inc (or (:version (first history)) 0))
                       :text (:text program)
                       :created_at (:modified_at program)
                       :current true}]
                     history)
     :total (inc (count history))}))
