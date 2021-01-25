(in-package :cl-user)
(defpackage mito-test.db.main
  (:use #:cl
        #:prove
        #:mito-test.util
        #:mito.db)
  (:export #:run-db-tests))
(in-package :mito-test.db.main)

(defun run-db-tests (conn)
  (plan 4)

  (subtest "column-definitions"
    (is (column-definitions conn "tweets")
        (ecase (dbi:connection-driver-type conn)
          (:mysql
           '(("id" :type "int" :auto-increment t :primary-key t :not-null t)
             ("status" :type "text" :auto-increment nil :primary-key nil  :not-null t)
             ("user" :type "varchar(64)" :auto-increment nil :primary-key nil :not-null t)))
          (:postgres
           '(("id" :type "integer" :auto-increment t :primary-key t :not-null t)
             ("status" :type "text" :auto-increment nil :primary-key nil  :not-null t)
             ("user" :type "character varying(64)" :auto-increment nil :primary-key nil :not-null t)))
          (:sqlite3
           '(("id" :type "INTEGER" :auto-increment t :primary-key t :not-null t)
             ("status" :type "TEXT" :auto-increment nil :primary-key nil :not-null t)
             ("user" :type "VARCHAR(64)" :auto-increment nil :primary-key nil :not-null t))))
        "tweets"))

  (subtest "table-indices"
    (is (mapcar #'cdr (table-indices conn "tweets"))
        '((:unique-key t :primary-key t :columns ("id"))
          (:unique-key t :primary-key nil :columns ("id" "user")))
        "tweets")

    (is (mapcar #'cdr (table-indices conn "users"))
        '((:unique-key t :primary-key t :columns ("id"))
          (:unique-key t :primary-key nil :columns ("first_name" "family_name")))
        "users"))

  (subtest "last-insert-id"
    (is (last-insert-id conn "users" "id") 0
        "Should be 0 when there's no record")
    (dbi:do-sql conn "INSERT INTO users (first_name, family_name) VALUES ('Eitaro', 'Fukamachi')")
    (is (last-insert-id conn "users" "id") 1
        "Should be 1 after inserting")
    (dbi:do-sql conn "INSERT INTO users (first_name, family_name) VALUES ('Rudolph', 'Miller')")
    (is (last-insert-id conn "users" "id") 2
        "Should be 2 after inserting once more."))

  (subtest "Retry DBI:DBI-DATABASE-ERROR when using prepared cache"
    (let ((mito:*connection* conn))
      (mito:execute-sql
        "DROP TABLE IF EXISTS prepared_cache_retry")
      (mito:execute-sql
        "CREATE TABLE IF NOT EXISTS prepared_cache_retry (id VARCHAR(32))")
      (let ((mito:*use-prepare-cached* t))
        (is (mito:retrieve-by-sql
              "SELECT * FROM prepared_cache_retry")
            nil)
        (mito:execute-sql
          "ALTER TABLE prepared_cache_retry ADD COLUMN name VARCHAR(64)")
        (is (mito:retrieve-by-sql
              "SELECT * FROM prepared_cache_retry")
            nil))))

  (finalize))
