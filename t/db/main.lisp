(in-package :cl-user)
(defpackage mito-test.db.main
  (:use #:cl
        #:prove
        #:mito-test.util
        #:mito.db)
  (:export #:run-db-tests))
(in-package :mito-test.db.main)

(defun run-db-tests (conn)
  (plan 3)

  (subtest "column-definitions"
    (is (column-definitions conn "tweets")
        (ecase (dbi:connection-driver-type conn)
          (:mysql
           '(("id" :type "int(11)" :auto-increment t :primary-key t :not-null t)
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
        (mapcar #'cdr
                '(("dummy_autoindex_tweets_2" :unique-key t :primary-key nil :columns ("id" "user"))
                  ("dummy_autoindex_tweets_1" :unique-key t :primary-key t :columns ("id"))))
        "tweets")

    (is (mapcar #'cdr (table-indices conn "users"))
        (mapcar #'cdr
                '(("dummy_autoindex_users_1" :unique-key t :primary-key nil :columns ("first_name" "family_name"))
                  ("PRIMARY" :unique-key t :primary-key t :columns ("id"))))
        "users"))

  (subtest "last-insert-id"
    (is (last-insert-id conn "users" "id") 0
        "Should be 0 when there's no record")
    (dbi:do-sql conn "INSERT INTO users (id, first_name, family_name) VALUES (1, 'Eitaro', 'Fukamachi')")
    (is (last-insert-id conn "users" "id") 1
        "Should be 1 after inserting")
    (let ((driver-type (dbi:connection-driver-type conn)))
      (dbi:disconnect conn)
      (setf conn (connect-to-testdb driver-type)))
    (is (last-insert-id conn "users" "id") 1
        "Should be still 1 after reconnecting")
    (dbi:do-sql conn "INSERT INTO users (id, first_name, family_name) VALUES (2, 'Rudolph', 'Miller')")
    (is (last-insert-id conn "users" "id") 2
        "Should be 2 after inserting once more."))

  (finalize))
