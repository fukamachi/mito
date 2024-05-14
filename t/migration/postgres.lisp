(defpackage #:mito-test.migration.postgres
  (:use #:cl
        #:rove
        #:mito
        #:mito.migration
        #:mito.migration.table
        #:mito-test.util))
(in-package #:mito-test.migration.postgres)

(setup
 (setf *connection* (connect-to-testdb :postgres))

 (when (find-class 'tweets nil)
   (setf (find-class 'tweets) nil))
 (execute-sql "DROP TABLE IF EXISTS tweets"))

(deftest postgres-migration-tests
  (testing "first definition (no explicit primary key)"
    (defclass tweets ()
      ((user :col-type (:varchar 128)
             :accessor tweet-user))
      (:metaclass dao-table-class)
      (:record-timestamps nil))
    (mapc #'execute-sql (table-definition 'tweets))

    (ok (equal (mito.migration.table::migration-expressions-for-others (find-class 'tweets) :postgres)
               '(nil nil nil nil nil))
        "No migration at first")

    (defclass tweets ()
      ((user :col-type (:varchar 128)
             :accessor tweet-user))
      (:metaclass dao-table-class)
      (:table-name "tweets")
      (:record-timestamps nil))

    (ok (equal (mito.migration.table::migration-expressions-for-others (find-class 'tweets) :postgres)
               '(nil nil nil nil nil))
        "No migration at first"))

  (testing "redefinition with :auto-pk nil"
    (defclass tweets ()
      ((user :col-type (:varchar 128)
             :accessor tweet-user))
      (:metaclass dao-table-class)
      (:auto-pk nil)
      (:record-timestamps nil))

    (destructuring-bind (add-columns
                         drop-columns
                         change-columns
                         add-indices
                         drop-indices)
        (mito.migration.table::migration-expressions-for-others (find-class 'tweets) :postgres)
      (ok (null add-columns)
          "No columns to add")
      (ok (equal (sxql:yield drop-columns) "ALTER TABLE tweets DROP COLUMN id")
          "Drop column id")
      (ok (null change-columns)
          "No columns to change")
      (ok (null add-indices)
          "No indices to add")
      (ok (null drop-indices)
          "No indices to drop")))

  (testing "redefinition (with explicit primary key)"
    (defclass tweets ()
      ((tweet-id :col-type :serial
                 :primary-key t
                 :reader tweet-id)
       (status :col-type :text
               :accessor tweet-status)
       (user :col-type (:varchar 128)
             :accessor tweet-user))
      (:metaclass dao-table-class)
      (:record-timestamps nil))

    (destructuring-bind (add-columns
                         drop-columns
                         change-columns
                         add-indices
                         drop-indices)
        (mito.migration.table::migration-expressions-for-others (find-class 'tweets) :postgres)
      (ok (equal (mapcar #'sxql:yield add-columns)
                 '("ALTER TABLE tweets ADD COLUMN status text NOT NULL, ADD COLUMN tweet_id serial NOT NULL PRIMARY KEY"))
          "Add id and status")
      (ok (equal (sxql:yield drop-columns) "ALTER TABLE tweets DROP COLUMN id")
          "Drop id")
      (ok (null change-columns)
          "No columns to change")
      (ok (null add-indices)
          "No indices to add (added when adding column)")
      (ok (null drop-indices)
          "No indices to drop (will remove when dropping column)"))

    (migrate-table (find-class 'tweets))

    (ok (equal (mito.migration.table::migration-expressions-for-others (find-class 'tweets) :postgres)
               '(nil nil nil nil nil))
        "No migration after migrating"))

  (testing "redefinition"
    (defclass tweets ()
      ((tweet-id :col-type :serial
                 :primary-key t
                 :reader tweet-id)
       (user :col-type (:varchar 64)
             :accessor tweet-user)
       (created-at :col-type (:char 8)))
      (:metaclass dao-table-class)
      (:record-timestamps nil))

    (destructuring-bind (add-columns
                         drop-columns
                         change-columns
                         add-indices
                         drop-indices)
        (mito.migration.table::migration-expressions-for-others (find-class 'tweets) :postgres)
      (ok (equal (mapcar #'sxql:yield add-columns)
                 '("ALTER TABLE tweets ADD COLUMN created_at character(8) NOT NULL")))
      (ok (equal (sxql:yield drop-columns) "ALTER TABLE tweets DROP COLUMN status"))
      (ok (equal (format nil "~{~A~^~%~}"
                         (mapcar #'sxql:yield change-columns))
                 "ALTER TABLE tweets ALTER COLUMN user TYPE character varying(64)"))
      (ok (null add-indices))
      (ok (null drop-indices)))

    (migrate-table (find-class 'tweets))

    (ok (equal (mito.migration.table::migration-expressions-for-others (find-class 'tweets) :postgres)
               '(nil nil nil nil nil))
        "No migration after migrating"))

  (testing "redefinition (modifying the column type)"
           (defclass tweets ()
             ((tweet-id :col-type :serial
                        :primary-key t
                        :reader tweet-id)
              (user :col-type (:varchar 128)
                    :accessor tweet-user)
              (created-at :col-type (:char 8)))
             (:metaclass dao-table-class)
             (:record-timestamps nil))

           (destructuring-bind (add-columns
                                drop-columns
                                change-columns
                                add-indices
                                drop-indices)
               (mito.migration.table::migration-expressions-for-others (find-class 'tweets) :postgres)
             (ok (null add-columns))
             (ok (null drop-columns))
             (ok (equal (format nil "~{~A~^~%~}"
                                (mapcar #'sxql:yield change-columns))
                        "ALTER TABLE tweets ALTER COLUMN user TYPE character varying(128)"))
             (ok (null add-indices))
             (ok (null drop-indices)))

           (migrate-table (find-class 'tweets))

           (ok (equal (mito.migration.table::migration-expressions-for-others (find-class 'tweets) :postgres)
                      '(nil nil nil nil nil))
               "No migration after migrating"))

  (testing "redefinition of primary key"
           (defclass tweets ()
             ((tweet-id :col-type :bigserial
                        :primary-key t
                        :reader tweet-id)
              (user :col-type (:varchar 128)
                    :accessor tweet-user)
              (created-at :col-type (:char 8)))
             (:metaclass dao-table-class)
             (:record-timestamps nil))

           (destructuring-bind (add-columns
                                drop-columns
                                change-columns
                                add-indices
                                drop-indices)
               (mito.migration.table::migration-expressions-for-others (find-class 'tweets) :postgres)
             (ok (null add-columns))
             (ok (null drop-columns))
             (ok (equal (format nil "~{~A~^~%~}"
                                (mapcar #'sxql:yield change-columns))
                        "ALTER TABLE tweets ALTER COLUMN tweet_id TYPE bigint"))
             (ok (null add-indices))
             (ok (null drop-indices)))

           (migrate-table (find-class 'tweets))

           (ok (equal (mito.migration.table::migration-expressions-for-others (find-class 'tweets) :postgres)
                      '(nil nil nil nil nil))
               "No migration after migrating"))

  (testing "add :unique-keys"
    (defclass tweets ()
      ((tweet-id :col-type :bigserial
                 :primary-key t
                 :reader tweet-id)
       (user :col-type (:varchar 128)
             :accessor tweet-user)
       (created-at :col-type (:char 8)))
      (:metaclass dao-table-class)
      (:unique-keys (user created-at))
      (:record-timestamps nil))

    (destructuring-bind (add-columns
                         drop-columns
                         change-columns
                         add-indices
                         drop-indices)
        (mito.migration.table::migration-expressions-for-others (find-class 'tweets) :postgres)
      (ok (null add-columns))
      (ok (null drop-columns))
      (ok (null change-columns))
      (ok (= (length add-indices) 1))
      (ok (ppcre:scan "^CREATE UNIQUE INDEX [^ ]+ ON tweets \\(user, created_at\\)$"
                      (sxql:yield (first add-indices))))
      (ok (null drop-indices)))

    (migrate-table (find-class 'tweets))

    (ok (equal (mito.migration.table::migration-expressions-for-others (find-class 'tweets) :postgres)
               '(nil nil nil nil nil))
        "No migration after migrating"))

  (testing "modify :unique-keys"
    (defclass tweets ()
      ((tweet-id :col-type :bigserial
                 :primary-key t
                 :reader tweet-id)
       (user :col-type (:varchar 128)
             :accessor tweet-user)
       (created-at :col-type (:char 8)))
      (:metaclass dao-table-class)
      (:unique-keys (tweet-id user created-at))
      (:record-timestamps nil))

    (destructuring-bind (add-columns
                         drop-columns
                         change-columns
                         add-indices
                         drop-indices)
        (mito.migration.table::migration-expressions-for-others (find-class 'tweets) :postgres)
      (ok (null add-columns))
      (ok (null drop-columns))
      (ok (null change-columns))
      (ok (= (length add-indices) 1))
      (ok (ppcre:scan "^CREATE UNIQUE INDEX [^ ]+ ON tweets \\(tweet_id, user, created_at\\)$"
                      (sxql:yield (first add-indices))))
      (ok (= (length drop-indices) 1))
      (ok (ppcre:scan "^DROP INDEX [^ ]+$"
                      (sxql:yield (first drop-indices)))))

    (migrate-table (find-class 'tweets))

    (ok (equal (mito.migration.table::migration-expressions-for-others (find-class 'tweets) :postgres)
               '(nil nil nil nil nil))
        "No migration after migrating"))

  (testing "delete :unique-keys and add :keys"
    (defclass tweets ()
      ((tweet-id :col-type :bigserial
                 :primary-key t
                 :reader tweet-id)
       (user :col-type (:varchar 128)
             :accessor tweet-user)
       (created-at :col-type (:char 8)))
      (:metaclass dao-table-class)
      (:keys (user created-at))
      (:record-timestamps nil))

    (destructuring-bind (add-columns
                         drop-columns
                         change-columns
                         add-indices
                         drop-indices)
        (mito.migration.table::migration-expressions-for-others (find-class 'tweets) :postgres)
      (ok (null add-columns))
      (ok (null drop-columns))
      (ok (null change-columns))
      (ok (= (length add-indices) 1))
      (ok (ppcre:scan "^CREATE INDEX [^ ]+ ON tweets \\(user, created_at\\)$"
                      (sxql:yield (first add-indices))))
      (ok (= (length drop-indices) 1))
      (ok (ppcre:scan "^DROP INDEX [^ ]+$"
                      (sxql:yield (first drop-indices)))))

    (migrate-table (find-class 'tweets))

    (ok (equal (mito.migration.table::migration-expressions-for-others (find-class 'tweets) :postgres)
               '(nil nil nil nil nil))
        "No migration after migrating"))

  (testing "composite primary keys"
    (when (find-class 'tweets-tag nil)
      (setf (find-class 'tweets-tag) nil))
    (execute-sql "DROP TABLE IF EXISTS tweets_tag")
    (when (find-class 'tag nil)
      (setf (find-class 'tag) nil))
    (execute-sql "DROP TABLE IF EXISTS tag")

    (defclass tag ()
      ((name :col-type (:varchar 10)
             :initarg :name))
      (:metaclass dao-table-class))
    (ensure-table-exists 'tag)
    (defclass tweets-tag ()
      ((tweet :col-type tweets
              :initarg :tweet)
       (tag :col-type tag
            :initarg :tag))
      (:metaclass dao-table-class)
      (:record-timestamps nil)
      (:auto-pk nil)
      (:primary-key tweet tag))
    (ensure-table-exists 'tweets-tag)

    (ok (equal (mito.migration.table::migration-expressions-for-others (find-class 'tweets-tag) :postgres)
               '(nil nil nil nil nil))
        "No migration after migrating")))

(teardown
 (disconnect-toplevel))
