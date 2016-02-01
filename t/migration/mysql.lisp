(in-package :cl-user)
(defpackage mito-test.migration.mysql
  (:use #:cl
        #:prove
        #:mito
        #:mito.migration
        #:mito-test.util))
(in-package :mito-test.migration.mysql)

(plan nil)

(setf *connection* (connect-to-testdb :mysql))

(when (find-class 'tweets nil)
  (setf (find-class 'tweets) nil))
(execute-sql "DROP TABLE IF EXISTS tweets")

(subtest "first definition (no explicit primary key)"
  (defclass tweets ()
    ((user :col-type (:varchar 128)
           :accessor tweet-user))
    (:metaclass dao-table-class))
  (execute-sql (table-definition 'tweets))

  (is (mito.migration::migration-expressions-for-others (find-class 'tweets) :mysql)
      '(nil nil nil nil nil)
      "No migration at first")

  (defclass tweets ()
    ((user :col-type (:varchar 128)
           :accessor tweet-user))
    (:metaclass dao-table-class)
    (:table-name "tweets"))

  (is (mito.migration::migration-expressions-for-others (find-class 'tweets) :mysql)
      '(nil nil nil nil nil)
      "No migration at first"))

(subtest "redefinition with :auto-pk nil"
  (defclass tweets ()
    ((user :col-type (:varchar 128)
           :accessor tweet-user))
    (:metaclass dao-table-class)
    (:auto-pk nil))

  (destructuring-bind (add-columns
                       drop-columns
                       change-columns
                       add-indices
                       drop-indices)
      (mito.migration::migration-expressions-for-others (find-class 'tweets) :mysql)
    (is add-columns nil
        "No columns to add")
    (is (sxql:yield drop-columns) "ALTER TABLE tweets DROP COLUMN id"
        "Drop column id")
    (is change-columns nil
        "No columns to change")
    (is add-indices nil
        "No indices to add")
    (is drop-indices nil
        "No indices to drop")))

(subtest "redefinition (with explicit primary key)"
  (defclass tweets ()
    ((tweet-id :col-type :serial
               :primary-key t
               :reader tweet-id)
     (status :col-type :text
             :accessor tweet-status)
     (user :col-type (:varchar 128)
           :accessor tweet-user))
    (:metaclass dao-table-class))

  (destructuring-bind (add-columns
                       drop-columns
                       change-columns
                       add-indices
                       drop-indices)
      (mito.migration::migration-expressions-for-others (find-class 'tweets) :mysql)
    (is (sxql:yield add-columns) "ALTER TABLE tweets ADD COLUMN status text, ADD COLUMN tweet_id int(10) unsigned NOT NULL AUTO_INCREMENT PRIMARY KEY"
        "Add id and status")
    (is (sxql:yield drop-columns) "ALTER TABLE tweets DROP COLUMN id"
        "Drop id")
    (is change-columns nil
        "No columns to change")
    (is add-indices nil
        "No indices to add (added when adding column)")
    (is drop-indices nil
        "No indices to drop (will remove when dropping column)"))

  (migrate-table (find-class 'tweets))

  (is (mito.migration::migration-expressions-for-others (find-class 'tweets) :mysql)
      '(nil nil nil nil nil)
      "No migration after migrating"))

(subtest "redefinition"
  (defclass tweets ()
    ((tweet-id :col-type :serial
               :primary-key t
               :reader tweet-id)
     (user :col-type (:varchar 64)
           :accessor tweet-user)
     (created-at :col-type (:char 8)))
    (:metaclass dao-table-class))

  (destructuring-bind (add-columns
                       drop-columns
                       change-columns
                       add-indices
                       drop-indices)
      (mito.migration::migration-expressions-for-others (find-class 'tweets) :mysql)
    (is (sxql:yield add-columns) "ALTER TABLE tweets ADD COLUMN created_at char(8)")
    (is (sxql:yield drop-columns) "ALTER TABLE tweets DROP COLUMN status")
    (is (format nil "~{~A~^~%~}"
                (mapcar #'sxql:yield change-columns))
        "ALTER TABLE tweets MODIFY COLUMN user varchar(64)")
    (is add-indices nil)
    (is drop-indices nil))

  (migrate-table (find-class 'tweets))

  (is (mito.migration::migration-expressions-for-others (find-class 'tweets) :mysql)
      '(nil nil nil nil nil)
      "No migration after migrating"))

(subtest "redefinition (modifying the column type)"
  (defclass tweets ()
    ((tweet-id :col-type :serial
               :primary-key t
               :reader tweet-id)
     (user :col-type (:varchar 128)
           :accessor tweet-user)
     (created-at :col-type (:char 8)))
    (:metaclass dao-table-class))

  (destructuring-bind (add-columns
                       drop-columns
                       change-columns
                       add-indices
                       drop-indices)
      (mito.migration::migration-expressions-for-others (find-class 'tweets) :mysql)
    (is add-columns nil)
    (is drop-columns nil)
    (is (format nil "~{~A~^~%~}"
                (mapcar #'sxql:yield change-columns)) "ALTER TABLE tweets MODIFY COLUMN user varchar(128)")
    (is add-indices nil)
    (is drop-indices nil))

  (migrate-table (find-class 'tweets))

  (is (mito.migration::migration-expressions-for-others (find-class 'tweets) :mysql)
      '(nil nil nil nil nil)
      "No migration after migrating"))

(subtest "redefinition of primary key"
  (defclass tweets ()
    ((tweet-id :col-type :bigserial
               :primary-key t
               :reader tweet-id)
     (user :col-type (:varchar 128)
           :accessor tweet-user)
     (created-at :col-type (:char 8)))
    (:metaclass dao-table-class))

  (destructuring-bind (add-columns
                       drop-columns
                       change-columns
                       add-indices
                       drop-indices)
      (mito.migration::migration-expressions-for-others (find-class 'tweets) :mysql)
    (is add-columns nil)
    (is drop-columns nil)
    (is (format nil "~{~A~^~%~}"
                (mapcar #'sxql:yield change-columns))
        "ALTER TABLE tweets MODIFY COLUMN tweet_id bigint(20) unsigned NOT NULL AUTO_INCREMENT")
    (is add-indices nil)
    (is drop-indices nil))

  (migrate-table (find-class 'tweets))

  (is (mito.migration::migration-expressions-for-others (find-class 'tweets) :mysql)
      '(nil nil nil nil nil)
      "No migration after migrating"))

(subtest "add :unique-keys"
  (defclass tweets ()
    ((tweet-id :col-type :bigserial
               :primary-key t
               :reader tweet-id)
     (user :col-type (:varchar 128)
           :accessor tweet-user)
     (created-at :col-type (:char 8)))
    (:metaclass dao-table-class)
    (:unique-keys (user created-at)))

  (destructuring-bind (add-columns
                       drop-columns
                       change-columns
                       add-indices
                       drop-indices)
      (mito.migration::migration-expressions-for-others (find-class 'tweets) :mysql)
    (is add-columns nil)
    (is drop-columns nil)
    (is change-columns nil)
    (is (length add-indices) 1)
    (like (sxql:yield (first add-indices)) "^CREATE UNIQUE INDEX [^ ]+ ON tweets \\(created_at, user\\)$")
    (is drop-indices nil))

  (migrate-table (find-class 'tweets))

  (is (mito.migration::migration-expressions-for-others (find-class 'tweets) :mysql)
      '(nil nil nil nil nil)
      "No migration after migrating"))

(subtest "modify :unique-keys"
  (defclass tweets ()
    ((tweet-id :col-type :bigserial
               :primary-key t
               :reader tweet-id)
     (user :col-type (:varchar 128)
           :accessor tweet-user)
     (created-at :col-type (:char 8)))
    (:metaclass dao-table-class)
    (:unique-keys (tweet-id user created-at)))

  (destructuring-bind (add-columns
                       drop-columns
                       change-columns
                       add-indices
                       drop-indices)
      (mito.migration::migration-expressions-for-others (find-class 'tweets) :mysql)
    (is add-columns nil)
    (is drop-columns nil)
    (is change-columns nil)
    (is (length add-indices) 1)
    (like (sxql:yield (first add-indices)) "^CREATE UNIQUE INDEX [^ ]+ ON tweets \\(created_at, tweet_id, user\\)$")
    (is (length drop-indices) 1)
    (like (sxql:yield (first drop-indices)) "^DROP INDEX [^ ]+ ON tweets$"))

  (migrate-table (find-class 'tweets))

  (is (mito.migration::migration-expressions-for-others (find-class 'tweets) :mysql)
      '(nil nil nil nil nil)
      "No migration after migrating"))

(subtest "delete :unique-keys and add :keys"
  (defclass tweets ()
    ((tweet-id :col-type :bigserial
               :primary-key t
               :reader tweet-id)
     (user :col-type (:varchar 128)
           :accessor tweet-user)
     (created-at :col-type (:char 8)))
    (:metaclass dao-table-class)
    (:keys (user created-at)))

  (destructuring-bind (add-columns
                       drop-columns
                       change-columns
                       add-indices
                       drop-indices)
      (mito.migration::migration-expressions-for-others (find-class 'tweets) :mysql)
    (is add-columns nil)
    (is drop-columns nil)
    (is change-columns nil)
    (is (length add-indices) 1)
    (like (sxql:yield (first add-indices)) "^CREATE INDEX [^ ]+ ON tweets \\(created_at, user\\)$")
    (is (length drop-indices) 1)
    (like (sxql:yield (first drop-indices)) "^DROP INDEX [^ ]+ ON tweets$"))

  (migrate-table (find-class 'tweets))

  (is (mito.migration::migration-expressions-for-others (find-class 'tweets) :mysql)
      '(nil nil nil nil nil)
      "No migration after migrating"))

(finalize)

(disconnect-toplevel)
