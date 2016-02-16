(in-package :cl-user)
(defpackage mito-test.migration.sqlite3
  (:use #:cl
        #:prove
        #:mito
        #:mito.migration
        #:mito-test.util))
(in-package :mito-test.migration.sqlite3)

(plan nil)

(setf *connection* (reconnect-to-testdb (connect-to-testdb :sqlite3)))

(when (find-class 'tweets nil)
  (setf (find-class 'tweets) nil))
(execute-sql "DROP TABLE IF EXISTS tweets")
(defclass tweets ()
  ((id :col-type :serial
       :primary-key t)
   (status :col-type (or :text :null))
   (user :col-type (:varchar 64)))
  (:metaclass dao-table-class)
  (:unique-keys (id user)))

(subtest "first definition (no explicit primary key)"
  (defclass tweets ()
    ((user :col-type (:varchar 128)
           :accessor tweet-user))
    (:metaclass dao-table-class))
  (mapc #'execute-sql (table-definition 'tweets))

  (is (migration-expressions (find-class 'tweets) :sqlite3)
      nil
      "No migration at first")

  (defclass tweets ()
    ((user :col-type (:varchar 128)
           :accessor tweet-user))
    (:metaclass dao-table-class)
    (:table-name "tweets"))

  (is (migration-expressions (find-class 'tweets) :sqlite3)
      nil
      "No migration at first"))

(subtest "redefinition with :auto-pk nil"
  (defclass tweets ()
    ((user :col-type (:varchar 128)
           :accessor tweet-user))
    (:metaclass dao-table-class)
    (:auto-pk nil))

  (ok (migration-expressions (find-class 'tweets) :sqlite3)))

(subtest "redefinition (with explicit primary key)"
  (defclass tweets ()
    ((id :col-type :serial
         :primary-key t
         :reader tweet-id)
     (status :col-type :text
             :accessor tweet-status)
     (user :col-type (:varchar 128)
           :accessor tweet-user))
    (:metaclass dao-table-class))

  (ok (migration-expressions (find-class 'tweets) :sqlite3))

  (migrate-table (find-class 'tweets))

  (is (migration-expressions (find-class 'tweets) :sqlite3)
      nil
      "No migration after migrating"))

(subtest "redefinition"
  (defclass tweets ()
    ((id :col-type :serial
         :primary-key t
         :reader tweet-id)
     (user :col-type (:varchar 64)
           :accessor tweet-user))
    (:metaclass dao-table-class))

  (ok (migration-expressions (find-class 'tweets) :sqlite3))

  (migrate-table (find-class 'tweets))

  (is (migration-expressions (find-class 'tweets) :sqlite3)
      nil
      "No migration after migrating"))

(subtest "redefinition (modifying the column type)"
  (defclass tweets ()
    ((id :col-type :serial
         :primary-key t
         :reader tweet-id)
     (user :col-type (:varchar 128)
           :accessor tweet-user))
    (:metaclass dao-table-class))

  (ok (migration-expressions (find-class 'tweets) :sqlite3))

  (migrate-table (find-class 'tweets))

  (is (migration-expressions (find-class 'tweets) :sqlite3)
      nil
      "No migration after migrating"))

(subtest "redefinition of primary key (bigserial)"
  (defclass tweets ()
    ((id :col-type :bigserial
         :primary-key t
         :reader tweet-id)
     (user :col-type (:varchar 128)
           :accessor tweet-user))
    (:metaclass dao-table-class))

  (is (migration-expressions (find-class 'tweets) :sqlite3)
      nil
      "BIGSERIAL is same as SERIAL on SQLite3"))

(subtest "add :unique-keys"
  (defclass tweets ()
    ((id :col-type :bigserial
         :primary-key t
         :reader tweet-id)
     (user :col-type (:varchar 128)
           :accessor tweet-user))
    (:metaclass dao-table-class)
    (:unique-keys user))

  (ok (migration-expressions (find-class 'tweets) :sqlite3))

  (migrate-table (find-class 'tweets))

  (is (migration-expressions (find-class 'tweets) :sqlite3)
      nil
      "No migration after migrating"))

(subtest "modify :unique-keys"
  (defclass tweets ()
    ((id :col-type :bigserial
         :primary-key t
         :reader tweet-id)
     (user :col-type (:varchar 128)
           :accessor tweet-user))
    (:metaclass dao-table-class)
    (:unique-keys (id user)))

  (ok (migration-expressions (find-class 'tweets) :sqlite3))

  (migrate-table (find-class 'tweets))

  (is (migration-expressions (find-class 'tweets) :sqlite3)
      nil
      "No migration after migrating"))

(subtest "delete :unique-keys and add :keys"
  (defclass tweets ()
    ((id :col-type :bigserial
         :primary-key t
         :reader tweet-id)
     (user :col-type (:varchar 128)
           :accessor tweet-user))
    (:metaclass dao-table-class)
    (:keys user))

  (ok (migration-expressions (find-class 'tweets) :sqlite3))

  (migrate-table (find-class 'tweets))

  (is (migration-expressions (find-class 'tweets) :sqlite3)
      nil
      "No migration after migrating"))

(subtest "composite primary keys"
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

  (is (migration-expressions (find-class 'tweets-tag) :sqlite3)
      nil
      "No migration after migrating"))

(finalize)

(disconnect-toplevel)
