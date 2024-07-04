(defpackage #:mito-test.migration.sqlite3
  (:use #:cl
        #:rove
        #:mito
        #:mito.migration
        #:mito-test.util))
(in-package #:mito-test.migration.sqlite3)

(setup
 (setf *connection* (reconnect-to-testdb (connect-to-testdb :sqlite3)))

 (when (find-class 'tweets nil)
   (setf (find-class 'tweets) nil))
 (execute-sql "DROP TABLE IF EXISTS tweets"))

(defclass tweets ()
  ((id :col-type :serial
       :primary-key t)
   (status :col-type (or :text :null))
   (user :col-type (:varchar 64)))
  (:metaclass dao-table-class)
  (:unique-keys (id user)))

(deftest sqlite3-migration-tests
  (testing "first definition (no explicit primary key)"
    (defclass tweets ()
      ((user :col-type (:varchar 128)
             :accessor tweet-user))
      (:metaclass dao-table-class))
    (mapc #'execute-sql (table-definition 'tweets))

    (ok (null (migration-expressions (find-class 'tweets) :sqlite3))
        "No migration at first")

    (defclass tweets ()
      ((user :col-type (:varchar 128)
             :accessor tweet-user))
      (:metaclass dao-table-class)
      (:table-name "tweets"))

    (ok (null (migration-expressions (find-class 'tweets) :sqlite3))
        "No migration at first"))

  (testing "redefinition with :auto-pk nil"
    (defclass tweets ()
      ((user :col-type (:varchar 128)
             :accessor tweet-user))
      (:metaclass dao-table-class)
      (:auto-pk nil))

    (ok (migration-expressions (find-class 'tweets) :sqlite3)))

  (testing "redefinition (with explicit primary key)"
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

    (ok (null (migration-expressions (find-class 'tweets) :sqlite3))
        "No migration after migrating"))

  (testing "redefinition"
    (defclass tweets ()
      ((id :col-type :serial
           :primary-key t
           :reader tweet-id)
       (user :col-type (:varchar 64)
             :accessor tweet-user))
      (:metaclass dao-table-class))

    (ok (migration-expressions (find-class 'tweets) :sqlite3))

    (migrate-table (find-class 'tweets))

    (ok (null (migration-expressions (find-class 'tweets) :sqlite3))
        "No migration after migrating"))

  (testing "redefinition (modifying the column type)"
    (defclass tweets ()
      ((id :col-type :serial
           :primary-key t
           :reader tweet-id)
       (user :col-type (:varchar 128)
             :accessor tweet-user))
      (:metaclass dao-table-class))

    (ok (migration-expressions (find-class 'tweets) :sqlite3))

    (migrate-table (find-class 'tweets))

    (ok (null (migration-expressions (find-class 'tweets) :sqlite3))
        "No migration after migrating"))

  (testing "redefinition of primary key (bigserial)"
    (defclass tweets ()
      ((id :col-type :bigserial
           :primary-key t
           :reader tweet-id)
       (user :col-type (:varchar 128)
             :accessor tweet-user))
      (:metaclass dao-table-class))

    (ok (null (migration-expressions (find-class 'tweets) :sqlite3))
        "BIGSERIAL is same as SERIAL on SQLite3"))

  (testing "add :unique-keys"
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

    (ok (null (migration-expressions (find-class 'tweets) :sqlite3))
        "No migration after migrating"))

  (testing "modify :unique-keys"
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

    (ok (null (migration-expressions (find-class 'tweets) :sqlite3))
        "No migration after migrating"))

  (testing "delete :unique-keys and add :keys"
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

    (ok (null (migration-expressions (find-class 'tweets) :sqlite3))
        "No migration after migrating"))

  (testing "composite primary keys"
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

    (ok (null (migration-expressions (find-class 'tweets-tag) :sqlite3))
        "No migration after migrating")))

(teardown
 (disconnect-toplevel))
