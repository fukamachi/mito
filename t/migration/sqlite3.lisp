(in-package :cl-user)
(defpackage mito-test.migration.sqlite3
  (:use #:cl
        #:prove
        #:mito
        #:mito-test.util))
(in-package :mito-test.migration.sqlite3)

(plan nil)

(defvar *conn* nil)
(setf *conn* (reconnect-to-testdb (connect-to-testdb :sqlite3)))

(dbi:do-sql *conn*
  "CREATE TABLE tweets (
    id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    status TEXT,
    user VARCHAR(64) NOT NULL,
    UNIQUE (id, user)
)")

(when (find-class 'tweets nil)
  (setf (find-class 'tweets) nil))
(defclass tweets ()
  ((id :col-type :serial
       :primary-key t)
   (status :col-type :text)
   (user :col-type (:varchar 64)
         :not-null t))
  (:metaclass dao-table-class)
  (:unique-keys (id user)))

(with-connection *conn*
  (is (mito.migration::migration-expressions (find-class 'tweets) :sqlite3) nil
      "Nothing to migrate"))

(finalize)

(dbi:disconnect *conn*)
