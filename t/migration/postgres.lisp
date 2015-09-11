(in-package :cl-user)
(defpackage mito-test.migration.postgres
  (:use #:cl
        #:prove
        #:mito
        #:mito-test.util))
(in-package :mito-test.migration.postgres)

(plan nil)

(defvar *conn* nil)
(setf *conn* (connect-to-testdb :postgres))

(dbi:do-sql *conn* "DROP TABLE IF EXISTS tweets")
(dbi:do-sql *conn*
  "CREATE TABLE tweets (
    id SERIAL PRIMARY KEY NOT NULL,
    status TEXT,
    \"user\" VARCHAR(64) NOT NULL,
    UNIQUE (id, \"user\")
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
  (is (mito.migration::migration-expressions (find-class 'tweets) :postgres) nil
      "Nothing to migrate"))

(finalize)

(dbi:disconnect *conn*)
