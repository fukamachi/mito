(in-package :cl-user)
(defpackage mito-test.migration.mysql
  (:use #:cl
        #:prove
        #:mito
        #:mito-test.util))
(in-package :mito-test.migration.mysql)

(plan nil)

(defvar *conn* nil)
(setf *conn* (connect-to-testdb :mysql))

(dbi:do-sql *conn* "DROP TABLE IF EXISTS tweets")
(dbi:do-sql *conn*
  "CREATE TABLE tweets (
    id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY NOT NULL,
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
  (is (mito.migration::migration-expressions (find-class 'tweets) :mysql) nil
      "Nothing to migrate"))

(finalize)

(dbi:disconnect *conn*)
