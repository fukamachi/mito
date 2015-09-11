(in-package :cl-user)
(defpackage mito-test.db.mysql
  (:use #:cl
        #:dbi
        #:mito-test.util
        #:mito-test.db.main))
(in-package :mito-test.db.mysql)

(defvar *conn* nil)
(setf *conn* (connect-to-testdb :mysql))

(dbi:do-sql *conn* "DROP TABLE IF EXISTS tweets")
(dbi:do-sql *conn*
  "CREATE TABLE tweets (
    id INTEGER AUTO_INCREMENT PRIMARY KEY NOT NULL,
    status TEXT NOT NULL,
    user VARCHAR(64) NOT NULL,
    UNIQUE (id, user)
)")

(dbi:do-sql *conn* "DROP TABLE IF EXISTS users")
(dbi:do-sql *conn*
  "CREATE TABLE users (
    id INTEGER AUTO_INCREMENT PRIMARY KEY NOT NULL,
    first_name VARCHAR(64) NOT NULL,
    family_name VARCHAR(64) NOT NULL,
    UNIQUE(first_name, family_name)
)")

(run-db-tests *conn*)

(disconnect-from-testdb *conn*)
