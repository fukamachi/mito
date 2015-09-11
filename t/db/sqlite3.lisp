(in-package :cl-user)
(defpackage mito-test.db.sqlite3
  (:use #:cl
        #:dbi
        #:mito-test.util
        #:mito-test.db.main))
(in-package :mito-test.db.sqlite3)

(defvar *conn* nil)
(setf *conn* (reconnect-to-testdb (connect-to-testdb :sqlite3)))

(dbi:do-sql *conn*
  "CREATE TABLE tweets (
    id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    status TEXT NOT NULL,
    user VARCHAR(64) NOT NULL,
    UNIQUE (id, user)
)")

(dbi:do-sql *conn*
  "CREATE TABLE users (
    id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
    first_name VARCHAR(64) NOT NULL,
    family_name VARCHAR(64) NOT NULL,
    UNIQUE(first_name, family_name)
)")

(run-db-tests *conn*)

(disconnect-from-testdb *conn*)
