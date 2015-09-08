(in-package :cl-user)
(defpackage mito-test.class
  (:use #:cl
        #:prove
        #:mito.class
        #:mito-test.util))
(in-package :mito-test.class)

(plan nil)

(subtest "create-table (MySQL)"
  (is-table-class :mysql
                  (defclass tweet ()
                    ((id :col-type :serial
                         :primary-key t)
                     (status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass table-class))
                  "CREATE TABLE tweet (id SERIAL NOT NULL PRIMARY KEY, status TEXT, user INTEGER)")
  (is-table-class :mysql
                  (defclass tweet ()
                    ((id :col-type :bigserial
                         :primary-key t)
                     (status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass table-class))
                  "CREATE TABLE tweet (id SERIAL NOT NULL PRIMARY KEY, status TEXT, user INTEGER)"
                  "BIGSERIAL is the same as SERIAL in MySQL")
  (is-table-class :mysql
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer)
                     (created-at :col-type :datetime))
                    (:metaclass table-class)
                    (:primary-key user created-at))
                  "CREATE TABLE tweet (status TEXT, user INTEGER, created_at DATETIME, PRIMARY KEY (user, created_at))"
                  "PRIMARY KEY")
  (is-table-class :mysql
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer)
                     (created-at :col-type :datetime))
                    (:metaclass table-class)
                    (:unique-keys (user created-at)))
                  "CREATE TABLE tweet (status TEXT, user INTEGER, created_at DATETIME, UNIQUE (user, created_at))"
                  "UNIQUE KEY")
  (is-table-class :mysql
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer)
                     (created-at :col-type :datetime))
                    (:metaclass table-class)
                    (:keys (user created-at)))
                  "CREATE TABLE tweet (status TEXT, user INTEGER, created_at DATETIME, KEY (user, created_at))"
                  "KEY"))

(subtest "create-table (PostgreSQL)"
  (is-table-class :postgres
                  (defclass tweet ()
                    ((id :col-type :serial
                         :primary-key t)
                     (status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass table-class))
                  "CREATE TABLE tweet (id SERIAL NOT NULL PRIMARY KEY, status TEXT, user INTEGER)")
  (is-table-class :postgres
                  (defclass tweet ()
                    ((id :col-type :bigserial
                         :primary-key t)
                     (status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass table-class))
                  "CREATE TABLE tweet (id BIGSERIAL NOT NULL PRIMARY KEY, status TEXT, user INTEGER)")
  (is-table-class :postgres
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer)
                     (created-at :col-type :datetime))
                    (:metaclass table-class)
                    (:primary-key user created-at))
                  "CREATE TABLE tweet (status TEXT, user INTEGER, created_at DATETIME, PRIMARY KEY (user, created_at))"
                  "PRIMARY KEY")
  (is-table-class :postgres
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer)
                     (created-at :col-type :datetime))
                    (:metaclass table-class)
                    (:unique-keys (user created-at)))
                  "CREATE TABLE tweet (status TEXT, user INTEGER, created_at DATETIME, UNIQUE (user, created_at))"
                  "UNIQUE KEY")
  (is-table-class :postgres
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer)
                     (created-at :col-type :datetime))
                    (:metaclass table-class)
                    (:keys (user created-at)))
                  "CREATE TABLE tweet (status TEXT, user INTEGER, created_at DATETIME, KEY (user, created_at))"
                  "KEY"))

(subtest "create-table (SQLite3)"
  (is-table-class :sqlite3
                  (defclass tweet ()
                    ((id :col-type :serial
                         :primary-key t)
                     (status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass table-class))
                  "CREATE TABLE tweet (id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, status TEXT, user INTEGER)")
  ;; bigserial
  (is-table-class :sqlite3
                  (defclass tweet ()
                    ((id :col-type :bigserial
                         :primary-key t)
                     (status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass table-class))
                  "CREATE TABLE tweet (id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, status TEXT, user INTEGER)")
  (is-table-class :sqlite3
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer)
                     (created-at :col-type :datetime))
                    (:metaclass table-class)
                    (:primary-key user created-at))
                  "CREATE TABLE tweet (status TEXT, user INTEGER, created_at DATETIME, PRIMARY KEY (user, created_at))"
                  "PRIMARY KEY")
  (is-table-class :sqlite3
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer)
                     (created-at :col-type :datetime))
                    (:metaclass table-class)
                    (:unique-keys (user created-at)))
                  "CREATE TABLE tweet (status TEXT, user INTEGER, created_at DATETIME, UNIQUE (user, created_at))"
                  "UNIQUE KEY")
  (is-table-class :sqlite3
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer)
                     (created-at :col-type :datetime))
                    (:metaclass table-class)
                    (:keys (user created-at)))
                  "CREATE TABLE tweet (status TEXT, user INTEGER, created_at DATETIME)"
                  "KEY (ignored)"))

(finalize)
