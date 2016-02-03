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
                  "CREATE TABLE tweet (id INT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY, status TEXT NOT NULL, user INTEGER NOT NULL)")
  (is-table-class :mysql
                  (defclass tweet ()
                    ((id :col-type :bigserial
                         :primary-key t)
                     (status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass table-class))
                  "CREATE TABLE tweet (id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY, status TEXT NOT NULL, user INTEGER NOT NULL)"
                  "BIGSERIAL")
  (is-table-class :mysql
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer)
                     (created-at :col-type :datetime))
                    (:metaclass table-class)
                    (:primary-keys user created-at))
                  "CREATE TABLE tweet (status TEXT NOT NULL, user INTEGER NOT NULL, created_at DATETIME NOT NULL, PRIMARY KEY (user, created_at))"
                  "PRIMARY KEY")
  (is-table-class :mysql
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer)
                     (created-at :col-type :datetime))
                    (:metaclass table-class)
                    (:unique-keys (user created-at)))
                  "CREATE TABLE tweet (status TEXT NOT NULL, user INTEGER NOT NULL, created_at DATETIME NOT NULL, UNIQUE (user, created_at))"
                  "UNIQUE KEY")
  (is-table-class :mysql
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer)
                     (created-at :col-type :datetime))
                    (:metaclass table-class)
                    (:keys (user created-at)))
                  "CREATE TABLE tweet (status TEXT NOT NULL, user INTEGER NOT NULL, created_at DATETIME NOT NULL, KEY (user, created_at))"
                  "KEY")
  (is-table-class :mysql
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer)
                     (created-at :col-type (or :datetime :null)))
                    (:metaclass table-class))
                  "CREATE TABLE tweet (status TEXT NOT NULL, user INTEGER NOT NULL, created_at DATETIME)"
                  "NULL"))

(subtest "create-table (PostgreSQL)"
  (is-table-class :postgres
                  (defclass tweet ()
                    ((id :col-type :serial
                         :primary-key t)
                     (status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass table-class))
                  "CREATE TABLE tweet (id SERIAL NOT NULL PRIMARY KEY, status TEXT NOT NULL, user INTEGER NOT NULL)")
  (is-table-class :postgres
                  (defclass tweet ()
                    ((id :col-type :bigserial
                         :primary-key t)
                     (status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass table-class))
                  "CREATE TABLE tweet (id BIGSERIAL NOT NULL PRIMARY KEY, status TEXT NOT NULL, user INTEGER NOT NULL)")
  (is-table-class :postgres
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer)
                     (created-at :col-type :datetime))
                    (:metaclass table-class)
                    (:primary-keys user created-at))
                  "CREATE TABLE tweet (status TEXT NOT NULL, user INTEGER NOT NULL, created_at DATETIME NOT NULL, PRIMARY KEY (user, created_at))"
                  "PRIMARY KEY")
  (is-table-class :postgres
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer)
                     (created-at :col-type :datetime))
                    (:metaclass table-class)
                    (:unique-keys (user created-at)))
                  "CREATE TABLE tweet (status TEXT NOT NULL, user INTEGER NOT NULL, created_at DATETIME NOT NULL, UNIQUE (user, created_at))"
                  "UNIQUE KEY")
  (is-table-class :postgres
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer)
                     (created-at :col-type :datetime))
                    (:metaclass table-class)
                    (:keys (user created-at)))
                  "CREATE TABLE tweet (status TEXT NOT NULL, user INTEGER NOT NULL, created_at DATETIME NOT NULL, KEY (user, created_at))"
                  "KEY")
  (is-table-class :postgres
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer)
                     (created-at :col-type (or :datetime :null)))
                    (:metaclass table-class))
                  "CREATE TABLE tweet (status TEXT NOT NULL, user INTEGER NOT NULL, created_at DATETIME)"
                  "NULL"))

(subtest "create-table (SQLite3)"
  (is-table-class :sqlite3
                  (defclass tweet ()
                    ((id :col-type :serial
                         :primary-key t)
                     (status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass table-class))
                  "CREATE TABLE tweet (id INTEGER PRIMARY KEY AUTOINCREMENT, status TEXT NOT NULL, user INTEGER NOT NULL)")
  ;; bigserial
  (is-table-class :sqlite3
                  (defclass tweet ()
                    ((id :col-type :bigserial
                         :primary-key t)
                     (status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass table-class))
                  "CREATE TABLE tweet (id INTEGER PRIMARY KEY AUTOINCREMENT, status TEXT NOT NULL, user INTEGER NOT NULL)")
  (is-table-class :sqlite3
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer)
                     (created-at :col-type :datetime))
                    (:metaclass table-class)
                    (:primary-keys user created-at))
                  "CREATE TABLE tweet (status TEXT NOT NULL, user INTEGER NOT NULL, created_at DATETIME NOT NULL, PRIMARY KEY (user, created_at))"
                  "PRIMARY KEY")
  (is-table-class :sqlite3
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer)
                     (created-at :col-type :datetime))
                    (:metaclass table-class)
                    (:unique-keys (user created-at)))
                  "CREATE TABLE tweet (status TEXT NOT NULL, user INTEGER NOT NULL, created_at DATETIME NOT NULL, UNIQUE (user, created_at))"
                  "UNIQUE KEY")
  (is-table-class :sqlite3
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer)
                     (created-at :col-type :datetime))
                    (:metaclass table-class)
                    (:keys (user created-at)))
                  "CREATE TABLE tweet (status TEXT NOT NULL, user INTEGER NOT NULL, created_at DATETIME NOT NULL)"
                  "KEY (ignored)")
  (is-table-class :sqlite3
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer)
                     (created-at :col-type (or :datetime :null)))
                    (:metaclass table-class))
                  "CREATE TABLE tweet (status TEXT NOT NULL, user INTEGER NOT NULL, created_at DATETIME)"
                  "NULL"))

(finalize)
