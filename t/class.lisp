(defpackage #:mito-test.class
  (:use #:cl
        #:rove
        #:mito.class
        #:mito-test.util))
(in-package #:mito-test.class)

(deftest create-table
  (testing "MySQL"
    (is-table-class :mysql
                    (defclass tweet ()
                      ((id :col-type :serial
                           :primary-key t)
                       (status :col-type :text)
                       (user :col-type :integer))
                      (:metaclass table-class))
                    "CREATE TABLE tweet (
    id INT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
    status TEXT NOT NULL,
    user INTEGER NOT NULL
)")
    (is-table-class :mysql
                    (defclass tweet ()
                      ((id :col-type :bigserial
                           :primary-key t)
                       (status :col-type :text)
                       (user :col-type :integer))
                      (:metaclass table-class))
                    "CREATE TABLE tweet (
    id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
    status TEXT NOT NULL,
    user INTEGER NOT NULL
)"
                    "BIGSERIAL")
    (is-table-class :mysql
                    (defclass tweet ()
                      ((status :col-type :text)
                       (user :col-type :integer)
                       (created-at :col-type :datetime))
                      (:metaclass table-class)
                      (:primary-key user created-at))
                    "CREATE TABLE tweet (
    status TEXT NOT NULL,
    user INTEGER NOT NULL,
    created_at DATETIME NOT NULL,
    PRIMARY KEY (user, created_at)
)"
                    "PRIMARY KEY")
    (is-table-class :mysql
                    (defclass tweet ()
                      ((status :col-type :text)
                       (user :col-type :integer)
                       (created-at :col-type :datetime))
                      (:metaclass table-class)
                      (:unique-keys (user created-at)))
                    "CREATE TABLE tweet (
    status TEXT NOT NULL,
    user INTEGER NOT NULL,
    created_at DATETIME NOT NULL,
    UNIQUE (user, created_at)
)"
                    "UNIQUE KEY")
    (is-table-class :mysql
                    (defclass tweet ()
                      ((status :col-type :text)
                       (user :col-type :integer)
                       (created-at :col-type :datetime))
                      (:metaclass table-class)
                      (:keys (user created-at)))
                    "CREATE TABLE tweet (
    status TEXT NOT NULL,
    user INTEGER NOT NULL,
    created_at DATETIME NOT NULL,
    KEY (user, created_at)
)"
                    "KEY")
    (is-table-class :mysql
                    (defclass tweet ()
                      ((status :col-type :text)
                       (user :col-type :integer)
                       (created-at :col-type (or :datetime :null)))
                      (:metaclass table-class))
                    "CREATE TABLE tweet (
    status TEXT NOT NULL,
    user INTEGER NOT NULL,
    created_at DATETIME
)"
                    "NULL"))

  (testing "PostgreSQL"
    (is-table-class :postgres
                    (defclass tweet ()
                      ((id :col-type :serial
                           :primary-key t)
                       (status :col-type :text)
                       (user :col-type :integer))
                      (:metaclass table-class))
                    "CREATE TABLE tweet (
    id SERIAL NOT NULL PRIMARY KEY,
    status TEXT NOT NULL,
    user INTEGER NOT NULL
)")
    (is-table-class :postgres
                    (defclass tweet ()
                      ((id :col-type :bigserial
                           :primary-key t)
                       (status :col-type :text)
                       (user :col-type :integer))
                      (:metaclass table-class))
                    "CREATE TABLE tweet (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    status TEXT NOT NULL,
    user INTEGER NOT NULL
)")
    (is-table-class :postgres
                    (defclass tweet ()
                      ((status :col-type :text)
                       (user :col-type :integer)
                       (created-at :col-type :datetime))
                      (:metaclass table-class)
                      (:primary-key user created-at))
                    "CREATE TABLE tweet (
    status TEXT NOT NULL,
    user INTEGER NOT NULL,
    created_at TIMESTAMP NOT NULL,
    PRIMARY KEY (user, created_at)
)"
                    "PRIMARY KEY")
    (is-table-class :postgres
                    (defclass tweet ()
                      ((status :col-type :text)
                       (user :col-type :integer)
                       (created-at :col-type :datetime))
                      (:metaclass table-class)
                      (:unique-keys (user created-at)))
                    '("CREATE TABLE tweet (
    status TEXT NOT NULL,
    user INTEGER NOT NULL,
    created_at TIMESTAMP NOT NULL
)"
                      "CREATE UNIQUE INDEX unique_tweet_user_created_at ON tweet (user, created_at)")
                    "UNIQUE KEY")
    (is-table-class :postgres
                    (defclass tweet ()
                      ((status :col-type :text)
                       (user :col-type :integer)
                       (created-at :col-type :datetime))
                      (:metaclass table-class)
                      (:keys (user created-at)))
                    '("CREATE TABLE tweet (
    status TEXT NOT NULL,
    user INTEGER NOT NULL,
    created_at TIMESTAMP NOT NULL
)" "CREATE INDEX key_tweet_user_created_at ON tweet (user, created_at)")
                    "KEY")
    (is-table-class :postgres
                    (defclass tweet ()
                      ((status :col-type :text)
                       (user :col-type :integer)
                       (created-at :col-type (or :datetime :null)))
                      (:metaclass table-class))
                    "CREATE TABLE tweet (
    status TEXT NOT NULL,
    user INTEGER NOT NULL,
    created_at TIMESTAMP
)"
                    "NULL"))

  (testing "SQLite3"
    (is-table-class :sqlite3
                    (defclass tweet ()
                      ((id :col-type :serial
                           :primary-key t)
                       (status :col-type :text)
                       (user :col-type :integer))
                      (:metaclass table-class))
                    "CREATE TABLE tweet (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    status TEXT NOT NULL,
    user INTEGER NOT NULL
)")
    ;; bigserial
    (is-table-class :sqlite3
                    (defclass tweet ()
                      ((id :col-type :bigserial
                           :primary-key t)
                       (status :col-type :text)
                       (user :col-type :integer))
                      (:metaclass table-class))
                    "CREATE TABLE tweet (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    status TEXT NOT NULL,
    user INTEGER NOT NULL
)")
    (is-table-class :sqlite3
                    (defclass tweet ()
                      ((status :col-type :text)
                       (user :col-type :integer)
                       (created-at :col-type :datetime))
                      (:metaclass table-class)
                      (:primary-key user created-at))
                    "CREATE TABLE tweet (
    status TEXT NOT NULL,
    user INTEGER NOT NULL,
    created_at DATETIME NOT NULL,
    PRIMARY KEY (user, created_at)
)"
                    "PRIMARY KEY")
    (is-table-class :sqlite3
                    (defclass tweet ()
                      ((status :col-type :text)
                       (user :col-type :integer)
                       (created-at :col-type :datetime))
                      (:metaclass table-class)
                      (:unique-keys (user created-at)))
                    "CREATE TABLE tweet (
    status TEXT NOT NULL,
    user INTEGER NOT NULL,
    created_at DATETIME NOT NULL,
    UNIQUE (user, created_at)
)"
                    "UNIQUE KEY")
    (is-table-class :sqlite3
                    (defclass tweet ()
                      ((status :col-type :text)
                       (user :col-type :integer)
                       (created-at :col-type :datetime))
                      (:metaclass table-class)
                      (:keys (user created-at)))
                    '("CREATE TABLE tweet (
    status TEXT NOT NULL,
    user INTEGER NOT NULL,
    created_at DATETIME NOT NULL
)"
                      "CREATE INDEX key_tweet_user_created_at ON tweet (user, created_at)")
                    "KEY")
    (is-table-class :sqlite3
                    (defclass tweet ()
                      ((status :col-type :text)
                       (user :col-type :integer)
                       (created-at :col-type (or :datetime :null)))
                      (:metaclass table-class))
                    "CREATE TABLE tweet (
    status TEXT NOT NULL,
    user INTEGER NOT NULL,
    created_at DATETIME
)"
                    "NULL")))

(deftest references
  (defclass user ()
    ((name :col-type (:varchar 64)
           :primary-key t))
    (:metaclass table-class))
  (is-table-class :mysql
                  (defclass tweet ()
                    ((user :col-type user))
                    (:metaclass table-class))
                  "CREATE TABLE tweet (
    user_name VARCHAR(64) NOT NULL
)")
  (is-table-class :mysql
                  (defclass tweet ()
                    ((user-name :references (user name) :col-type (:or :null :text)))
                    (:metaclass table-class))
                  "CREATE TABLE tweet (
    user_name VARCHAR(64) NOT NULL
)")
  (is-table-class :mysql
                  (defclass tweet ()
                    ((id :col-type :bigserial
                         :primary-key t)
                     (user :col-type user))
                    (:metaclass table-class)
                    (:unique-keys user))
                  "CREATE TABLE tweet (
    id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
    user_name VARCHAR(64) NOT NULL,
    UNIQUE (user_name)
)")

  (is-table-class :mysql
                  (defclass tweet-tags ()
                    ((tweet1 :col-type tweet)
                     (tweet2 :col-type tweet)
                     (uuid :col-type (:varchar 40)))
                    (:metaclass table-class)
                    (:unique-keys (tweet1 tweet2 uuid)))
                  "CREATE TABLE tweet_tags (
    tweet1_id BIGINT UNSIGNED NOT NULL,
    tweet2_id BIGINT UNSIGNED NOT NULL,
    uuid VARCHAR(40) NOT NULL,
    UNIQUE (tweet1_id, tweet2_id, uuid)
)")

  (is-table-class :mysql
                  (defclass tweet-tags ()
                    ((tweet1 :col-type (or tweet :null)))
                    (:metaclass table-class))
                  "CREATE TABLE tweet_tags (
    tweet1_id BIGINT UNSIGNED
)"))

(deftest self-reference
  (is-table-class :mysql
                  (defclass category ()
                    ((parent :col-type category
                             :initarg :parent
                             :accessor parent))
                    (:metaclass mito:dao-table-class))
                  "CREATE TABLE category (
    id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
    parent_id BIGINT UNSIGNED NOT NULL,
    created_at TIMESTAMP,
    updated_at TIMESTAMP
)")
  (is-table-class :postgres
                  (defclass category ()
                    ((parent :col-type category
                             :initarg :parent
                             :accessor parent))
                    (:metaclass mito:dao-table-class))
                  "CREATE TABLE category (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    parent_id BIGINT NOT NULL,
    created_at TIMESTAMPTZ,
    updated_at TIMESTAMPTZ
)"))
