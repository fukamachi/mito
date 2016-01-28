(in-package :cl-user)
(defpackage mito-test.dao
  (:use #:cl
        #:prove
        #:mito.dao
        #:mito.connection
        #:mito-test.util))
(in-package :mito-test.dao)

(plan nil)

(subtest "dao-table-class inheritance"

  (when (find-class 'tweet nil)
    (setf (find-class 'tweet) nil))

  (defclass tweet () ()
    (:metaclass dao-table-class))

  (is (c2mop:class-direct-superclasses (find-class 'tweet))
      (list (find-class 'dao-class))
      "dao-table-class inherits dao-table implicitly")

  (defclass my-dao-class (dao-class) ())

  (defclass tweet (my-dao-class) ()
    (:metaclass dao-table-class))

  (is (c2mop:class-direct-superclasses (find-class 'tweet))
      (list (find-class 'my-dao-class))
      "Not inherit dao-class directly")

  (is-table-class :mysql
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass dao-table-class))
                  "CREATE TABLE tweet (%oid BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY, status TEXT, user INTEGER)"
                  "auto-pk")

  (is-table-class :mysql
                  (defclass tweet ()
                    ((id :col-type :serial
                         :primary-key t)
                     (status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass dao-table-class))
                  "CREATE TABLE tweet (id INT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY, status TEXT, user INTEGER)"
                  "add original PK")

  (is-table-class :mysql
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass dao-table-class))
                  "CREATE TABLE tweet (%oid BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY, status TEXT, user INTEGER)"
                  "redefinition w/o PK")

  (is-table-class :mysql
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass dao-table-class)
                    (:auto-pk nil))
                  "CREATE TABLE tweet (status TEXT, user INTEGER)"
                  "auto-pk is nil"))

(subtest "relation"
  (setf *connection* (connect-to-testdb :mysql))
  (when (find-class 'user nil)
    (setf (find-class 'user) nil))
  (when (find-class 'tweet nil)
    (setf (find-class 'tweet) nil))

  (defclass user ()
    ((id :col-type :serial
         :not-null t
         :primary-key t)
     (name :col-type :text
           :not-null t
           :initarg :name))
    (:metaclass dao-table-class))

  (defclass tweet ()
    ((status :col-type :text
             :initarg :status
             :accessor tweet-status)
     (user :col-type user
           :initarg :user
           :accessor tweet-user))
    (:metaclass dao-table-class))

  (is (sxql:yield (table-definition 'tweet))
      "CREATE TABLE tweet (%oid BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY, status TEXT, user_id INT UNSIGNED)")
  (mito:execute-sql "DROP TABLE IF EXISTS user")
  (mito:execute-sql "DROP TABLE IF EXISTS tweet")
  (mito:ensure-table-exists 'user)
  (mito:ensure-table-exists 'tweet)
  (let ((user (mito:create-dao 'user :name "Eitaro")))
    (mito:create-dao 'tweet :status "Hello" :user user))

  (let ((tweets (mito:select-dao 'tweet)))
    (is (length tweets) 1)
    (is-type (first tweets) 'tweet)
    (is-type (tweet-user (first tweets)) 'user))

  (disconnect-toplevel))

(finalize)
