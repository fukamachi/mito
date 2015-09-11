(in-package :cl-user)
(defpackage mito-test.dao
  (:use #:cl
        #:prove
        #:mito.dao
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
                  "CREATE TABLE tweet (%oid BIGINT UNSIGNED NOT NULL AUTO_INCREMENT, status TEXT, user INTEGER, PRIMARY KEY (%oid))"
                  "auto-pk")

  (is-table-class :mysql
                  (defclass tweet ()
                    ((id :col-type :serial
                         :primary-key t)
                     (status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass dao-table-class))
                  "CREATE TABLE tweet (id INT UNSIGNED NOT NULL AUTO_INCREMENT, status TEXT, user INTEGER, PRIMARY KEY (id))"
                  "add original PK")

  (is-table-class :mysql
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass dao-table-class))
                  "CREATE TABLE tweet (%oid BIGINT UNSIGNED NOT NULL AUTO_INCREMENT, status TEXT, user INTEGER, PRIMARY KEY (%oid))"
                  "redefinition w/o PK")

  (is-table-class :mysql
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass dao-table-class)
                    (:auto-pk nil))
                  "CREATE TABLE tweet (status TEXT, user INTEGER)"
                  "auto-pk is nil"))

(finalize)
