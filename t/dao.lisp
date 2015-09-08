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
                  "CREATE TABLE tweet (%oid SERIAL NOT NULL PRIMARY KEY, status TEXT, user INTEGER)")

  ;; add original primary-key
  (is-table-class :mysql
                  (defclass tweet ()
                    ((id :col-type :serial
                         :primary-key t)
                     (status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass dao-table-class))
                  "CREATE TABLE tweet (id SERIAL NOT NULL PRIMARY KEY, status TEXT, user INTEGER)")

  ;; redefinition
  (is-table-class :mysql
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass dao-table-class))
                  "CREATE TABLE tweet (%oid SERIAL NOT NULL PRIMARY KEY, status TEXT, user INTEGER)"))

(finalize)
