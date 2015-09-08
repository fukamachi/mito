(in-package :cl-user)
(defpackage mito-test.class
  (:use #:cl
        #:prove
        #:mito.class))
(in-package :mito-test.class)

(plan nil)

(defmacro is-table-class (driver class-definition create-table)
  (let ((class (gensym "CLASS")))
    `(let ((,class ,class-definition))
       (is (let ((sxql:*use-placeholder* nil))
             (sxql:yield (create-table-sxql ,class ,driver)))
           ,create-table
           (format nil "~A (~S)" (class-name ,class) ,driver)))))

(subtest "create-table (MySQL)"
  (is-table-class :mysql
                  (defclass tweet ()
                    ((id :col-type :serial
                         :primary-key t)
                     (status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass table-class))
                  "CREATE TABLE tweet (id SERIAL NOT NULL PRIMARY KEY, status TEXT, user INTEGER)"))

(subtest "create-table (PostgreSQL)"
  (is-table-class :postgres
                  (defclass tweet ()
                    ((id :col-type :serial
                         :primary-key t)
                     (status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass table-class))
                  "CREATE TABLE tweet (id SERIAL NOT NULL PRIMARY KEY, status TEXT, user INTEGER)"))

(subtest "create-table (SQLite3)"
  (is-table-class :sqlite3
                  (defclass tweet ()
                    ((id :col-type :serial
                         :primary-key t)
                     (status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass table-class))
                  "CREATE TABLE tweet (id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, status TEXT, user INTEGER)"))

(finalize)
