(in-package :cl-user)
(defpackage mito.class
  (:use #:cl
        #:mito.class.column
        #:mito.class.table)
  (:import-from #:sxql
                #:make-statement
                #:primary-key
                #:unique-key
                #:index-key)
  (:export #:create-table-sxql

           #:table-class
           #:table-name
           #:table-column-info
           #:table-indices-info))
(in-package :mito.class)

(defgeneric create-table-sxql (class driver-type &key if-not-exists)
  (:method (class driver-type &key if-not-exists)
    (apply #'sxql:make-statement
           :create-table
           (list (intern (table-name class) :keyword)
            :if-not-exists if-not-exists)
           (mapcar (lambda (column)
                     (table-column-info-for-create-table column driver-type))
                   (database-column-slots class))
           (mapcar (lambda (index)
                     (cond
                       ((getf index :primary-key)
                        (sxql:primary-key (getf index :columns)))
                       ((getf index :unique-key)
                        (sxql:unique-key (getf index :columns)))
                       (t
                        (sxql:index-key (getf index :columns)))))
                   (table-indices-info class driver-type)))))
