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
                     (let ((column-info (table-column-info column driver-type)))
                       (rplaca column-info
                               (intern (car column-info) :keyword))
                       (case driver-type
                         ;; PostgreSQL use SERIAL instead of AUTO_INCREMENT
                         (:postgres
                          (setf (getf (cdr column-info) :auto-increment) nil))
                         ;; SQLite3 uses AUTOINCREMENT rather than AUTO_INCREMENT
                         (:sqlite3
                          (when (getf (cdr column-info) :auto-increment)
                            (rplaca (member :auto-increment (cdr column-info) :test #'eq)
                                    :autoincrement))))
                       column-info))
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
