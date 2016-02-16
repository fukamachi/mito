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
  (:import-from #:alexandria
                #:appendf)
  (:export #:create-table-sxql

           #:table-class
           #:table-column-class
           #:table-column-name
           #:table-column-slots
           #:table-direct-column-slots
           #:table-name
           #:table-primary-key
           #:table-serial-key
           #:database-column-slots
           #:table-column-info
           #:table-indices-info

           #:find-slot-by-name))
(in-package :mito.class)

(defgeneric create-table-sxql (class driver-type &key if-not-exists)
  (:method (class driver-type &key if-not-exists)
    (let ((add-indices '()))
      (cons
       (apply #'sxql:make-statement
              :create-table
              (list (sxql:make-sql-symbol (table-name class))
                    :if-not-exists if-not-exists)
              (mapcar (lambda (column)
                        (table-column-info-for-create-table column driver-type))
                      (database-column-slots class))
              (mapcan (lambda (index)
                        (cond
                          ((getf (cdr index) :primary-key)
                           (if (cdr (getf (cdr index) :columns))
                               (list (sxql:primary-key (mapcar #'sxql:make-sql-symbol (getf (cdr index) :columns))))
                               nil))
                          ((getf (cdr index) :unique-key)
                           (list (sxql:unique-key (mapcar #'sxql:make-sql-symbol (getf (cdr index) :columns)))))
                          (t
                           (if (eq driver-type :postgres)
                               (progn
                                 (appendf add-indices
                                          (list (sxql:create-index (sxql:make-sql-symbol
                                                                    (format nil "key_~A_~{~A~^_~}"
                                                                            (table-name class)
                                                                            (getf (cdr index) :columns)))
                                                                   :on
                                                                   (cons (sxql:make-sql-symbol (table-name class))
                                                                         (mapcar #'sxql:make-sql-symbol (getf (cdr index) :columns))))))
                                 nil)
                               (list (sxql:index-key (mapcar #'sxql:make-sql-symbol (getf (cdr index) :columns))))))))
                      (table-indices-info class driver-type)))
       add-indices))))

(defun find-slot-by-name (class slot-name &key (test #'eq))
  (find slot-name
        (table-column-slots (if (typep class 'symbol)
                                (find-class class)
                                class))
        :test test
        :key #'c2mop:slot-definition-name))
