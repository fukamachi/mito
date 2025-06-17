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
                #:appendf
                #:ensure-list)
  (:export #:create-table-sxql

           #:table-class
           #:table-column-class
           #:table-column-name
           #:table-column-type
           #:table-column-not-null-p
           #:table-column-slots
           #:table-direct-column-slots
           #:table-name
           #:table-primary-key
           #:table-serial-key
           #:database-column-slots
           #:table-column-info
           #:table-indices-info
           #:find-slot-by-name

           #:find-parent-column
           #:find-child-columns))
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
                           (unless (some #'primary-key-p (database-column-slots class))
                             (list (sxql:primary-key (mapcar #'sxql:make-sql-symbol (getf (cdr index) :columns))))))
                          ((getf (cdr index) :unique-key)
                           (if (eq driver-type :postgres)
                               (progn
                                 (appendf add-indices
                                          (list (sxql:create-index (sxql:make-sql-symbol
                                                                    (format nil "unique_~A_~{~A~^_~}"
                                                                            (table-name class)
                                                                            (getf (cdr index) :columns)))
                                                                   :on
                                                                   (cons (sxql:make-sql-symbol (table-name class))
                                                                         (mapcar #'sxql:make-sql-symbol (getf (cdr index) :columns)))
                                                                   :unique t)))
                                 nil)
                               (list (sxql:unique-key (mapcar #'sxql:make-sql-symbol (getf (cdr index) :columns))))))
                          (t
                           (if (eq driver-type :mysql)
                               (list (sxql:index-key (mapcar #'sxql:make-sql-symbol (getf (cdr index) :columns))))
                               (progn
                                 (appendf add-indices
                                          (list (sxql:create-index (sxql:make-sql-symbol
                                                                    (format nil "key_~A_~{~A~^_~}"
                                                                            (table-name class)
                                                                            (getf (cdr index) :columns)))
                                                                   :on
                                                                   (cons (sxql:make-sql-symbol (table-name class))
                                                                         (mapcar #'sxql:make-sql-symbol (getf (cdr index) :columns))))))
                                 nil)))))
                      (table-indices-info class driver-type)))
       add-indices))))

(defmethod table-column-references-column ((column table-column-class))
  (destructuring-bind (&optional ref-class-name ref-column-name)
      (ensure-list (table-column-references column))
    (when ref-class-name
      (let ((ref-class (find-class ref-class-name)))
        (find-slot-by-name ref-class
                           (or ref-column-name
                               (let ((pk-names (table-primary-key ref-class)))
                                 (unless pk-names
                                   (error "Foreign class ~S has no primary keys and no slot name is specified to :references"
                                          (class-name ref-class)))
                                 (when (cdr pk-names)
                                   (error "Foreign class ~S has a composite primary key and failed to detect which to use for :references"
                                          (class-name ref-class)))
                                 (first pk-names))))))))
