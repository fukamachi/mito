(in-package :cl-user)
(defpackage mito.migration
  (:use #:cl)
  (:import-from #:mito.dao
                #:dao-table-class)
  (:import-from #:mito.class
                #:database-column-slots
                #:table-name
                #:table-column-info
                #:table-indices-info)
  (:import-from #:mito.db
                #:table-indices
                #:column-definitions)
  (:import-from #:mito.connection
                #:*connection*
                #:driver-type
                #:check-connected)
  (:import-from #:mito.type
                #:get-column-real-name)
  (:import-from #:mito.util
                #:list-diff)
  (:import-from #:sxql
                #:make-statement
                #:drop-column
                #:add-column)
  (:export #:migrate-table))
(in-package :mito.migration)

(defgeneric migrate-table (class)
  (:method ((class symbol))
    (migrate-table (find-class class)))
  (:method ((class dao-table-class))
    (check-connected)
    ;; TODO
    (migration-expressions class (driver-type *connection*))))

(defun migration-expressions (class driver-type)
  (let* ((table-name (table-name class))
         (table-columns
           (mapcar (lambda (column)
                     (let ((info (table-column-info column driver-type)))
                       (setf (getf (cdr info) :type)
                             (get-column-real-name *connection* (getf (cdr info) :type)))
                       info))
                   (database-column-slots class)))
         (table-indices (table-indices-info class driver-type))
         (db-columns (column-definitions *connection* table-name))
         (db-indices (table-indices *connection* table-name)))
    (multiple-value-bind (intersection to-delete to-add)
        (list-diff db-columns table-columns
                   :key #'car)
      ;; TODO: take care of the order of columns
      (append
       ;; add columns
       (when to-add
         (list (apply #'sxql:make-statement :alter-table (intern table-name :keyword)
                      (mapcar (lambda (column)
                                (sxql:add-column (intern (car column) :keyword)))
                              to-add))))
       ;; drop columns
       (when to-delete
         (list (apply #'sxql:make-statement :alter-table (intern table-name :keyword)
                      (mapcar (lambda (column)
                                (sxql:drop-column (intern (car column) :keyword)))
                              to-delete))))
       ;; change columns
       (when intersection
         (loop for db-column in intersection
               for table-column = (find (car db-column) table-columns :test #'string= :key #'car)
               unless (equalp db-column table-column)
                 collect (apply #'sxql:make-clause :alter-column table-column) into clauses
               finally
                  (return (and clauses
                               (list (apply #'sxql:make-statement :alter-table (intern table-name :keyword)
                                            clauses))))))))
    (multiple-value-bind (intersection to-delete to-add)
        (list-diff db-indices table-indices
                   :key #'cdr
                   :test #'equalp
                   :sort-fn (constantly t))
      (declare (ignore intersection))
      ;; TODO: deleting indices must be done first.
      (append
       (when to-delete
         (list (apply #'sxql:make-statement :alter-table (intern table-name :keyword)
                      (loop for (index-name . options) in to-delete
                            if (getf options :primary-key)
                              collect (sxql:drop-primary-key)
                            else
                              collect (apply #'sxql:make-clause :drop-index
                                             index-name
                                             :on (list* (intern table-name :keyword)
                                                        (getf options :columns)))))))
       (when to-add
         (list (apply #'sxql:make-statement :alter-table (intern table-name :keyword)
                      (loop for (index-name . options) in to-add
                            if (getf options :primary-key)
                              collect (apply #'sxql:make-clause :add-primary-key (getf options :columns))
                            else
                              collect (apply #'sxql:make-clause :create-index
                                             index-name
                                             :unique (getf options :unique-key)
                                             :on (list* (intern table-name :keyword)
                                                        (getf options :columns)))))))))))
