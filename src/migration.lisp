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
                #:column-definitions
                #:execute-sql)
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
                #:make-clause
                #:drop-column
                #:drop-primary-key
                #:create-index
                #:drop-index)
  (:import-from #:alexandria
                #:make-keyword)
  (:export #:migrate-table))
(in-package :mito.migration)

(defgeneric migrate-table (class)
  (:method ((class symbol))
    (migrate-table (find-class class)))
  (:method ((class dao-table-class))
    (check-connected)
    (destructuring-bind (add-columns
                         drop-columns
                         change-columns
                         add-indices
                         drop-indices)
        (migration-expressions class (driver-type *connection*))
      (when drop-indices
        (mapc #'execute-sql drop-indices))
      (when drop-columns
        (execute-sql drop-columns))
      (when add-columns
        (execute-sql add-columns))
      (when change-columns
        (execute-sql change-columns))
      (when add-indices
        (mapc #'execute-sql add-indices)))))

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
    (multiple-value-bind (columns-intersection
                          columns-to-delete
                          columns-to-add)
        (list-diff db-columns table-columns
                   :key #'car)

      (multiple-value-bind (indices-intersection
                            indices-to-delete
                            indices-to-add)
          (list-diff db-indices table-indices
                     :key #'cdr
                     :test #'equalp
                     :sort-fn (constantly t))
        (declare (ignore indices-intersection))
        ;; TODO: take care of the order of columns
        (list
         ;; add columns
         (if columns-to-add
             (apply #'sxql:make-statement :alter-table (intern table-name :keyword)
                    (mapcar (lambda (column)
                              (apply #'sxql:make-clause :add-column (intern (car column) :keyword)
                                     (cdr column)))
                            columns-to-add))
             nil)
         ;; drop columns
         (if columns-to-delete
             (apply #'sxql:make-statement :alter-table (intern table-name :keyword)
                    (mapcar (lambda (column)
                              (sxql:drop-column (intern (car column) :keyword)))
                            columns-to-delete))
             nil)
         ;; change columns
         (if columns-intersection
             (loop for db-column in columns-intersection
                   for table-column = (find (car db-column) table-columns :test #'string= :key #'car)
                   unless (equalp db-column table-column)
                     collect (progn
                               ;; Don't add PRIMARY KEY if the column is already the primary key
                               (when (getf (cdr db-column) :primary-key)
                                 (setf (getf (cdr table-column) :primary-key) nil))
                               (apply #'sxql:make-clause :modify-column (intern (car table-column) :keyword)
                                      (cdr table-column))) into clauses
                   finally
                      (return (and clauses
                                   (apply #'sxql:make-statement :alter-table (intern table-name :keyword)
                                          clauses))))
             nil)
         ;; add indices
         (if indices-to-add
             (loop for (index-name . options) in indices-to-add
                   if (getf options :primary-key)
                     append
                   ;; Ignore if the columns are just added.
                     (if (or (cdr (getf options :columns))
                             (not (find (first (getf options :columns))
                                        columns-to-add
                                        :key #'car
                                        :test #'string=)))
                         (list
                          (sxql:make-statement :alter-table (intern table-name :keyword)
                                               (apply #'sxql:make-clause :add-primary-key
                                                      (mapcar #'make-keyword (getf options :columns)))))
                         nil)
                   else
                     collect (sxql:create-index
                              index-name
                              :unique (getf options :unique-key)
                              :on (list* (intern table-name :keyword)
                                         (mapcar #'make-keyword (getf options :columns)))))
             nil)
         ;; drop indices
         (if indices-to-delete
             (loop for (index-name . options) in indices-to-delete
                   ;; Ignore if the index's columns are going to be dropped.
                   unless (every (lambda (col)
                                   (find col columns-to-delete
                                         :key #'car
                                         :test #'string=))
                                 (getf options :columns))
                     collect
                     (if (getf options :primary-key)
                         (sxql:make-statement :alter-table (intern table-name :keyword)
                                              (sxql:drop-primary-key))
                         (sxql:drop-index index-name
                                          :on (intern table-name :keyword))))
             nil))))))
