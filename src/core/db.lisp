(in-package :cl-user)
(defpackage mito.db
  (:use #:cl)
  (:import-from #:mito.connection
                #:*connection*
                #:with-quote-char
                #:connection-quote-character
                #:check-connected)
  (:import-from #:mito.logger
                #:with-trace-sql)
  (:import-from #:mito.util
                #:lispify
                #:with-prepared-query
                #:execute-with-retry)
  (:import-from #:dbi
                #:connection-driver-type
                #:do-sql
                #:execute
                #:fetch-all)
  (:import-from #:dbi.driver
                #:query-row-count)
  (:import-from #:sxql
                #:*quote-character*
                #:yield)
  (:import-from #:sxql.sql-type
                #:sql-statement
                #:conjunctive-op)
  (:import-from #:sxql.composed-statement
                #:composed-statement)
  (:export #:*use-prepare-cached*
           #:last-insert-id
           #:table-indices
           #:column-definitions
           #:table-view-query
           #:table-exists-p
           #:execute-sql
           #:retrieve-by-sql))
(in-package :mito.db)

(defvar *use-prepare-cached* nil
  "EXPERIMENTAL FEATURE: If this is T, Mito uses DBI:PREPARE-CACHED
to retrieve/execute SQLs instead of DBI:PREPARE. The default value is NIL.
Note that DBI:PREPARE-CACHED is added CL-DBI v0.9.5.")

(defun last-insert-id (conn table-name serial-key-name)
  (check-type serial-key-name string)
  (let ((sxql:*quote-character* (connection-quote-character conn)))
    (ecase (dbi:connection-driver-type conn)
      (:mysql    (mito.db.mysql:last-insert-id conn table-name serial-key-name))
      (:postgres (mito.db.postgres:last-insert-id conn table-name serial-key-name))
      (:sqlite3  (mito.db.sqlite3:last-insert-id conn table-name)))))

(defun table-indices (conn table-name)
  (sort
   (funcall
    (ecase (dbi:connection-driver-type conn)
      (:mysql    #'mito.db.mysql:table-indices)
      (:postgres #'mito.db.postgres:table-indices)
      (:sqlite3  #'mito.db.sqlite3:table-indices))
    conn table-name)
   (lambda (a b)
     (cond
       ((getf a :primary-key)
        (not (getf b :primary-key)))
       ((getf b :primary-key) nil)
       ((getf a :unique-key)
        (or (not (getf b :unique-key))
            (string< (prin1-to-string a) (prin1-to-string b))))
       (t
        (string< (prin1-to-string a) (prin1-to-string b)))))
   :key #'cdr))

(defun column-definitions (conn table-name)
  (funcall
   (ecase (dbi:connection-driver-type conn)
     (:mysql    #'mito.db.mysql:column-definitions)
     (:postgres #'mito.db.postgres:column-definitions)
     (:sqlite3  #'mito.db.sqlite3:column-definitions))
   conn table-name))

(defun table-view-query (conn table-name)
  (funcall
   (ecase (dbi:connection-driver-type conn)
     (:mysql #'mito.db.mysql:table-view-query)
     (:postgres #'mito.db.postgres:table-view-query))
   conn table-name))

(defun table-exists-p (conn table-name)
  (multiple-value-bind (sql binds)
      (sxql:yield
       (ecase (dbi:connection-driver-type conn)
         (:mysql
          (sxql:select :1
            (sxql:from :information_schema.tables)
            (sxql:where (:and (:= :table_schema (dbi:connection-database-name conn))
                              (:= :table_name table-name)))
            (sxql:limit 1)))
         (:postgres
          (sxql:select :1
            (sxql:from :information_schema.tables)
            (sxql:where (:and (:= :table_schema "public")
                              (:= :table_name table-name)))
            (sxql:limit 1)))
         (:sqlite3
          (sxql:select :1
            (sxql:from :sqlite_master)
            (sxql:where (:and (:= :name table-name)
                              (:= :type "table")))
            (sxql:limit 1)))))
    (with-prepared-query query (conn sql)
      (and (dbi:fetch-all
            (execute-with-retry query binds))
           t))))

(defgeneric execute-sql (sql &optional binds)
  (:method :before (sql &optional binds)
    (declare (ignore sql binds))
    (check-connected))
  (:method ((sql string) &optional binds)
    (with-trace-sql
      (with-prepared-query query (*connection* sql :use-prepare-cached *use-prepare-cached*)
        (setf query (execute-with-retry query binds))
        (query-row-count query))))
  (:method ((sql sql-statement) &optional binds)
    (declare (ignore binds))
    (with-quote-char
      (multiple-value-bind (sql binds)
          (sxql:yield sql)
        (execute-sql sql binds)))))

(defun array-convert-nulls-to-nils (results-array)
  (let ((darray (make-array (array-total-size results-array)
                            :displaced-to results-array
                            :element-type (array-element-type results-array))))
    (loop for x across darray
          for i from 0
          do (typecase x
               ((eql :null)
                (setf (aref darray i) nil))
               (cons
                (setf (aref darray i)
                      (list-convert-nulls-to-nils x)))
               ((and (not string) vector)
                (setf (aref darray i)
                      (array-convert-nulls-to-nils x)))))
    results-array))

(defun list-convert-nulls-to-nils (results-list)
  (mapcar (lambda (x)
            (typecase x
              ((eql :null)
               nil)
              (cons
               (list-convert-nulls-to-nils x))
              ((and (not string) vector)
               (array-convert-nulls-to-nils x))
              (otherwise
               x)))
          results-list))

(defgeneric retrieve-by-sql (sql &key binds)
  (:method :before (sql &key binds)
    (declare (ignore sql binds))
    (check-connected))
  (:method ((sql string) &key binds)
    (with-prepared-query query (*connection* sql :use-prepare-cached *use-prepare-cached*)
      (let* ((results
               (dbi:fetch-all
                (with-trace-sql
                  (execute-with-retry query binds))))
             (results
               (loop for result in results
                     collect
                     (loop for (k v) on result by #'cddr
                           collect (lispify k)
                           collect (cond ((eq v :null) nil)
                                         ((and v (listp v))
                                          (list-convert-nulls-to-nils v))
                                         ((arrayp v)
                                          (array-convert-nulls-to-nils v))
                                         (t v))))))

        results)))
  (:method ((sql sql-statement) &key binds)
    (declare (ignore binds))
    (with-quote-char
      (multiple-value-bind (sql binds)
          (sxql:yield sql)
        (retrieve-by-sql sql :binds binds))))
  (:method ((sql composed-statement) &key binds)
    (declare (ignore binds))
    (with-quote-char
      (multiple-value-bind (sql binds)
          (sxql:yield sql)
        (retrieve-by-sql sql :binds binds))))
  ;; For UNION [ALL]
  (:method ((sql conjunctive-op) &key binds)
    (declare (ignore binds))
    (with-quote-char
      (multiple-value-bind (sql binds)
          (sxql:yield sql)
        (retrieve-by-sql sql :binds binds)))))
