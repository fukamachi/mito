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
  (:import-from #:sxql/composer
                #:select-query-state)
  (:export #:*use-prepare-cached*
           #:last-insert-id
           #:table-indices
           #:column-definitions
           #:table-view-query
           #:table-exists-p
           #:execute-sql
           #:retrieve-by-sql
           #:acquire-advisory-lock
           #:release-advisory-lock))
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
  (check-type table-name string)
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
            (execute-with-retry query binds)
            :format :plist)
           t))))

(defun sxql-to-sql (sql)
  (with-quote-char (sxql:yield sql)))

(defun ensure-sql (sql)
  (etypecase sql
    (string sql)
    ((or sql-statement
         composed-statement
         ;; For UNION [ALL]
         conjunctive-op
         select-query-state)
     (sxql-to-sql sql))))

(defgeneric execute-sql (sql &optional binds)
  (:method ((sql string) &optional binds)
    (check-connected)
    (with-trace-sql
      (with-prepared-query query (*connection* sql :use-prepare-cached *use-prepare-cached*)
        (setf query (execute-with-retry query binds))
        (query-row-count query))))
  (:method ((sql sql-statement) &optional binds)
    (declare (ignore binds))
    (multiple-value-bind (sql binds)
        (sxql-to-sql sql)
      (execute-sql sql binds))))

(defun lispified-fields (query)
  (mapcar (lambda (field)
            (declare (type string field))
            (intern (map 'string
                         (lambda (char)
                           (declare (type character char))
                           (if (char= char #\_)
                               #\-
                               (char-upcase char)))
                         field)
                    :keyword))
          (dbi:query-fields query)))

(defun convert-nulls-to-nils (value)
  (typecase value
    ((eql :null)
     nil)
    (cons
     (mapcar #'convert-nulls-to-nils value))
    ((and (not string) vector)
     (map (type-of value) #'convert-nulls-to-nils value))
    (otherwise
     value)))

(defvar *plist-row-lispify* nil)

(defun retrieve-from-query (query format)
  (ecase format
    (:plist
     (let ((rows (dbi:fetch-all query :format :values))
           (fields (if *plist-row-lispify*
                       (lispified-fields query)
                       (mapcar (lambda (field)
                                 (intern field :keyword))
                               (dbi:query-fields query)))))
       (loop for row in rows
             collect
             (loop for field in fields
                   for v in row
                   collect field
                   collect (convert-nulls-to-nils v)))))
    (:alist
     (let ((rows (dbi:fetch-all query :format :values)))
       (mapcar (lambda (row)
                 (loop for v in row
                       for field in (dbi:query-fields query)
                       collect (cons field
                                     (convert-nulls-to-nils v))))
               rows)))
    (:hash-table
     (let ((rows (dbi:fetch-all query :format :hash-table)))
       (maphash (lambda (k v)
                  (setf (gethash k rows)
                        (convert-nulls-to-nils v)))
                rows)
       rows))
    (:values
     (convert-nulls-to-nils
      (dbi:fetch-all query :format :values)))))

(defgeneric retrieve-by-sql (sql &key binds format lispify)
  (:method ((sql string) &key binds format (lispify nil lispify-specified))
    (check-connected)
    (with-prepared-query query (*connection* sql :use-prepare-cached *use-prepare-cached*)
      (let* ((query (with-trace-sql
                      (execute-with-retry query binds)))
             (format (or format :plist))
             (*plist-row-lispify*
               (if lispify-specified
                   lispify
                   (case format
                     (:plist t)
                     (otherwise nil)))))
        (retrieve-from-query query format))))
  (:method (sql &rest args &key binds &allow-other-keys)
    (assert (null binds))
    (multiple-value-bind (sql binds)
        (ensure-sql sql)
      (apply #'retrieve-by-sql sql :binds binds args))))

(defun acquire-advisory-lock (conn id)
  (funcall
   (case (dbi:connection-driver-type conn)
     (:postgres #'mito.db.postgres:acquire-advisory-lock)
     (:mysql #'mito.db.mysql:acquire-advisory-lock)
     (otherwise
      ;; Just ignore
      (lambda (&rest args) (declare (ignore args)))))
   conn id))

(defun release-advisory-lock (conn id)
  (funcall
   (case (dbi:connection-driver-type conn)
     (:postgres #'mito.db.postgres:release-advisory-lock)
     (:mysql #'mito.db.mysql:release-advisory-lock)
     (otherwise
      ;; Just ignore
      (lambda (&rest args) (declare (ignore args)))))
   conn id))
