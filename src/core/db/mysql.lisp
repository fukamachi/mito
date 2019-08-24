(in-package :cl-user)
(defpackage mito.db.mysql
  (:use #:cl
        #:mito.util
        #:sxql)
  (:import-from #:mito.connection
                #:connection-quote-character)
  (:import-from #:dbi
                #:execute
                #:fetch
                #:fetch-all
                #:connection-database-name)
  (:import-from #:alexandria
                #:delete-from-plist)
  (:export #:last-insert-id
           #:table-indices
           #:column-definitions
           #:table-view-query))
(in-package :mito.db.mysql)

(defun last-insert-id (conn table-name serial-key-name)
  (declare (ignore table-name serial-key-name))
  (with-prepared-query query (conn "SELECT last_insert_id() AS last_insert_id")
    (getf (dbi:fetch
           (dbi:execute query))
          :|last_insert_id|
          0)))

(defun table-indices (conn table-name)
  (with-prepared-query query
      (conn (format nil
                    ;; Ensure the field names are downcased
                    "SELECT index_name AS index_name, column_name AS column_name, non_unique AS non_unique
                     FROM information_schema.statistics
                     WHERE table_schema = '~A'
                       AND table_name = '~A'
                     ORDER BY index_name, seq_in_index"
                    (connection-database-name conn)
                    table-name))
    (let ((results (dbi:execute query)))
      (mapcar (lambda (plist)
                (destructuring-bind (index-name &rest column-list) plist
                  (list index-name
                        :unique-key (or (string= index-name "PRIMARY")
                                        (= (getf (first column-list) :|non_unique|) 0))
                        :primary-key (string= index-name "PRIMARY")
                        :columns (mapcar (lambda (column)
                                           (getf column :|column_name|))
                                         column-list))))
              (group-by-plist (dbi:fetch-all results)
                              :key :|index_name|
                              :test #'string=)))))

(defun ensure-string (val)
  (etypecase val
    ((vector (unsigned-byte 8))
     (map 'string #'code-char val))
    (string val)))

(defun column-definitions (conn table-name)
  (let ((sql (format nil "SHOW FULL FIELDS FROM `~A`" table-name)))
    (with-prepared-query query (conn sql)
      (let* ((results (dbi:execute query))
             (definitions
               (loop for column = (dbi:fetch results)
                     while column
                     collect (list (getf column :|Field|)
                                   :type (ensure-string (getf column :|Type|))
                                   :auto-increment (string= (getf column :|Extra|) "auto_increment")
                                   :primary-key (string= (getf column :|Key|) "PRI")
                                   :not-null (or (string= (getf column :|Key|) "PRI")
                                                 (string= (getf column :|Null|) "NO"))))))
        ;; Set :primary-key NIL if there's a composite primary key.
        (if (< 1 (count-if (lambda (def)
                             (getf (cdr def) :primary-key))
                           definitions))
            (mapc (lambda (def)
                    (setf (getf (cdr def) :primary-key) nil))
                  definitions)
            definitions)))))

(defun table-view-query (conn table-name)
  (with-prepared-query query (conn (format nil "SHOW CREATE VIEW `~A`" table-name))
    (let ((results (dbi:execute query)))
      (getf (first (dbi:fetch-all results)) :|Create View|))))
