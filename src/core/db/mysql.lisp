(in-package :cl-user)
(defpackage mito.db.mysql
  (:use #:cl
        #:mito.util
        #:sxql)
  (:import-from #:mito.connection
                #:connection-quote-character)
  (:import-from #:dbi
                #:prepare
                #:execute
                #:fetch
                #:fetch-all
                #:connection-database-name)
  (:import-from #:alexandria
                #:delete-from-plist)
  (:export #:last-insert-id
           #:table-indices
           #:column-definitions))
(in-package :mito.db.mysql)

(defun last-insert-id (conn table-name serial-key-name)
  (let ((serial-key (sxql:make-sql-symbol serial-key-name)))
    (let ((sxql:*quote-character* (or sxql:*quote-character*
                                      (connection-quote-character conn))))
      (getf (dbi:fetch
             (dbi:execute
              (dbi:prepare conn
                           (sxql:yield
                            (select ((:as serial-key :last_insert_id))
                              (from (sxql:make-sql-symbol table-name))
                              (order-by (:desc serial-key))
                              (limit 1))))))
            :|last_insert_id|
            0))))

(defun table-indices (conn table-name)
  (let ((query
          (dbi:execute
           (dbi:prepare conn
                        (format nil "SELECT index_name, column_name, non_unique
                                 FROM information_schema.statistics
                                 WHERE table_schema = '~A'
                                   AND table_name = '~A'
                                 ORDER BY index_name, seq_in_index"
                                (connection-database-name conn)
                                table-name)))))
    (mapcar (lambda (plist)
              (destructuring-bind (index-name &rest column-list) plist
                (list index-name
                      :unique-key (or (string= index-name "PRIMARY")
                                      (= (getf (first column-list) :|non_unique|) 0))
                      :primary-key (string= index-name "PRIMARY")
                      :columns (mapcar (lambda (column)
                                         (getf column :|column_name|))
                                       column-list))))
            (group-by-plist (dbi:fetch-all query)
                            :key :|index_name|
                            :test #'string=))))

(defun column-definitions (conn table-name)
  (let* ((sql (format nil "SHOW FULL FIELDS FROM `~A`" table-name))
         (query (dbi:execute (dbi:prepare conn sql)))
         (definitions
           (loop for column = (dbi:fetch query)
                 while column
                 collect (list (getf column :|Field|)
                               :type (getf column :|Type|)
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
        definitions)))
