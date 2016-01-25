(in-package :cl-user)
(defpackage mito.db.postgres
  (:use #:cl
        #:sxql
        #:split-sequence
        #:mito.util)
  (:import-from #:dbi
                #:prepare
                #:execute
                #:fetch
                #:fetch-all)
  (:export #:last-insert-id
           #:column-definitions
           #:table-indices))
(in-package :mito.db.postgres)

(defun last-insert-id (conn table-name serial-key-name)
  (let ((serial-key (sxql:make-sql-symbol serial-key-name)))
    (getf (dbi:fetch
           (dbi:execute
            (dbi:prepare conn
                         (sxql:yield
                          (select ((:as serial-key :last_insert_id))
                            (from (sxql:make-sql-symbol table-name))
                            (order-by (:desc serial-key))
                            (limit 1))))))
          :|last_insert_id|
          0)))

(defun get-serial-keys (conn table-name)
  (let ((query
          (dbi:execute
           (dbi:prepare conn
                        (format nil "SELECT relname FROM pg_class WHERE relkind = 'S' AND relname LIKE '~A_%'"
                                table-name)))))
    (loop for row = (dbi:fetch query)
          while row
          collect (second
                   (split-sequence #\_ (getf row :|relname|)
                                   :count 2)))))

(defun column-definitions (conn table-name)
  (let* ((serial-keys (get-serial-keys conn table-name))
         (sql (format nil "SELECT~
                        ~%    f.attname AS name,~
                        ~%    pg_catalog.format_type(f.atttypid,f.atttypmod) AS type,~
                        ~%    f.attnotnull AS notnull,~
                        ~%    CASE~
                        ~%        WHEN p.contype = 'p' THEN true~
                        ~%        ELSE false~
                        ~%    END AS primary~
                        ~%FROM pg_attribute f~
                        ~%    JOIN pg_class c ON c.oid = f.attrelid~
                        ~%    LEFT JOIN pg_constraint p ON p.conrelid = f.attrelid AND f.attnum = ANY (p.conkey)~
                        ~%WHERE c.relkind = 'r'::char~
                        ~%    AND c.relname = '~A'~
                        ~%    AND f.attnum > 0~
                        ~%    AND f.atttypid != 0~
                        ~%ORDER BY f.attnum, p.contype" table-name))
         (query (dbi:execute (dbi:prepare conn sql))))
    (delete-duplicates
     (loop for column = (dbi:fetch query)
           while column
           collect (list (getf column :|name|)
                         :type (getf column :|type|)
                         :auto-increment (not (null (member (getf column :|name|)
                                                            serial-keys
                                                            :test #'string=)))
                         :primary-key (getf column :|primary|)
                         :not-null (or (getf column :|primary|)
                                       (getf column :|notnull|))))
     :key #'car
     :test #'string=
     :from-end t)))

(defun table-indices (conn table-name)
  (let ((columns (mapcar #'car (column-definitions conn table-name)))
        (query (dbi:execute (dbi:prepare conn
                                         (format nil
                                                 "SELECT~
                                                ~%    i.relname AS index_name,~
                                                ~%    a.attname AS column_name,~
                                                ~%    ix.indisunique AS is_unique,~
                                                ~%    ix.indisprimary AS is_primary~
                                                ~%FROM~
                                                ~%    pg_class t,~
                                                ~%    pg_class i,~
                                                ~%    pg_index ix,~
                                                ~%    pg_attribute a~
                                                ~%WHERE~
                                                ~%    t.oid = ix.indrelid~
                                                ~%    and i.oid = ix.indexrelid~
                                                ~%    and a.attrelid = t.oid~
                                                ~%    and a.attnum = ANY(ix.indkey)~
                                                ~%    and t.relkind = 'r'~
                                                ~%    and t.relname LIKE '~A'" table-name)))))
    (mapcar #'(lambda (plist)
                (destructuring-bind (index-name &rest column-list) plist
                  (list index-name
                        :unique-key (getf (first column-list) :|is_unique|)
                        :primary-key (getf (first column-list) :|is_primary|)
                        :columns (sort (mapcar #'(lambda (column)
                                                   (getf column :|column_name|))
                                               column-list)
                                       (lambda (a b)
                                         (< (position a columns :test #'string=)
                                            (position b columns :test #'string=)))))))
            (group-by-plist (dbi:fetch-all query)
                            :key :|index_name|
                                 :test #'string=))))
