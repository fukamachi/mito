(in-package :cl-user)
(defpackage mito.db.postgres
  (:use #:cl
        #:sxql
        #:mito.util)
  (:import-from #:dbi
                #:execute
                #:fetch
                #:fetch-all)
  (:export #:last-insert-id
           #:column-definitions
           #:table-indices
           #:table-view-query))
(in-package :mito.db.postgres)

(defun last-insert-id (conn table-name serial-key-name)
  (handler-case
      (with-prepared-query query
          (conn (format nil
                        "SELECT currval(pg_get_serial_sequence('~A', '~A')) AS last_insert_id"
                        table-name
                        serial-key-name))
        (getf (dbi:fetch
               (dbi:execute query))
              :|last_insert_id|
              0))
    (dbi:<dbi-error> () 0)))

(defun get-serial-keys (conn table-name)
  (remove-if-not
   (lambda (column)
     (with-prepared-query query
         (conn (format nil "SELECT pg_get_serial_sequence('~A', '~A')" table-name column))
       (let ((seq (getf
                   (first
                    (dbi:fetch-all
                     (dbi:execute query)))
                   :|pg_get_serial_sequence|)))
         (if (eq seq :null)
             nil
             seq))))
   (with-prepared-query query
       (conn (format nil "SELECT column_name FROM information_schema.columns WHERE table_name = '~A'"
                     table-name))
     (mapcar (lambda (row)
               (getf row :|column_name|))
             (dbi:fetch-all
              (dbi:execute query))))))

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
                        ~%ORDER BY f.attnum, p.contype" table-name)))
    (with-prepared-query query (conn sql)
      (let ((definitions
              (delete-duplicates
               (loop with results = (dbi:execute query)
                     for column = (dbi:fetch results)
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
        ;; Set :primary-key NIL if there's a composite primary key.
        (if (< 1 (count-if (lambda (def)
                             (getf (cdr def) :primary-key))
                           definitions))
            (mapc (lambda (def)
                    (setf (getf (cdr def) :primary-key) nil))
                  definitions)
            definitions)))))

(defun table-indices (conn table-name)
  (with-prepared-query query (conn (format nil
                                           "SELECT~
                                          ~%    i.relname AS index_name,~
                                          ~%    ARRAY(~
                                          ~%        SELECT pg_get_indexdef(ix.indexrelid, k + 1, TRUE)~
                                          ~%        FROM~
                                          ~%          generate_subscripts(ix.indkey, 1) AS k~
                                          ~%        ORDER BY k~
                                          ~%    ) AS column_names,~
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
                                          ~%    and t.relname = '~A'~
                                          ~%GROUP BY~
                                          ~%    t.relname, i.relname, ix.indexrelid, ix.indkey, ix.indisunique, ix.indisprimary~
                                          ~%ORDER BY t.relname, i.relname" table-name))
    (let ((results (dbi:execute query)))
      (mapcar #'(lambda (plist)
                  (destructuring-bind (&key |index_name| |column_names| |is_unique| |is_primary|) plist
                    (list |index_name|
                          :unique-key |is_unique|
                          :primary-key |is_primary|
                          :columns (map 'list (lambda (column)
                                                (if (and (char= (aref column 0) #\")
                                                         (char= (aref column (1- (length column))) #\"))
                                                    (read-from-string column)
                                                    column))
                                        |column_names|))))
              (dbi:fetch-all results)))))

(defun table-view-query (conn table-name)
  (with-prepared-query query (conn (format nil "SELECT pg_get_viewdef('~A'::regclass) AS def" table-name))
    (let ((results (dbi:execute query)))
      (string-right-trim
       '(#\Space #\;)
       (string-left-trim
        '(#\Space)
        (getf (first (dbi:fetch-all results)) :|def|))))))
