(in-package :cl-user)
(defpackage mito.migration.versions
  (:use #:cl
        #:sxql)
  (:import-from #:mito.migration.table
                #:migration-expressions)
  (:import-from #:mito.migration.sql-parse
                #:parse-statements)
  (:import-from #:mito.dao
                #:dao-class
                #:dao-table-class
                #:table-definition)
  (:import-from #:mito.connection
                #:*connection*
                #:check-connected
                #:with-quote-char)
  (:import-from #:mito.class
                #:table-name)
  (:import-from #:mito.db
                #:execute-sql
                #:retrieve-by-sql
                #:table-exists-p)
  (:export #:all-migration-expressions
           #:current-migration-version
           #:update-migration-version
           #:generate-migrations
           #:migrate
           #:migration-status))
(in-package :mito.migration.versions)

(defun schema-migrations-table-definition ()
  (sxql:create-table (:schema_migrations :if-not-exists t)
      ((version :type '(:varchar 255)
                :primary-key t)
       (applied_at :type :timestamptz
                   :default (sxql.sql-type:make-sql-keyword "CURRENT_TIMESTAMP")))))

(defun initialize-migrations-table ()
  (check-connected)
  (let ((*error-output* (make-broadcast-stream)))
    (execute-sql (schema-migrations-table-definition))))

(defun all-dao-classes ()
  (let ((hash (make-hash-table :test 'eq)))
    (labels ((new-class-p (class)
               (if (gethash class hash)
                   nil
                   (setf (gethash class hash) t)))
             (depending-classes (class)
               (let ((dep-classes (mito.dao:depending-table-classes class)))
                 (loop for c in dep-classes
                       if (new-class-p c)
                         append (depending-classes c)
                         and collect c)))
             (class-subclasses (class)
               (let ((subclasses (c2mop:class-direct-subclasses class)))
                 (loop for class in subclasses
                       append (cons class (class-subclasses class))))))
      (mapcan (lambda (class)
                (append (depending-classes class)
                        (if (new-class-p class)
                            (list class)
                            '())))
              (class-subclasses (find-class 'dao-class))))))

(defun all-migration-expressions ()
  (check-connected)
  (mapcan (lambda (class)
            (if (table-exists-p *connection* (table-name class))
                (migration-expressions class)
                (table-definition class)))
          (all-dao-classes)))

(defun current-migration-version ()
  (initialize-migrations-table)
  (getf (first (retrieve-by-sql
                (sxql:select :version
                  (sxql:from :schema_migrations)
                  (sxql:order-by (:desc :version))
                  (sxql:limit 1))))
        :version))

(defun update-migration-version (version)
  (execute-sql
   (sxql:insert-into :schema_migrations
     (sxql:set= :version version))))

(defun generate-version ()
  (multiple-value-bind (sec min hour day mon year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D"
            year mon day hour min sec)))

(defun generate-migrations (directory &key force)
  (let* ((schema.sql (merge-pathnames #P"schema.sql" directory))
         (directory (merge-pathnames #P"migrations/" directory))
         (version (generate-version))
         (destination (make-pathname :name (format nil "~A.up" version)
                                     :type "sql"
                                     :defaults directory))
         (expressions (all-migration-expressions))
         (sxql:*use-placeholder* nil))

    ;; Warn if there're non-applied migration files.
    (let* ((current-version (current-migration-version))
           (sql-files (sort (uiop:directory-files directory "*.up.sql")
                            #'string<
                            :key #'pathname-name))
           (non-applied-files
             (if current-version
                 (remove-if-not (lambda (version)
                                  (and version
                                       (string< current-version version)))
                                sql-files
                                :key #'migration-file-version)
                 sql-files)))
      (when non-applied-files
        (if (y-or-n-p "Found non-applied ~D migration file~:*~P. Will you delete them?"
                      (length non-applied-files))
            (dolist (file non-applied-files)
              (format *error-output* "~&Deleting '~A'...~%" file)
              (delete-file file))
            (progn
              (format *error-output* "~&Given up.~%")
              (return-from generate-migrations nil)))))

    (if (or expressions force)
        (progn
          (ensure-directories-exist directory)
          (with-open-file (out destination
                               :direction :output
                               :if-does-not-exist :create)
            (let ((out (make-broadcast-stream *standard-output* out)))
              (with-quote-char
                (map nil
                     (lambda (ex)
                       (format out "~&~A;~%" (sxql:yield ex)))
                     expressions))))
          (with-open-file (out schema.sql
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
            (with-quote-char
              (format out "~{~{~A;~%~}~^~%~}"
                      (mapcar (lambda (class)
                                (mapcar #'sxql:yield (table-definition class)))
                              (all-dao-classes)))
              (format out "~2&~A;~%"
                      (sxql:yield (schema-migrations-table-definition))))
            (format out "~&INSERT INTO schema_migrations (version) VALUES ('~A');~%"
                    version))
          (format t "~&Successfully generated: ~A~%" destination)
          destination)
        (progn
          (format t "~&Nothing to migrate.~%")
          nil))))

(defun migration-file-version (file)
  (let* ((name (pathname-name file))
         (pos (or (position #\_ name)
                  (position #\. name :from-end t)))
         (version
           (if pos
               (subseq name 0 pos)
               name)))
    (when (and (= (length version) 14)
               (every #'digit-char-p version))
      version)))

(defun migration-files (base-directory &key (sort-by #'string<))
  (sort (uiop:directory-files (merge-pathnames #P"migrations/" base-directory)
                              "*.up.sql")
        sort-by
        :key #'pathname-name))

(defun %migration-status (directory)
  (let ((db-versions
          (retrieve-by-sql
           (sxql:select (:version :applied_at)
             (sxql:from :schema_migrations)
             (sxql:order-by :version))))
        (files (migration-files directory)))
    (loop while (and files
                     db-versions
                     (string< (migration-file-version (first files))
                              (getf (first db-versions) :version)))
          do (pop files))
    (let (results)
      (loop for db-version in db-versions
            do (destructuring-bind (&key version applied-at) db-version
                 (loop while (string< (migration-file-version (first files)) version)
                       for file = (pop files)
                       do (push (list :down :version (migration-file-version file) :file file)
                                results))
                 (if (string= version (migration-file-version (first files)))
                     (push (list :up :version version :file (pop files))
                           results)
                     (push (list :up :version version) results))))
      (nconc (nreverse results)
             (mapcar (lambda (file)
                       (list :down :version (migration-file-version file) :file file))
                     files)))))

(defun migration-status (directory)
  (initialize-migrations-table)
  (format t "~& Status   Migration ID~%--------------------------~%")
  (dolist (result (%migration-status directory))
    (destructuring-bind (type &key version file) result
      (ecase type
        (:up   (format t "~&   up     ~A" version))
        (:down (format t "~&  down    ~A" version)))
      (etypecase file
        (null (format t "   (NO FILE)~%"))
        (pathname (format t "~%"))))))

(defun migrate (directory &key dry-run)
  (let* ((current-version (current-migration-version))
         (schema.sql (merge-pathnames #P"schema.sql" directory))
         (sql-files-to-apply
           (if current-version
               (mapcar #'cdr
                       (remove :up
                               (%migration-status directory)
                               :key #'car))
               (and (probe-file schema.sql)
                    (list schema.sql)))))
    (cond
      (sql-files-to-apply
       (dbi:with-transaction *connection*
         (dolist (file sql-files-to-apply)
           (format t "~&Applying '~A'...~%" file)
           (let ((content (uiop:read-file-string file)))
             (dolist (stmt (parse-statements content))
               (format t "~&-> ~A~%" stmt)
               (let ((mito.logger::*mito-logger-stream* nil))
                 (execute-sql stmt))))
           (when current-version
             (let ((version (migration-file-version file)))
               (update-migration-version version))))
         (let ((version (migration-file-version (first (last sql-files-to-apply)))))
           (if dry-run
               (format t "~&No problems were found while migration.~%")
               (format t "~&Successfully updated to the version ~S.~%" version)))
         (when dry-run
           (dbi:rollback *connection*))))
      (current-version
       (format t "~&Version ~S is up to date.~%" current-version))
      (t
       (format t "~&Nothing to migrate.~%")))))
