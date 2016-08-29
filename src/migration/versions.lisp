(in-package :cl-user)
(defpackage mito.migration.versions
  (:use #:cl
        #:sxql)
  (:import-from #:mito.migration.table
                #:migration-expressions)
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
           #:migrate))
(in-package :mito.migration.versions)

(defun schema-migrations-table-definition ()
  (sxql:create-table (:schema_migrations :if-not-exists t)
      ((version :type '(:varchar 255)
                :primary-key t))))

(defun initialize-migrations-table ()
  (check-connected)
  (execute-sql (schema-migrations-table-definition)))

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

(defun generate-migrations (directory)
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

    (if expressions
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

(defun read-one-sql (stream)
  (let ((sql
          (string-trim '(#\Space #\Tab #\Newline #\LineFeed)
                       (with-output-to-string (s)
                         (loop for char = (read-char stream nil nil)
                               while char
                               until (char= char #\;)
                               do (write-char char s))))))
    (if (= (length sql) 0)
        nil
        sql)))

(defun migrate (directory &key dry-run)
  (let* ((current-version (current-migration-version))
         (sql-files (sort (uiop:directory-files (merge-pathnames #P"migrations/" directory)
                                                "*.up.sql")
                          #'string<
                          :key #'pathname-name))
         (sql-files-to-apply
           (if current-version
               (remove-if-not (lambda (version)
                                (and version
                                     (string< current-version version)))
                              sql-files
                              :key #'migration-file-version)
               (list
                (merge-pathnames #P"schema.sql" directory)))))
    (if sql-files-to-apply
        (dbi:with-transaction *connection*
          (dolist (file sql-files-to-apply)
            (format t "~&Applying '~A'...~%" file)
            (with-open-file (in file)
              (loop for sql = (read-one-sql in)
                    while sql
                    do (format t "~&-> ~A;~%" sql)
                       (unless dry-run
                         (let ((mito.logger::*mito-logger-stream* nil))
                           (execute-sql sql))))))
          (let ((version (migration-file-version (first (last sql-files)))))
            (when current-version
              (update-migration-version version))
            (format t "~&Successfully updated to the version ~S.~%" version)))
        (format t "~&Version ~S is up to date.~%" current-version))))
