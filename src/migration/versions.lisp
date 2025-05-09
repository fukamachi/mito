(in-package :cl-user)
(defpackage mito.migration.versions
  (:use #:cl
        #:sxql)
  (:import-from #:mito.migration.table
                #:migration-expressions)
  (:import-from #:mito.migration.sql-parse
                #:parse-statements)
  (:import-from #:mito.migration.util
                #:generate-advisory-lock-id)
  (:import-from #:mito.dao
                #:dao-class
                #:dao-table-class
                #:dao-table-view
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
                #:table-exists-p
                #:acquire-advisory-lock
                #:release-advisory-lock
                #:column-definitions)
  (:import-from #:mito.type
                #:get-column-real-type)
  (:import-from #:cl-dbi
                #:connection-driver-type)
  (:import-from #:alexandria
                #:with-gensyms
                #:once-only
                #:set-equal)
  (:export #:*migration-version-format*
           #:all-migration-expressions
           #:current-migration-version
           #:update-migration-version
           #:generate-migrations
           #:migrate
           #:migration-status))
(in-package :mito.migration.versions)

(defvar *migration-version-format* :time)

(defun schema-migrations-table-definition ()
  (let ((driver-type (connection-driver-type *connection*)))
    (sxql:create-table (:schema_migrations :if-not-exists t)
        ((version :type :bigint
                  :primary-key t)
         (applied_at :type (if (eq driver-type :postgres)
                               :timestamptz
                               :timestamp)
                     :not-null t
                     :default (sxql.sql-type:make-sql-keyword "CURRENT_TIMESTAMP"))
         (dirty :type :boolean
                :not-null t
                :default (if (eq driver-type :postgres)
                             (sxql.sql-type:make-sql-keyword "false")
                             0))))))

(defun initialize-migrations-table ()
  (check-connected)
  (let ((*error-output* (make-broadcast-stream))
        (sxql:*use-placeholder* nil)
        (driver-type (connection-driver-type *connection*)))
    (dbi:with-transaction *connection*
      (if (table-exists-p *connection* "schema_migrations")
          (let ((db-columns (column-definitions *connection* "schema_migrations")))
            (unless
                (and (set-equal (mapcar 'first db-columns)
                                '("version" "applied_at" "dirty")
                                :test 'equal)
                     (equal (getf (cdr (find "version" db-columns :test 'equal :key 'first)) :type)
                            (get-column-real-type *connection* :bigint)))
              (execute-sql
               (sxql:alter-table :schema_migrations
                 (sxql:rename-to :schema_migrations_backup)))
              (execute-sql (schema-migrations-table-definition))
              (cond
                ((or (not (find "applied_at" db-columns :test 'equal :key 'first))
                     (eql 0 (caar (retrieve-by-sql "SELECT COUNT(*) FROM schema_migrations_backup WHERE applied_at IS NOT NULL" :format :values))))
                 (execute-sql
                  (format nil
                          "INSERT INTO schema_migrations (version) ~
                           SELECT CAST(version AS ~A) ~
                           FROM schema_migrations_backup ~
                           ORDER BY version DESC LIMIT 1"
                          (case driver-type
                            (:mysql "UNSIGNED")
                            (otherwise "BIGINT")))))
                (t
                 (execute-sql
                  (format nil
                          "INSERT INTO schema_migrations (version, applied_at, dirty) ~
                           SELECT CAST(version AS ~A), applied_at, CAST(~:[0~;dirty~] AS ~A) ~
                           FROM schema_migrations_backup ~
                           WHERE applied_at IS NOT NULL"
                          (case driver-type
                            (:mysql "UNSIGNED")
                            (otherwise "BIGINT"))
                          (find "dirty" db-columns :test 'equal :key 'first)
                          (case driver-type
                            (:mysql "UNSIGNED")
                            (otherwise "BOOLEAN"))))))
              (execute-sql
               (sxql:drop-table :schema_migrations_backup))))
          (execute-sql (schema-migrations-table-definition))))))

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
      (remove-if-not (lambda (class)
                       (or (typep class 'dao-table-class)
                           (typep class 'dao-table-view)))
                     (mapcan (lambda (class)
                               (append (depending-classes class)
                                       (if (new-class-p class)
                                           (list class)
                                           '())))
                             (class-subclasses (find-class 'dao-class)))))))

(defun all-migration-expressions ()
  (check-connected)
  (loop for class in (all-dao-classes)
        for (up down) = (if (table-exists-p *connection* (table-name class))
                            (multiple-value-list (migration-expressions class))
                            (list (table-definition class)
                                  (list (sxql:drop-table (sxql:make-sql-symbol (table-name class))))))
        append up into up-expressions
        append down into down-expressions
        finally (return
                  (values up-expressions
                          down-expressions))))

(defun current-migration-version ()
  (initialize-migrations-table)
  (let ((row (first (retrieve-by-sql
                     (sxql:select (:version)
                       (sxql:from :schema_migrations)
                       (sxql:order-by (:desc :version))
                       (sxql:limit 1))))))
    (getf row :version)))

(defun update-migration-version (version)
  (execute-sql
   (sxql:insert-into :schema_migrations
     (sxql:set= :version version))))

(defun generate-time-version ()
  (multiple-value-bind (sec min hour day mon year)
      (decode-universal-time (get-universal-time) 0)
    (parse-integer
     (format nil "~4,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D"
             year mon day hour min sec))))

(defun generate-version (&optional current-version)
  (ecase *migration-version-format*
    (:time (generate-time-version))
    (:serial
     (if current-version
         (1+ current-version)
         1))))

(defun generate-migrations (directory &key force)
  (let ((schema.sql (merge-pathnames #P"schema.sql" directory))
        (directory (merge-pathnames #P"migrations/" directory))
        (current-version (current-migration-version)))

    ;; Warn if there're non-applied migration files.
    (let* ((sql-files (sort (uiop:directory-files directory "*.up.sql")
                            #'string<
                            :key #'pathname-name))
           (non-applied-files
             (if current-version
                 (remove-if-not (lambda (version)
                                  (and version
                                       (< current-version version)))
                                sql-files
                                :key #'migration-file-version)
                 sql-files)))
      (when non-applied-files
        (if (y-or-n-p "Found non-applied ~D migration file~:*~P. Will you delete them?"
                      (length non-applied-files))
            (flet ((delete-migration-file (file)
                     (format *error-output* "~&Deleting '~A'...~%" file)
                     (delete-file file)))
              (dolist (up-file non-applied-files)
                (delete-migration-file up-file)
                (let ((down-file
                        (make-pathname :name (ppcre:regex-replace "\\.up$" (pathname-name up-file) ".down")
                                       :defaults up-file)))
                  (when (uiop:file-exists-p down-file)
                    (delete-migration-file down-file)))))
            (progn
              (format *error-output* "~&Given up.~%")
              (return-from generate-migrations nil)))))

    (flet ((write-expressions (expressions destination &key print)
             (ensure-directories-exist directory)
             (with-open-file (out destination
                                  :direction :output
                                  :if-does-not-exist :create)
               (let ((out (if print
                              (make-broadcast-stream *standard-output* out)
                              out)))
                 (with-quote-char
                     (map nil
                          (lambda (ex)
                            (format out "~&~A;~%" (sxql:yield ex)))
                          expressions))))
             (let ((sxql:*use-placeholder* nil))
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
                           (sxql:yield (schema-migrations-table-definition))))))
             destination))
      (multiple-value-bind
            (up-expressions down-expressions)
          (all-migration-expressions)
        (cond
          ((or up-expressions force)
           (let* ((version (generate-version current-version))
                  (up-destination (make-pathname :name (format nil "~A.up" version)
                                                 :type "sql"
                                                 :defaults directory))
                  (down-destination (make-pathname :name (format nil "~A.down" version)
                                                   :type "sql"
                                                   :defaults directory))
                  (sxql:*use-placeholder* nil))
             (write-expressions up-expressions up-destination :print t)
             (when down-expressions
               (write-expressions down-expressions down-destination))
             (format t "~&Successfully generated: ~A~%" up-destination)
             (values up-destination
                     (when down-expressions
                       down-destination))))
          (t
           (format t "~&Nothing to migrate.~%")
           (values)))))))

(defun migration-file-version (file)
  (let* ((name (pathname-name file))
         (pos (position-if (complement #'digit-char-p) name))
         (version
           (if pos
               (subseq name 0 pos)
               name)))
    (when (<= 1 (length version))
      (handler-case
          (parse-integer version)
        (error ()
          (warn "Invalid version format in a migration file: ~A~%Version must be an integer. Ignored." file))))))

(defun migration-files (base-directory &key (sort-by #'<))
  (sort (uiop:directory-files (merge-pathnames #P"migrations/" base-directory)
                              "*.up.sql")
        sort-by
        :key #'migration-file-version))

(defun %migration-status (directory)
  (let ((db-versions
          (or (handler-case (retrieve-by-sql
                             (sxql:select (:version)
                               (sxql:from :schema_migrations)
                               (sxql:where (:not-null :applied_at))
                               (sxql:order-by :version)))
                (dbi:<dbi-programming-error> () nil))
              ;; XXX: for backward-compatibility (apply all non-applied files since e18d942ba0e556b1533d5a5ac5a9775e7c6abe93)
              (retrieve-by-sql
               (sxql:select (:version)
                            (sxql:from :schema_migrations)
                            (sxql:order-by (:desc :version))
                            (sxql:limit 1)))))
        (files (migration-files directory)))
    (loop while (and files
                     db-versions
                     (< (migration-file-version (first files))
                        (getf (first db-versions) :version)))
          do (pop files))
    (let (results)
      (loop for db-version in db-versions
            do (destructuring-bind (&key version) db-version
                 (loop while (and files (< (migration-file-version (first files)) version))
                       for file = (pop files)
                       do (push (list :down :version (migration-file-version file) :file file)
                                results))
                 (if (and files (= version (migration-file-version (first files))))
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

(defparameter *advisory-lock-drivers* '(:postgres))

(defmacro with-advisory-lock ((connection) &body body)
  (with-gensyms (lock-id driver)
    (once-only (connection)
      `(let ((,driver (connection-driver-type ,connection)))
         (if (member ,driver *advisory-lock-drivers* :test 'eq)
             (let ((,lock-id (generate-advisory-lock-id (dbi:connection-database-name ,connection))))
               (acquire-advisory-lock ,connection ,lock-id)
               (unwind-protect (progn ,@body)
                 (release-advisory-lock ,connection ,lock-id)))
             (progn ,@body))))))

(defun migrate (directory &key dry-run force)
  (check-type directory pathname)
  (with-advisory-lock (*connection*)
    (let* ((current-version (current-migration-version))
           (schema.sql (merge-pathnames #P"schema.sql" directory))
           (sql-files-to-apply
             (if current-version
                 (mapcar (lambda (result)
                           (getf (cdr result) :file))
                         (remove :up
                                 (%migration-status directory)
                                 :key #'car))
                 (and (probe-file schema.sql)
                      (list schema.sql)))))
      (cond
        (sql-files-to-apply
         (dbi:with-transaction *connection*
           (dolist (file sql-files-to-apply)
             (unless force
               (format t "~&Applying '~A'...~%" file)
               (let ((content (uiop:read-file-string file)))
                 (dolist (stmt (parse-statements content))
                   (format t "~&-> ~A~%" stmt)
                   (let ((mito.logger::*mito-logger-stream* nil))
                     (execute-sql stmt)))))
             (when current-version
               (let ((version (migration-file-version file)))
                 (update-migration-version version))))
           (let* ((migration-files (migration-files directory))
                  (latest-migration-file (first (last (if current-version
                                                          sql-files-to-apply
                                                          migration-files))))
                  (version (if latest-migration-file
                               (migration-file-version latest-migration-file)
                               (generate-version))))
             (unless current-version
               (if migration-files
                   ;; Record all versions on the first table creation
                   (dolist (file migration-files)
                     (update-migration-version (migration-file-version file)))
                   (update-migration-version version)))
             (if dry-run
                 (format t "~&No problems were found while migration.~%")
                 (format t "~&Successfully updated to the version ~S.~%" version)))
           (when dry-run
             (dbi:rollback *connection*))))
        (current-version
         (format t "~&Version ~S is up to date.~%" current-version))
        (t
         (format t "~&Nothing to migrate.~%"))))))
