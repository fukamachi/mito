(in-package :cl-user)
(defpackage mito.migration.table
  (:use #:cl
        #:sxql)
  (:import-from #:mito.dao
                #:convert-for-driver-type
                #:dao-table-class
                #:dao-table-view
                #:table-definition
                #:dao-table-view-as-query)
  (:import-from #:mito.dao.column
                #:dao-table-column-deflate)
  (:import-from #:mito.class
                #:database-column-slots
                #:table-name
                #:table-column-info
                #:table-column-type
                #:table-indices-info
                #:create-table-sxql
                #:find-slot-by-name)
  (:import-from #:mito.db
                #:table-indices
                #:column-definitions
                #:table-view-query
                #:table-exists-p
                #:execute-sql)
  (:import-from #:mito.connection
                #:*connection*
                #:driver-type
                #:check-connected
                #:connected-p)
  (:import-from #:mito.type
                #:get-column-real-type)
  (:import-from #:mito.logger
                #:with-sql-logging)
  (:import-from #:mito.util
                #:lispify
                #:list-diff
                #:ensure-class)
  (:import-from #:alexandria
                #:ensure-list)
  (:export #:*auto-migration-mode*
           #:*migration-keep-temp-tables*
           #:migrate-table
           #:migration-expressions))
(in-package :mito.migration.table)

(defvar *auto-migration-mode* nil)

(defvar *migration-keep-temp-tables* t
  "SQLite3 migration creates temporary tables with pre-migration data.
If this variable is T they won't be deleted after migration.")

(defgeneric migrate-table (class)
  (:method ((class symbol))
    (migrate-table (find-class class)))
  (:method ((class dao-table-class))
    (check-connected)
    (dbi:with-transaction *connection*
      (with-sql-logging
        (mapc #'execute-sql
              (migration-expressions class))))))

(defun migration-expressions-for-others (class driver-type)
  (let* ((table-name (table-name class))
         (table-columns
           (mapcar (lambda (column)
                     (let ((info (table-column-info column driver-type)))
                       (setf (getf (cdr info) :type)
                             (get-column-real-type *connection* (getf (cdr info) :type)))
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
                     :sort-fn
                     (lambda (a b)
                       (string< (prin1-to-string (cdr a))
                                (prin1-to-string (cdr b)))))
        (declare (ignore indices-intersection))
        ;; TODO: take care of the order of columns
        (list
         ;; add columns
         (if columns-to-add
             (let ((drop-defaults '()))
               (cons
                (apply #'sxql:make-statement :alter-table (sxql:make-sql-symbol table-name)
                       (mapcar (lambda (column)
                                 (sxql:make-clause :add-column (sxql:make-sql-symbol (car column))
                                                   :type
                                                   (let ((type (getf (cdr column) :type)))
                                                     (if (and (eq driver-type :postgres)
                                                              (getf (cdr column) :auto-increment))
                                                         (cond
                                                           ((string= type "integer")
                                                            "serial")
                                                           ((string= type "bigint")
                                                            "bigserial")
                                                           (t
                                                            (error "Invalid PostgreSQL serial type: ~S" type)))
                                                         type))
                                                   :default
                                                   (if (getf (cdr column) :not-null)
                                                       (let ((slot
                                                               (find-slot-by-name class (lispify (car column))
                                                                                  :test #'string-equal)))
                                                         (cond
                                                           ((c2mop:slot-definition-initfunction slot)
                                                            (push (car column) drop-defaults)
                                                            (convert-for-driver-type
                                                             (driver-type)
                                                             (table-column-type slot)
                                                             (dao-table-column-deflate slot
                                                                                       (funcall (c2mop:slot-definition-initfunction slot)))))
                                                           (t
                                                            (warn "Adding a non-null column ~S but there's no :initform to set default"
                                                                  (car column))
                                                            nil)))
                                                       nil)
                                                   :primary-key (getf (cdr column) :primary-key)
                                                   :not-null (getf (cdr column) :not-null)
                                                   :auto-increment (and (eq driver-type :mysql)
                                                                        (getf (cdr column) :auto-increment))))
                               columns-to-add))
                (when drop-defaults
                  (list
                   (apply #'sxql:make-statement :alter-table (sxql:make-sql-symbol table-name)
                          (mapcar (lambda (column-name)
                                    (sxql:alter-column (sxql:make-sql-symbol column-name)
                                                       :drop-default t))
                                  (nreverse drop-defaults)))))))
             nil)
         ;; drop columns
         (if columns-to-delete
             (apply #'sxql:make-statement :alter-table (sxql:make-sql-symbol table-name)
                    (mapcar (lambda (column)
                              (sxql:drop-column (sxql:make-sql-symbol (car column))))
                            columns-to-delete))
             nil)
         ;; change columns
         (if columns-intersection
             (loop with before-alter-sequences = '()
                   with after-alter-sequences = '()
                   for db-column in columns-intersection
                   for table-column = (find (car db-column) table-columns :test #'string= :key #'car)
                   unless (equalp db-column table-column)
                     append (case driver-type
                              (:postgres
                               (loop for (k v) on (cdr table-column) by #'cddr
                                     for current-value = (getf (cdr db-column) k)
                                     unless (or (eq k :primary-key) ;; ignore :primary-key as it'll be added in the later indices' section
                                                (eql v current-value))
                                       collect
                                       (case k
                                         (:auto-increment
                                          (let ((seq (format nil "~A_~A_seq"
                                                             table-name
                                                             (car table-column))))
                                            (if v
                                                (progn
                                                  ;; create a new sequence
                                                  (push
                                                   (sxql:make-statement :create-sequence (sxql:make-sql-symbol seq))
                                                   before-alter-sequences)
                                                  (sxql:make-clause :set-default `(:nextval ,seq)))
                                                (progn
                                                  ;; delete the existing sequence
                                                  (push
                                                   (sxql:make-statement :drop-sequence
                                                                        (sxql:make-sql-symbol seq))
                                                   after-alter-sequences)
                                                  (sxql:make-clause :alter-column
                                                                    (sxql:make-sql-symbol (car table-column))
                                                                    :drop-default t)))))
                                         (otherwise
                                          (sxql:make-clause :alter-column
                                                            (sxql:make-sql-symbol (car table-column))
                                                            k v)))))
                              (otherwise
                               ;; Don't add PRIMARY KEY if the column is already the primary key
                               (when (getf (cdr db-column) :primary-key)
                                 (setf (getf (cdr table-column) :primary-key) nil))
                               (list
                                (apply #'sxql:make-clause :modify-column (sxql:make-sql-symbol (car table-column))
                                       (cdr table-column)))))
                       into clauses
                   finally
                      (return
                        (nconc
                         (nreverse before-alter-sequences)
                         (and clauses
                              (list (apply #'sxql:make-statement :alter-table (sxql:make-sql-symbol table-name)
                                           clauses)))
                         (nreverse after-alter-sequences))))
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
                          (sxql:make-statement :alter-table (sxql:make-sql-symbol table-name)
                                               (apply #'sxql:make-clause :add-primary-key
                                                      (mapcar #'sxql:make-sql-symbol (getf options :columns)))))
                         nil)
                   else
                     collect (sxql:create-index
                              (sxql:make-sql-symbol index-name)
                              :unique (getf options :unique-key)
                              :on (list* (sxql:make-sql-symbol table-name)
                                         (mapcar #'sxql:make-sql-symbol (getf options :columns)))))
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
                     append
                     (nconc
                      (when (and (not (eq driver-type :postgres))
                                 (getf options :primary-key))
                        (list (sxql:make-statement :alter-table (sxql:make-sql-symbol table-name)
                                                   (sxql:drop-primary-key))))
                      (list
                       (apply #'sxql:drop-index index-name
                              (if (eq driver-type :postgres)
                                  nil
                                  (list :on (sxql:make-sql-symbol table-name)))))))
             nil))))))

(defun migration-expressions-for-sqlite3 (class)
  (let* ((table-name (table-name class))
         (tmp-table-name (gensym table-name))
         (table-columns
           (mapcar (lambda (column)
                     (let ((info (table-column-info column :sqlite3)))
                       (setf (getf (cdr info) :type)
                             (get-column-real-type *connection* (getf (cdr info) :type)))
                       info))
                   (database-column-slots class)))
         (table-indices (table-indices-info class :sqlite3))
         (db-columns (column-definitions *connection* table-name))
         (db-indices (table-indices *connection* table-name)))

    (dolist (idx db-indices)
      (setf (getf (cdr idx) :columns)
            (sort (getf (cdr idx) :columns) #'string<=)))
    (dolist (idx table-indices)
      (setf (getf (cdr idx) :columns)
            (sort (getf (cdr idx) :columns) #'string<=)))

    (unless (every #'null
                   (append (cdr (multiple-value-list (list-diff db-columns table-columns
                                                                :test #'equalp
                                                                :sort-fn (constantly t))))
                           (cdr (multiple-value-list
                                 (list-diff db-indices table-indices :key #'cdr
                                                                     :test #'equalp
                                                                     :sort-fn (constantly t))))))
      (list*
       (sxql:alter-table (sxql:make-sql-symbol table-name)
         (sxql:rename-to tmp-table-name))

       (first (create-table-sxql class :sqlite3))

       (let* ((column-names (mapcar #'car
                                    (column-definitions *connection* table-name)))
              (slot-names (mapcar #'car table-columns))
              (same (list-diff column-names slot-names))
              (same-names (mapcar #'sxql:make-sql-symbol same))
              (new (set-difference slot-names column-names :test #'string-equal)))
         (multiple-value-bind (new-names defaults)
             (slot-defaults class table-columns new)
           (sxql:insert-into (sxql:make-sql-symbol table-name) (append same-names new-names)
             (sxql:select
                 (apply #'sxql:make-clause :fields (append same-names defaults))
               (sxql:from tmp-table-name)))))
       (unless *migration-keep-temp-tables*
         (list (sxql:drop-table tmp-table-name)))))))

(defun slot-defaults (class table-columns new-fields)
  (let (new-names defaults)
    (dolist (new-field new-fields)
      (let ((slot
              (find-slot-by-name class (lispify (string-upcase new-field))
                                 :test #'string-equal)))
        (cond
          ((c2mop:slot-definition-initfunction slot)
           (push (sxql:make-sql-symbol new-field) new-names)
           (push
            (convert-for-driver-type
             :sqlite3
             (table-column-type slot)
             (dao-table-column-deflate slot
                                       (funcall (c2mop:slot-definition-initfunction slot))))
            defaults))
          (t
           (when (getf (cdr (find new-field table-columns :key #'first)) :not-null)
             (warn "Adding a non-null column ~S but there's no :initform to set default"
                   new-field))))))
    (values new-names defaults)))

(defun migration-expressions (class &optional (driver-type (driver-type *connection*)))
  (setf class (ensure-class class))
  (etypecase class
    (dao-table-view
     (execute-sql
      (sxql:make-statement :create-view
                           (sxql:make-sql-symbol (format nil "__~A" (table-name class)))
                           :or-replace t
                           :as (first (dao-table-view-as-query class))))
     (unwind-protect
          (if (equal (table-view-query *connection* (format nil "__~A" (table-name class)))
                     (table-view-query *connection* (table-name class)))
              nil
              (table-definition class :or-replace t))
       (execute-sql
        (format nil "DROP VIEW ~@[~A~]__~A~@[~A~]"
                sxql:*quote-character*
                (table-name class)
                sxql:*quote-character*))))
    (dao-table-class
     (if (eq driver-type :sqlite3)
         (migration-expressions-for-sqlite3 class)
         (destructuring-bind (add-columns
                              drop-columns
                              change-columns
                              add-indices
                              drop-indices)
             (migration-expressions-for-others class driver-type)
           (nconc drop-indices
                  (ensure-list drop-columns)
                  add-columns
                  change-columns
                  add-indices))))))

(defmethod initialize-instance :after ((class dao-table-class) &rest initargs)
  (declare (ignore initargs))
  (when (and *auto-migration-mode*
             (connected-p))
    (unless (table-exists-p *connection* (table-name class))
      (with-sql-logging
        (mapc #'execute-sql (table-definition class))))
    (migrate-table class)))

(defmethod reinitialize-instance :after ((class dao-table-class) &rest initargs)
  (declare (ignore initargs))
  (when (and *auto-migration-mode*
             (connected-p))
    (unless (table-exists-p *connection* (table-name class))
      (with-sql-logging
        (mapc #'execute-sql (table-definition class))))
    (migrate-table class)))
