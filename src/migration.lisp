(in-package :cl-user)
(defpackage mito.migration
  (:use #:cl)
  (:import-from #:mito.dao
                #:dao-table-class
                #:table-definition)
  (:import-from #:mito.class
                #:database-column-slots
                #:table-name
                #:table-column-info
                #:table-indices-info
                #:create-table-sxql)
  (:import-from #:mito.db
                #:table-indices
                #:column-definitions
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
                #:list-diff)
  (:import-from #:sxql
                #:make-statement
                #:make-clause
                #:drop-column
                #:drop-primary-key
                #:create-index
                #:drop-index)
  (:import-from #:alexandria
                #:ensure-list)
  (:export #:*auto-migration-mode*
           #:migrate-table
           #:migration-expressions))
(in-package :mito.migration)

(defvar *auto-migration-mode* nil)

(defgeneric migrate-table (class)
  (:method ((class symbol))
    (migrate-table (find-class class)))
  (:method ((class dao-table-class))
    (check-connected)
    (dbi:with-transaction *connection*
      (with-sql-logging
        (mapc #'execute-sql
              (migration-expressions class))))))

(defstruct (set-default (:include sxql.sql-type:expression-clause (sxql.sql-type::name "SET DEFAULT"))
                        (:constructor make-set-default (expression &aux (expression (sxql.clause::detect-and-convert expression))))))
(defstruct (drop-default (:include sxql.sql-type:sql-clause)))
(defmethod sxql:yield ((clause drop-default))
  "DROP DEFAULT")

(defstruct (create-sequence (:include sxql.sql-type:sql-statement (sxql.sql-type::name "CREATE SEQUENCE"))
                            (:constructor make-create-sequence (sequence-name &aux (sequence-name (sxql.clause::detect-and-convert sequence-name)))))
  sequence-name)
(defmethod sxql:yield ((statement create-sequence))
  (sxql.sql-type:with-yield-binds
    (format nil "CREATE SEQUENCE ~A" (sxql:yield (create-sequence-sequence-name statement)))))

(defstruct (drop-sequence (:include sxql.sql-type:sql-statement (sxql.sql-type::name "DROP SEQUENCE"))
                          (:constructor make-drop-sequence (sequence-name
                                                            &key if-exists
                                                            &aux (sequence-name (sxql.clause::detect-and-convert sequence-name)))))
  sequence-name
  if-exists)
(defmethod sxql:yield ((statement drop-sequence))
  (sxql.sql-type:with-yield-binds
    (format nil "DROP SEQUENCE~:[~; IF EXISTS~] ~A"
            (drop-sequence-if-exists statement)
            (sxql:yield (drop-sequence-sequence-name statement)))))

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

      (dolist (idx db-indices)
        (setf (getf (cdr idx) :columns)
              (sort (getf (cdr idx) :columns) #'string<=)))
      (dolist (idx table-indices)
        (setf (getf (cdr idx) :columns)
              (sort (getf (cdr idx) :columns) #'string<=)))
      (multiple-value-bind (indices-intersection
                            indices-to-delete
                            indices-to-add)
          (list-diff db-indices table-indices
                     :key #'cdr
                     :test #'equalp
                     :sort-fn (constantly t))
        (declare (ignore indices-intersection))
        ;; TODO: take care of the order of columns
        (list
         ;; add columns
         (if columns-to-add
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
                                                :primary-key (getf (cdr column) :primary-key)
                                                :not-null (getf (cdr column) :not-null)
                                                :auto-increment (and (eq driver-type :mysql)
                                                                     (getf (cdr column) :auto-increment))))
                            columns-to-add))
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
             (loop with alter-sequences = '()
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
                                                   (make-create-sequence seq)
                                                   alter-sequences)
                                                  (make-set-default `(:nextval ,seq)))
                                                (progn
                                                  ;; delete the existing sequence
                                                  (push
                                                   (make-drop-sequence seq)
                                                   alter-sequences)
                                                  (make-drop-default)))))
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
                         (nreverse alter-sequences)
                         (and clauses
                              (list (apply #'sxql:make-statement :alter-table (sxql:make-sql-symbol table-name)
                                           clauses))))))
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
                              index-name
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
      (list
       (sxql:alter-table (sxql:make-sql-symbol table-name)
         (sxql:rename-to tmp-table-name))

       (create-table-sxql class :sqlite3)

       (let* ((column-names (mapcar #'car
                                    (column-definitions *connection* table-name)))
              (slot-names (mapcar #'car table-columns))
              (same (list-diff column-names slot-names))
              (same (mapcar #'sxql:make-sql-symbol same)))
         (sxql:insert-into (sxql:make-sql-symbol table-name) same
           (sxql:select
               (apply #'sxql:make-clause :fields same)
             (sxql:from tmp-table-name))))))))

(defun migration-expressions (class &optional (driver-type (driver-type *connection*)))
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
               (ensure-list add-columns)
               change-columns
               add-indices))))

(defmethod initialize-instance :after ((class dao-table-class) &rest initargs)
  (declare (ignore initargs))
  (when (and *auto-migration-mode*
             (connected-p))
    (unless (table-exists-p *connection* (table-name class))
      (with-sql-logging
        (execute-sql (table-definition class))))
    (migrate-table class)))

(defmethod reinitialize-instance :after ((class dao-table-class) &rest initargs)
  (declare (ignore initargs))
  (when (and *auto-migration-mode*
             (connected-p))
    (unless (table-exists-p *connection* (table-name class))
      (with-sql-logging
        (execute-sql (table-definition class))))
    (migrate-table class)))
