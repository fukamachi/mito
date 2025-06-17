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
  (:import-from #:closer-mop)
  (:import-from #:alexandria
                #:ensure-list
                #:compose
                #:delete-from-plist
                #:remove-from-plist)
  (:export #:*auto-migration-mode*
           #:*migration-keep-temp-tables*
           #:migrate-table
           #:migration-expressions))
(in-package :mito.migration.table)

(defvar *auto-migration-mode* nil)

(defvar *migration-keep-temp-tables* nil
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

(defun plist= (plist1 plist2 &key (test 'equalp))
  (every test
         (sort
          (loop for (k v) on plist1 by #'cddr
                collect (cons k v))
          #'string<
          :key #'car)
         (sort
          (loop for (k v) on plist2 by #'cddr
                collect (cons k v))
          #'string<
          :key #'car)))

(defun column-definition-equal-p (column1 column2)
  (and (equal (first column1) (first column2))
       (plist= (cdr column1) (cdr column2))))

(defun migration-expressions-between-for-sqlite3 (class from-columns to-columns from-indices to-indices)
  (let* ((table-name (table-name class))
         (tmp-table-name (gensym table-name)))

    (unless (every #'null
                   (append (cdr (multiple-value-list
                                 (list-diff from-columns to-columns
                                            :key #'cdr
                                            :test #'plist=
                                            :sort-fn (constantly t))))
                           (cdr (multiple-value-list
                                 (list-diff from-indices to-indices
                                            :key #'cdr
                                            :test #'plist=
                                            :sort-fn (constantly t))))))
      (append
       (list
        (sxql:alter-table (sxql:make-sql-symbol table-name)
          (sxql:rename-to tmp-table-name)))

       (create-table-sxql class :sqlite3)

       (multiple-value-bind (columns-intersection
                             columns-to-delete
                             columns-to-add)
           (list-diff from-columns to-columns
                      :key #'car)
         (declare (ignore columns-to-delete))
         (let ((new-columns-have-default
                 (loop for new-column in columns-to-add
                       for slot = (find-slot-by-name class (lispify (car new-column))
                                                     :test #'string-equal)
                       when slot
                       append
                       (cond
                         ((c2mop:slot-definition-initfunction slot)
                          (list
                           (cons (car new-column)
                                 (convert-for-driver-type
                                  :sqlite3
                                  (table-column-type slot)
                                  (dao-table-column-deflate slot
                                                            (funcall (c2mop:slot-definition-initfunction slot)))))))
                         (t
                          (warn "Adding a non-null column ~S but there's no :initform to set default"
                                (car new-column))
                          nil)))))
           (list
            (sxql:insert-into (sxql:make-sql-symbol table-name)
              (append (mapcar (compose #'sxql:make-sql-symbol #'car)
                              columns-intersection)
                      (mapcar (compose #'sxql:make-sql-symbol #'car)
                              new-columns-have-default))
              (sxql:select
                  (apply #'sxql:make-clause :fields
                         (append (mapcar (compose #'sxql:make-sql-symbol #'car)
                                         columns-intersection)
                                 (mapcar #'cdr new-columns-have-default)))
                (sxql:from tmp-table-name))))))
       (unless *migration-keep-temp-tables*
         (list (sxql:drop-table tmp-table-name)))))))

(defun migration-expressions-between (class driver-type from-columns to-columns from-indices to-indices)
  (when (eq driver-type :sqlite3)
    (return-from migration-expressions-between
      (list (migration-expressions-between-for-sqlite3
             class from-columns to-columns from-indices to-indices))))

  (let ((table-name (table-name class)))
    (multiple-value-bind (columns-intersection
                          columns-to-delete
                          columns-to-add)
        (list-diff from-columns to-columns
                   :key #'car)

      (multiple-value-bind (indices-intersection
                            indices-to-delete
                            indices-to-add)
          (list-diff from-indices to-indices
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
         (when columns-to-add
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
                                                       ;; Set the default only for 'up' migration when adding a new column.
                                                       (when slot
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
                                                            nil))))
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
                                (nreverse drop-defaults))))))))
         ;; drop columns
         (when columns-to-delete
           (list
            (apply #'sxql:make-statement :alter-table (sxql:make-sql-symbol table-name)
                   (mapcar (lambda (column)
                             (sxql:drop-column (sxql:make-sql-symbol (car column))))
                           columns-to-delete))))
         ;; change columns
         (loop with before-alter-sequences = '()
               with after-alter-sequences = '()
               for db-column in columns-intersection
               for table-column = (find (car db-column) to-columns :test #'string= :key #'car)
               unless (column-definition-equal-p (cons (car db-column)
                                                       (remove-from-plist (cdr db-column) :primary-key))
                                                 (cons (car table-column)
                                                       (remove-from-plist (cdr table-column) :primary-key)))
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
                                    (:default
                                        (when v
                                          (sxql:make-clause :set-default v)))
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
         ;; add indices
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
         ;; drop indices
         (loop for (index-name . options) in indices-to-delete
               ;; Ignore if the index's columns are going to be dropped.
               unless (every (lambda (col)
                               (find col columns-to-delete
                                     :key #'car
                                     :test #'string=))
                             (getf options :columns))
               append
                  (if (eq driver-type :postgres)
                      (if (getf options :primary-key)
                          (list
                           (sxql:make-statement :alter-table (sxql:make-sql-symbol table-name)
                                                (sxql:drop-constraint (sxql:make-sql-symbol index-name))))
                          (list
                           (sxql:drop-index index-name)))
                      (nconc
                       (when (getf options :primary-key)
                         (let ((column (and (null (cdr (getf options :columns)))
                                            (find (car (getf options :columns)) from-columns
                                                  :test 'equal
                                                  :key #'first))))
                           (list (sxql:make-statement :alter-table (sxql:make-sql-symbol table-name)
                                                      (if (and column
                                                               (getf (cdr column) :auto-increment))
                                                          (apply #'sxql:make-clause :modify-column (sxql:make-sql-symbol (car column))
                                                                 (remove-from-plist (cdr column) :auto-increment :primary-key))
                                                          (sxql:drop-primary-key))))))
                       (list
                        (sxql:drop-index index-name
                                         :on (sxql:make-sql-symbol table-name)))))))))))

(defun omit-default (definitions)
  (mapcar (lambda (definition)
            (cons (car definition)
                  (delete-from-plist (cdr definition) :default)))
          definitions))

(defun migration-expressions-aux (class driver-type)
  (let* ((table-name (table-name class))
         (table-columns
           (mapcar (lambda (column)
                     (let ((info (table-column-info column driver-type)))
                       (setf (getf (cdr info) :type)
                             (get-column-real-type *connection* (getf (cdr info) :type)))
                       info))
                   (database-column-slots class)))
         (table-indices (table-indices-info class driver-type))
         (db-columns (omit-default (column-definitions *connection* table-name)))
         (db-indices (table-indices *connection* table-name)))
    (values
     (migration-expressions-between class driver-type
                                    db-columns table-columns
                                    db-indices table-indices)
     ;; Don't generate down migration files for SQLite3,
     ;; as it writes CREATE TABLE statements from the class definition
     ;; and it ends up to be the same in both up and down.
     (unless (eq driver-type :sqlite3)
       (migration-expressions-between class driver-type
                                      table-columns db-columns
                                      table-indices db-indices)))))

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
              (values
               (table-definition class :or-replace t)
               (list (sxql:make-statement :drop-view (table-name class)))))
       (execute-sql
        (sxql:make-statement :drop-view
                             (format nil "__~A" (table-name class))))))
    (dao-table-class
     (flet ((order-expressions (expressions-groups)
              (destructuring-bind (add-columns
                                   &optional
                                   drop-columns
                                   change-columns
                                   add-indices
                                   drop-indices)
                  expressions-groups
                (append
                 drop-indices
                 drop-columns
                 add-columns
                 change-columns
                 add-indices))))
       (multiple-value-bind (up down)
           (migration-expressions-aux class driver-type)
         (values (order-expressions up)
                 (and down (order-expressions down))))))))

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
