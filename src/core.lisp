(in-package :cl-user)
(defpackage mito.core
  (:use #:cl
        #:mito.error)
  (:import-from #:mito.connection
                #:*connection*
                #:check-connected
                #:connect-toplevel
                #:disconnect-toplevel
                #:with-connection)
  (:import-from #:mito.class
                #:table-class
                #:table-column-class
                #:table-name
                #:table-primary-key
                #:table-serial-key
                #:table-column-name
                #:database-column-slots)
  (:import-from #:mito.db
                #:last-insert-id
                #:execute-sql
                #:retrieve-by-sql)
  (:import-from #:mito.dao
                #:object-id
                #:dao-class
                #:dao-table-class
                #:dao-synced
                #:inflate
                #:deflate
                #:table-definition
                #:dao-table-column-rel-key-fn
                #:make-dao-instance)
  (:import-from #:mito.logger
                #:logger-stream
                #:with-sql-logging)
  (:import-from #:mito.util
                #:lispify
                #:unlispify
                #:symbol-name-literally)
  (:import-from #:dbi
                #:with-transaction)
  (:import-from #:sxql
                #:insert-into
                #:update
                #:delete-from
                #:select
                #:from
                #:where
                #:add-child
                #:make-clause)
  (:export #:object-id
           #:table-class
           #:table-column-class
           #:table-name

           #:dao-class
           #:dao-table-class

           #:inflate
           #:deflate

           #:logger-stream

           #:*connection*
           #:connect-toplevel
           #:disconnect-toplevel
           #:with-connection

           #:mito-error
           #:invalid-definition
           #:col-type-required
           #:no-primary-keys
           #:connection-not-established

           #:execute-sql
           #:retrieve-by-sql
           #:insert-dao
           #:create-dao
           #:update-dao
           #:delete-dao
           #:save-dao
           #:select-dao
           #:find-dao

           #:table-definition
           #:ensure-table-exists
           #:recreate-table))
(in-package :mito.core)

(defun make-set-clause (obj)
  (apply #'sxql:make-clause :set=
         (mapcan
          (lambda (slot)
            (let ((slot-name (c2mop:slot-definition-name slot)))
              (cond
                ((dao-table-column-rel-key-fn slot)
                 (let ((val (funcall (dao-table-column-rel-key-fn slot) obj)))
                   (list (sxql:make-sql-symbol (table-column-name slot))
                         val)))
                ((not (slot-boundp obj slot-name))
                 nil)
                (t
                 (let ((value (slot-value obj slot-name)))
                   (list (sxql:make-sql-symbol (table-column-name slot))
                         (deflate obj slot-name value)))))))
          (database-column-slots (class-of obj)))))

(defgeneric insert-dao (obj)
  (:method ((obj dao-class))
    (check-connected)
    (let ((serial-key (table-serial-key (class-of obj))))
      (dbi:with-transaction *connection*
        (execute-sql
         (sxql:insert-into (sxql:make-sql-symbol (table-name (class-of obj)))
           (make-set-clause obj)))
        (when serial-key
          (setf (slot-value obj serial-key)
                (last-insert-id *connection* (table-name (class-of obj))
                                (unlispify (symbol-name-literally serial-key))))))
      (setf (dao-synced obj) t)
      obj)))

(defgeneric create-dao (class &rest initargs)
  (:method ((class-name symbol) &rest initargs)
    (apply #'create-dao (find-class class-name) initargs))
  (:method ((class dao-table-class) &rest initargs)
    (let ((obj (apply #'make-dao-instance class initargs)))
      (insert-dao obj))))

(defgeneric update-dao (obj)
  (:method ((obj dao-class))
    (check-connected)
    (let ((primary-key (table-primary-key (class-of obj))))
      (unless primary-key
        (error 'no-primary-keys :table (table-name (class-of obj))))

      (execute-sql
       (sxql:update (sxql:make-sql-symbol (table-name (class-of obj)))
         (make-set-clause obj)
         (sxql:where
          `(:and ,@(mapcar (lambda (key)
                             `(:= ,(unlispify key) ,(slot-value obj key)))
                           primary-key))))))))

(defgeneric delete-dao (obj)
  (:method ((obj dao-class))
    (check-connected)
    (let ((primary-key (table-primary-key (class-of obj))))
      (unless primary-key
        (error 'no-primary-keys :table (table-name (class-of obj))))

      (prog1
          (execute-sql
           (sxql:delete-from (sxql:make-sql-symbol (table-name (class-of obj)))
             (sxql:where
              `(:and ,@(mapcar (lambda (key)
                                 `(:= ,(unlispify key) ,(slot-value obj key)))
                               primary-key)))))
        (setf (dao-synced obj) nil)))))

(defgeneric save-dao (obj)
  (:method ((obj dao-class))
    (if (dao-synced obj)
        (update-dao obj)
        (insert-dao obj))))

(defgeneric select-dao (class &rest expressions)
  (:method :before (class &rest expressions)
    (declare (ignore class expressions))
    (check-connected))
  (:method ((class symbol) &rest expressions)
    (apply #'select-dao (find-class class) expressions))
  (:method ((class dao-table-class) &rest expressions)
    (let ((select-sql
            (sxql:select :*
              (sxql:from (sxql:make-sql-symbol (table-name class))))))
      (dolist (ex expressions)
        (sxql:add-child select-sql ex))

      (mapcar (lambda (result)
                (apply #'make-dao-instance class result))
              (retrieve-by-sql select-sql)))))

(defgeneric find-dao (class &rest pk-values)
  (:method :before (class &rest pk-values)
    (declare (ignore class pk-values))
    (check-connected))
  (:method ((class dao-table-class) &rest pk-values)
    (let ((primary-key (table-primary-key class)))
      (unless primary-key
        (error 'no-primary-keys :table (table-name class)))

      (let ((sql
              (sxql:select :*
                (sxql:from (sxql:make-sql-symbol (table-name class)))
                (sxql:where `(:and ,@(mapcar (lambda (key val)
                                               `(:= ,(unlispify key) ,val))
                                             primary-key
                                             pk-values)))
                (sxql:limit 1))))
        (apply #'make-dao-instance class (first (retrieve-by-sql sql)))))))

(defun ensure-table-exists (class)
  (with-sql-logging
    (execute-sql (table-definition class :if-not-exists t))))

(defun recreate-table (class)
  (when (symbolp class)
    (setf class (find-class class)))
  (with-sql-logging
    (execute-sql (sxql:drop-table (sxql:make-sql-symbol (table-name class))))
    (execute-sql (table-definition class))))
