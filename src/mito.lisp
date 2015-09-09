(in-package :cl-user)
(defpackage mito
  (:use #:cl
        #:mito.error)
  (:import-from #:mito.connection
                #:*connection*
                #:check-connected
                #:connection-quote-character
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
                #:last-insert-id)
  (:import-from #:mito.dao
                #:dao-class
                #:dao-table-class
                #:dao-synced)
  (:import-from #:mito.util
                #:lispify
                #:unlispify)
  (:import-from #:dbi
                #:with-transaction
                #:do-sql
                #:prepare
                #:execute
                #:fetch-all)
  (:import-from #:sxql
                #:*quote-character*
                #:yield
                #:insert-into
                #:update
                #:delete-from
                #:select
                #:from
                #:where
                #:add-child
                #:make-clause)
  (:import-from #:sxql.sql-type
                #:sql-statement)
  (:export #:table-class
           #:table-column-class
           #:table-name

           #:dao-class
           #:dao-table-class

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
           #:select-dao))
(in-package :mito)

(defmacro with-quote-char (&body body)
  `(let ((sxql:*quote-character* (or sxql:*quote-character*
                                     (connection-quote-character *connection*))))
     ,@body))

(defgeneric execute-sql (sql &optional binds)
  (:method :before (sql &optional binds)
    (declare (ignore sql binds))
    (check-connected))
  (:method ((sql string) &optional binds)
    (apply #'dbi:do-sql *connection* sql binds))
  (:method ((sql sql-statement) &optional binds)
    (declare (ignore binds))
    (with-quote-char
      (multiple-value-bind (sql binds)
          (sxql:yield sql)
        (apply #'dbi:do-sql *connection* sql binds)))))

(defun make-dao-instance (class &rest initargs)
  (when (symbolp class)
    (setf class (find-class class)))

  (assert (and class
               (typep class 'dao-table-class)))

  (let ((obj (make-instance class)))
    ;; Ignore columns which is not defined in defclass as a slot.
    (loop with undef = '#:undef
          for column in (database-column-slots class)
          for val = (getf initargs (intern (symbol-name (table-column-name column)) :keyword)
                          undef)
          unless (eq val undef)
            do (setf (slot-value obj (table-column-name column)) val))
    (setf (dao-synced obj) t)
    obj))

(defgeneric retrieve-by-sql (sql &key binds as)
  (:method :before (sql &key binds as)
    (declare (ignore sql binds as))
    (check-connected))
  (:method ((sql string) &key binds as)
    (let* ((results
             (dbi:fetch-all
              (apply #'dbi:execute (dbi:prepare *connection* sql)
                     binds)))
           (results
             (loop for (k v) on results by #'cddr
                   collect (lispify k)
                   collect v)))
      (if as
          (mapcar (lambda (result)
                    (apply #'make-dao-instance as result))
                  results)
          results)))
  (:method ((sql sql-statement) &key binds as)
    (declare (ignore binds))
    (with-quote-char
      (multiple-value-bind (sql binds)
          (sxql:yield sql)
        (retrieve-by-sql sql :binds binds :as as)))))

(defun make-set-clause (obj)
  (apply #'sxql:make-clause :set=
         (mapcan
          (lambda (slot)
            (let ((slot-name (c2mop:slot-definition-name slot)))
              (if (slot-boundp obj slot-name)
                  (let ((value (slot-value obj slot-name)))
                    (list (intern (symbol-name (table-column-name slot)) :keyword)
                          value))
                  nil)))
          (database-column-slots (class-of obj)))))

(defgeneric insert-dao (obj)
  (:method ((obj dao-class))
    (check-connected)
    (let ((serial-key (table-serial-key (class-of obj))))
      (dbi:with-transaction *connection*
        (execute-sql
         (sxql:insert-into (intern (table-name (class-of obj)) :keyword)
           (make-set-clause obj)))
        (when serial-key
          (setf (slot-value obj serial-key)
                (last-insert-id *connection* (table-name (class-of obj)) serial-key))))
      (setf (dao-synced obj) t)
      obj)))

(defgeneric create-dao (class &rest initargs)
  (:method ((class-name symbol) &rest initargs)
    (apply #'create-dao (find-class class-name) initargs))
  (:method ((class dao-table-class) &rest initargs)
    (let ((obj (apply #'make-instance class initargs)))
      (insert-dao obj))))

(defgeneric update-dao (obj)
  (:method ((obj dao-class))
    (check-connected)
    (let ((primary-key (table-primary-key (class-of obj))))
      (unless primary-key
        (error 'no-primary-keys :table (table-name (class-of obj))))

      (execute-sql
       (sxql:update (intern (table-name (class-of obj)) :keyword)
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
           (sxql:delete-from (intern (table-name (class-of obj)) :keyword)
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
              (sxql:from (intern (table-name class) :keyword)))))
      (dolist (ex expressions)
        (sxql:add-child select-sql ex))

      (retrieve-by-sql select-sql :as class))))
