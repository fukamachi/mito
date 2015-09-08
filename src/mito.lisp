(in-package :cl-user)
(defpackage mito
  (:use #:cl)
  (:import-from #:mito.connection
                #:*connection*
                #:check-connected
                #:connection-quote-character
                #:connect-toplevel
                #:disconnect-toplevel)
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
                #:dao-table-class)
  (:import-from #:mito.util
                :unlispify)
  (:import-from #:dbi
                #:with-transaction
                #:do-sql)
  (:import-from #:sxql
                #:*quote-character*
                #:yield
                #:insert-into
                #:update
                #:delete-from
                #:where
                #:make-clause)
  (:export #:table-class
           #:table-column-class
           #:table-name

           #:dao-class
           #:dao-table-class

           #:*connection*
           #:connect-toplevel
           #:disconnect-toplevel

           #:insert-dao
           #:create-dao
           #:update-dao
           #:delete-dao))
(in-package :mito)

(defun execute-sxql (sxql)
  (check-connected)
  (let ((sxql:*quote-character* (or sxql:*quote-character*
                                    (connection-quote-character *connection*))))
    (multiple-value-bind (sql binds)
        (sxql:yield sxql)
      (apply #'dbi:do-sql *connection* sql binds))))

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
        (execute-sxql
         (sxql:insert-into (intern (table-name (class-of obj)) :keyword)
           (make-set-clause obj)))
        (when serial-key
          (setf (slot-value obj serial-key)
                (last-insert-id *connection* (table-name (class-of obj)) serial-key))))
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
        (error "Unknown primary key in ~S." (table-name (class-of obj))))

      (execute-sxql
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
        (error "Unknown primary key in ~S." (table-name (class-of obj))))

      (execute-sxql
       (sxql:delete-from (intern (table-name (class-of obj)) :keyword)
         (sxql:where
          `(:and ,@(mapcar (lambda (key)
                             `(:= ,(unlispify key) ,(slot-value obj key)))
                           primary-key))))))))
