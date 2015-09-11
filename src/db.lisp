(in-package :cl-user)
(defpackage mito.db
  (:use #:cl)
  (:import-from #:mito.connection
                #:*connection*
                #:connection-quote-character
                #:check-connected)
  (:import-from #:mito.dao
                #:dao-table-class
                #:dao-synced)
  (:import-from #:mito.class
                #:database-column-slots
                #:table-column-name)
  (:import-from #:mito.util
                #:lispify)
  (:import-from #:dbi
                #:connection-driver-type
                #:do-sql
                #:prepare
                #:execute
                #:fetch-all)
  (:import-from #:sxql
                #:*quote-character*
                #:yield)
  (:import-from #:sxql.sql-type
                #:sql-statement)
  (:export #:last-insert-id
           #:table-indices
           #:column-definitions
           #:execute-sql
           #:retrieve-by-sql))
(in-package :mito.db)

(defun last-insert-id (conn table-name serial-key-name)
  (ecase (dbi:connection-driver-type conn)
    (:mysql    (mito.db.mysql:last-insert-id conn table-name serial-key-name))
    (:postgres (mito.db.postgres:last-insert-id conn table-name serial-key-name))
    (:sqlite3  (mito.db.sqlite3:last-insert-id conn table-name))))

(defun table-indices (conn table-name)
  (sort
   (funcall
    (ecase (dbi:connection-driver-type conn)
      (:mysql    #'mito.db.mysql:table-indices)
      (:postgres #'mito.db.postgres:table-indices)
      (:sqlite3  #'mito.db.sqlite3:table-indices))
    conn table-name)
   (lambda (a b)
     (cond
       ((getf a :primary-key)
        (not (getf b :primary-key)))
       ((getf b :primary-key) nil)
       ((getf a :unique-key)
        (or (not (getf b :unique-key))
            (string< (prin1-to-string a) (prin1-to-string b))))
       (t
        (string< (prin1-to-string a) (prin1-to-string b)))))
   :key #'cdr))

(defun column-definitions (conn table-name)
  (funcall
   (ecase (dbi:connection-driver-type conn)
     (:mysql    #'mito.db.mysql:column-definitions)
     (:postgres #'mito.db.postgres:column-definitions)
     (:sqlite3  #'mito.db.sqlite3:column-definitions))
   conn table-name))

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
