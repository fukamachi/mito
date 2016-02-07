(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-bind (#+sbcl (warning #'muffle-warning))
    (defpackage mito.dao
      (:use #:cl
            #:sxql
            #:mito.class)
      (:import-from #:mito.dao.column
                    #:dao-table-column-foreign-class
                    #:dao-table-column-rel-key
                    #:dao-table-column-rel-column-name
                    #:dao-table-column-rel-key-fn)
      (:import-from #:mito.connection
                    #:*connection*
                    #:check-connected)
      (:import-from #:mito.class
                    #:database-column-slots)
      (:import-from #:mito.db
                    #:last-insert-id
                    #:execute-sql
                    #:retrieve-by-sql)
      (:import-from #:mito.logger
                    #:with-sql-logging)
      (:import-from #:mito.util
                    #:unlispify
                    #:symbol-name-literally
                    #:ensure-class)
      (:import-from #:dbi
                    #:with-transaction)
      (:import-from #:alexandria
                    #:ensure-list
                    #:once-only
                    #:with-gensyms)
      (:export #:insert-dao
               #:update-dao
               #:create-dao
               #:delete-dao
               #:save-dao
               #:select-dao
               #:includes
               #:find-dao
               #:retrieve-dao
               #:count-dao
               #:recreate-table
               #:ensure-table-exists))))
(in-package :mito.dao)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-reexport:reexport-from :mito.dao.mixin)
  (cl-reexport:reexport-from :mito.dao.table))

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
      obj))
  (:method :before ((obj record-timestamps-mixin))
    (let ((now (local-time:now)))
      (setf (object-created-at obj) now)
      (setf (object-updated-at obj) now))))

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
                           primary-key))))))
    (values))
  (:method :before ((obj record-timestamps-mixin))
    (let ((now (local-time:now)))
      (setf (object-updated-at obj) now))))

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
        (setf (dao-synced obj) nil)))
    (values)))

(defgeneric save-dao (obj)
  (:method ((obj dao-class))
    (if (dao-synced obj)
        (update-dao obj)
        (insert-dao obj))))

(defun select-by-sql (class sql)
  (mapcar (lambda (result)
            (apply #'make-dao-instance class result))
          (retrieve-by-sql sql)))

(defun include-foreign-objects (class records)
  (when records
    (let* ((foreign-class (class-of (first records)))
           (foreign-slots (remove-if-not (lambda (slot)
                                           (eq (dao-table-column-foreign-class slot)
                                               class))
                                         (table-column-slots foreign-class))))
      (unless foreign-slots
        (error "~S is not related to ~S" class foreign-class))
      (when (cdr foreign-slots)
        (error "Cannot use 'includes' with a class which has composite primary keys."))
      (let* ((rel-slot (dao-table-column-rel-key (first foreign-slots)))
             (sql
               (sxql:select :*
                 (sxql:from (sxql:make-sql-symbol (table-name class)))
                 (sxql:where
                  (:in (sxql:make-sql-symbol (table-column-name rel-slot))
                       (loop for obj in records
                             collect (slot-value obj (c2mop:slot-definition-name (first foreign-slots))))))))
             (results
               (select-by-sql class sql)))
        (dolist (obj records)
          (setf (slot-value obj (dao-table-column-rel-column-name (first foreign-slots)))
                (find-if (lambda (result)
                           (equal (slot-value result (c2mop:slot-definition-name rel-slot))
                                  (slot-value obj (c2mop:slot-definition-name (first foreign-slots)))))
                         results)))
        records))))

(defmacro select-dao (class &body clauses)
  (with-gensyms (sql clause results include-classes foreign-class)
    (once-only (class)
      `(progn
         (setf ,class (ensure-class ,class))
         (let* ((sxql:*sql-symbol-conversion* #'unlispify)
                (,sql
                  (sxql:select :*
                    (sxql:from (sxql:make-sql-symbol (table-name ,class)))))
                (,include-classes '()))
           (flet ((includes (&rest classes)
                    (setf ,include-classes (mapcar #'ensure-class classes))
                    nil))
             (dolist (,clause (list ,@clauses))
               (when ,clause
                 (add-child ,sql ,clause)))
             (let ((,results (select-by-sql ,class ,sql)))
               (dolist (,foreign-class ,include-classes)
                 (include-foreign-objects ,foreign-class ,results))
               ,results)))))))

(defun where-and (fields-and-values)
  (when fields-and-values
    (sxql:where `(:and
                  ,@(loop for (field value) on fields-and-values by #'cddr
                          collect `(:= ,field ,value))))))

(defun find-dao (class &rest fields-and-values)
  (first
   (select-dao class
     (where-and fields-and-values)
     (sxql:limit 1))))

(defun retrieve-dao (class &rest fields-and-values)
  (select-dao class
    (where-and fields-and-values)))

(defun count-dao (class &rest fields-and-values)
  (setf class (ensure-class class))
  (let ((sql (sxql:select ((:as (:count :*) :count))
               (sxql:from (sxql:make-sql-symbol (table-name class))))))
    (when fields-and-values
      (add-child sql
                 (where-and fields-and-values)))
    (getf (first
           (retrieve-by-sql sql))
          :count)))

(defun ensure-table-exists (class)
  (with-sql-logging
    (execute-sql (table-definition class :if-not-exists t))))

(defun recreate-table (class)
  (setf class (ensure-class class))
  (with-sql-logging
    (execute-sql (sxql:drop-table (sxql:make-sql-symbol (table-name class))))
    (execute-sql (table-definition class))))
