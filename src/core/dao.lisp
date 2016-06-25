(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-bind (#+sbcl (warning #'muffle-warning))
    (defpackage mito.dao
      (:use #:cl
            #:sxql
            #:mito.class)
      (:import-from #:mito.dao.column
                    #:dao-table-column-deflate
                    #:dao-table-column-foreign-class
                    #:dao-table-column-foreign-slot)
      (:import-from #:mito.dao.table
                    #:find-parent-column
                    #:find-child-columns)
      (:import-from #:mito.connection
                    #:*connection*
                    #:check-connected)
      (:import-from #:mito.class
                    #:database-column-slots
                    #:ghost-slot-p
                    #:find-slot-by-name)
      (:import-from #:mito.db
                    #:last-insert-id
                    #:execute-sql
                    #:retrieve-by-sql
                    #:table-exists-p)
      (:import-from #:mito.logger
                    #:with-sql-logging)
      (:import-from #:mito.util
                    #:unlispify
                    #:symbol-name-literally
                    #:ensure-class)
      (:import-from #:optima
                    #:match
                    #:guard)
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

(defun foreign-value (obj slot)
  (let* ((class (class-of obj))
         (foreign-slot (dao-table-column-foreign-slot slot))
         (rel-column-name (find-parent-column class slot)))
    (and rel-column-name
         (slot-boundp obj rel-column-name)
         (slot-value (slot-value obj rel-column-name)
                     (c2mop:slot-definition-name foreign-slot)))))

(defun make-set-clause (obj)
  (let ((class (class-of obj)))
    (apply #'sxql:make-clause :set=
           (mapcan
            (lambda (slot)
              (let ((slot-name (c2mop:slot-definition-name slot)))
                (cond
                  ((and (dao-table-column-foreign-class slot)
                        (foreign-value obj slot))
                   (list (sxql:make-sql-symbol (table-column-name slot))
                         (foreign-value obj slot)))
                  ((not (slot-boundp obj slot-name))
                   nil)
                  (t
                   (let ((value (slot-value obj slot-name)))
                     (list (sxql:make-sql-symbol (table-column-name slot))
                           (funcall (dao-table-column-deflate slot) value)))))))
            (database-column-slots class)))))

(defgeneric insert-dao (obj)
  (:method ((obj dao-class))
    (check-connected)
    (let ((serial-key (table-serial-key (class-of obj))))
      (execute-sql
       (sxql:insert-into (sxql:make-sql-symbol (table-name (class-of obj)))
         (make-set-clause obj)))
      (when serial-key
        (setf (slot-value obj serial-key)
              (last-insert-id *connection* (table-name (class-of obj))
                              (unlispify (symbol-name-literally serial-key)))))
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
      (setf (dao-synced obj) nil)
      (save-dao obj))))

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

(defun include-foreign-objects (foreign-class records)
  (when records
    (let* ((class (class-of (first records)))
           (rel-slots (remove-if-not (lambda (slot)
                                       (eq (dao-table-column-foreign-class slot)
                                           foreign-class))
                                     (table-column-slots class))))
      (unless rel-slots
        (error "~S is not related to ~S" class foreign-class))
      (when (cdr rel-slots)
        (error "Cannot use 'includes' with a class which has composite primary keys."))
      (let* ((foreign-slot (dao-table-column-foreign-slot (first rel-slots)))
             (sql
               (sxql:select :*
                 (sxql:from (sxql:make-sql-symbol (table-name foreign-class)))
                 (sxql:where
                  (:in (sxql:make-sql-symbol (table-column-name foreign-slot))
                       (loop for obj in records
                             collect (slot-value obj (c2mop:slot-definition-name (first rel-slots))))))))
             (results
               (select-by-sql foreign-class sql)))
        (dolist (obj records)
          (setf (slot-value obj (find-parent-column class (first rel-slots)))
                (find-if (lambda (result)
                           (equal (slot-value result (c2mop:slot-definition-name foreign-slot))
                                  (slot-value obj (c2mop:slot-definition-name (first rel-slots)))))
                         results)))
        records))))

(defun child-columns (column class)
  (let ((slot (find-slot-by-name class column :test #'string=)))
    (and slot
         (find-child-columns class slot))))

(defun slot-foreign-value (object class slot-name)
  (slot-value object
              (c2mop:slot-definition-name
               (dao-table-column-foreign-slot
                (find-slot-by-name class slot-name)))))

(defun expand-op (object class)
  "Expand relational columns if the operator is :=, :!= , :in or :not-in."
  (optima:match object
    ((or (cons (guard op (or (eql op :=)
                             (eql op :!=)))
               (cons (guard x (keywordp x)) (cons y nil)))
         (cons (guard op (or (eql op :=)
                             (eql op :!=)))
               (cons y
                     (cons (guard x (keywordp x))
                           nil))))
     `(let ((children (child-columns ,x ,class)))
        (if children
            (apply #'sxql:make-op
                   :and
                   (mapcar (lambda (col)
                             (sxql:make-op ,op
                                           (sxql:make-sql-symbol (unlispify (symbol-name-literally col)))
                                           (slot-foreign-value ,y ,class col)))
                           children))
            (sxql:make-op ,op ,x ,y))))
    ((cons (guard op (or (eql op :in)
                         (eql op :not-in)))
           (cons (guard x (keywordp x))
                 (cons y nil)))
     `(let ((children (child-columns ,x ,class)))
        (cond
          ((and children
                (cdr children))
           (error "Cannot specify a relational column which has composite primary keys"))
          (children
           (sxql:make-op ,op
                         (sxql:make-sql-symbol (unlispify (symbol-name-literally (first children))))
                         (mapcar (lambda (obj)
                                   (slot-foreign-value obj ,class (first children)))
                                 ,y)))
          (t (sxql:make-op ,op ,x ,y)))))
    ((cons (optima:guard op (keywordp op))
           args)
     `(sxql:make-op ,op
                    ,@(mapcar (lambda (arg)
                                (expand-op arg class)) args)))
    (otherwise object)))

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
           (macrolet ((where (expression)
                        `(sxql:make-clause :where ,(expand-op expression ',class))))
             (flet ((includes (&rest classes)
                      (setf ,include-classes (mapcar #'ensure-class classes))
                      nil))
               (dolist (,clause (list ,@clauses))
                 (when ,clause
                   (add-child ,sql ,clause)))
               (let ((,results (select-by-sql ,class ,sql)))
                 (dolist (,foreign-class ,include-classes)
                   (include-foreign-objects ,foreign-class ,results))
                 ,results))))))))

(defun where-and (fields-and-values class)
  (when fields-and-values
    (let ((op (loop for (field value) on fields-and-values by #'cddr
                    for slot = (find-slot-by-name class field :test #'string=)
                    unless slot
                      do (error "Class ~S does not have a slot named ~S" class field)
                    if (ghost-slot-p slot)
                      append (let ((children (mapcar (lambda (slot-name)
                                                       (find-slot-by-name class slot-name :test #'string=))
                                                     (find-child-columns class slot))))
                               (when children
                                 `((:and ,@(loop for child in children
                                                 collect
                                                 `(:= ,(intern (string (c2mop:slot-definition-name child)) :keyword)
                                                      ,(slot-value value
                                                                   (c2mop:slot-definition-name
                                                                    (dao-table-column-foreign-slot child)))))))))
                    else
                      collect `(:= ,field ,value))))
      (when op
        (sxql:where `(:and ,@op))))))

(defun find-dao (class &rest fields-and-values)
  (setf class (ensure-class class))
  (first
   (select-dao class
     (where-and fields-and-values class)
     (sxql:limit 1))))

(defun retrieve-dao (class &rest fields-and-values)
  (setf class (ensure-class class))
  (select-dao class
    (where-and fields-and-values class)))

(defun count-dao (class &rest fields-and-values)
  (setf class (ensure-class class))
  (let ((sql (sxql:select ((:as (:count :*) :count))
               (sxql:from (sxql:make-sql-symbol (table-name class))))))
    (when fields-and-values
      (add-child sql
                 (where-and fields-and-values class)))
    (getf (first
           (retrieve-by-sql sql))
          :count)))

(defun ensure-table-exists (class)
  (setf class (ensure-class class))
  (unless (table-exists-p *connection* (table-name class))
    (with-sql-logging
      (mapc #'execute-sql (table-definition class)))))

(defun recreate-table (class)
  (setf class (ensure-class class))
  (let ((exists (table-exists-p *connection* (table-name class))))
    (with-sql-logging
      (when exists
        (execute-sql (sxql:drop-table (sxql:make-sql-symbol (table-name class)))))
      (mapc #'execute-sql (table-definition class)))))
