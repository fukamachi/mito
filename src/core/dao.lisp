(uiop:define-package #:mito.dao
  (:use #:cl
        #:sxql
        #:mito.class)
  (:use-reexport #:mito.dao.mixin
                 #:mito.dao.view
                 #:mito.dao.table)
  (:import-from #:mito.dao.column
                #:dao-table-column-deflate)
  (:import-from #:mito.conversion
                #:convert-for-driver-type)
  (:import-from #:mito.connection
                #:*connection*
                #:check-connected
                #:driver-type)
  (:import-from #:mito.class
                #:database-column-slots
                #:ghost-slot-p
                #:find-slot-by-name
                #:find-parent-column
                #:find-child-columns
                #:table-name
                #:table-column-references-column
                #:table-column-name
                #:table-column-type
                #:table-column-not-null-p)
  (:import-from #:mito.db
                #:last-insert-id
                #:execute-sql
                #:retrieve-by-sql
                #:table-exists-p
                #:ensure-sql)
  (:import-from #:mito.logger
                #:with-sql-logging)
  (:import-from #:mito.util
                #:lispify
                #:unlispify
                #:symbol-name-literally
                #:ensure-class
                #:execute-with-retry)
  (:import-from #:trivia
                #:match
                #:guard)
  (:import-from #:alexandria
                #:appendf
                #:ensure-list
                #:once-only
                #:with-gensyms)
  (:export #:insert-dao
           #:update-dao
           #:create-dao
           #:delete-dao
           #:delete-by-values
           #:save-dao
           #:select-dao
           #:select-by-sql
           #:includes
           #:include-foreign-objects
           #:find-dao
           #:retrieve-dao
           #:count-dao
           #:recreate-table
           #:ensure-table-exists
           #:deftable
           #:do-select))
(in-package #:mito.dao)

(defun foreign-value (obj slot)
  (let* ((class (class-of obj))
         (foreign-slot (table-column-references-column slot))
         (rel-column-name (find-parent-column class slot)))
    (if (and rel-column-name
             (slot-boundp obj rel-column-name))
        (values (and (slot-value obj rel-column-name)
                     (slot-value (slot-value obj rel-column-name)
                                 (c2mop:slot-definition-name foreign-slot)))
                t)
        (values nil nil))))

(defun make-set-clause (obj &key columns)
  (let* ((class (class-of obj))
         (column-slots (database-column-slots class)))
    (when columns
      (setf column-slots
            (remove-if-not (lambda (slot)
                             (let ((slot-name (c2mop:slot-definition-name slot)))
                               (find-if (lambda (column-name)
                                          (typecase column-name
                                            ((and symbol (not keyword))
                                             (eq column-name slot-name))
                                            (otherwise
                                             (string= column-name slot-name))))
                                        columns)))
                           column-slots)))
    (apply #'sxql:make-clause :set=
           (mapcan
            (lambda (slot)
              (let ((slot-name (c2mop:slot-definition-name slot)))
                (cond
                  ((table-column-references-column slot)
                   (multiple-value-bind (value win)
                       (foreign-value obj slot)
                     (cond
                       (win
                        (list (sxql:make-sql-symbol (table-column-name slot))
                              value))
                       ((slot-boundp obj slot-name)
                        (list (sxql:make-sql-symbol (table-column-name slot))
                              (slot-value obj slot-name)))
                       (t nil))))
                  ((not (slot-boundp obj slot-name))
                   nil)
                  (t
                   (let ((value (slot-value obj slot-name)))
                     (list (sxql:make-sql-symbol (table-column-name slot))
                           (convert-for-driver-type (driver-type)
                                                    (table-column-type slot)
                                                    (dao-table-column-deflate slot value))))))))
            column-slots))))

(defgeneric insert-dao (obj)
  (:method ((obj dao-class))
    (check-connected)
    (let ((serial-key (table-serial-key (class-of obj))))
      (execute-sql
       (sxql:insert-into (sxql:make-sql-symbol (table-name (class-of obj)))
         (make-set-clause obj)))
      (when (and serial-key
                 ;; We only want to retrieve last-insert-id
                 ;; in case if user didn't set it manually
                 ;; for the inserted object:
                 (or (not (slot-boundp obj serial-key))
                     (null (slot-value obj serial-key))))
        (setf (slot-value obj serial-key)
              (last-insert-id *connection* (table-name (class-of obj))
                              (unlispify (symbol-name-literally serial-key)))))
      (setf (dao-synced obj) t)
      obj))
  (:method :before ((obj record-timestamps-mixin))
    (let ((now (local-time:now)))
      (setf (object-created-at obj) now)
      (setf (object-updated-at obj) now)))
  (:documentation "Insert the object OBJ into the DB."))

(defgeneric create-dao (class &rest initargs)
  (:method ((class-name symbol) &rest initargs)
    (apply #'create-dao (find-class class-name) initargs))
  (:method ((class dao-table-class) &rest initargs)
    (let ((obj (apply #'make-instance class initargs)))
      (setf (dao-synced obj) nil)
      (save-dao obj)))
  (:documentation "Create an object of class CLASS with INITARGS and save it into the DB.

Example:

(mito:create-dao 'user :name \"Eitaro Fukamachi\" :email \"e.arrows@gmail.com\")

same as:

(defvar me
  (make-instance 'user :name \"Eitaro Fukamachi\" :email \"e.arrows@gmail.com\"))
;=> USER

(mito:insert-dao me)"))

(defgeneric update-dao (obj &key columns)
  (:method ((obj dao-class) &key columns)
    (check-connected)
    (let ((primary-key (table-primary-key (class-of obj))))
      (unless primary-key
        (error 'no-primary-keys :table (table-name (class-of obj))))

      (execute-sql
       (sxql:update (sxql:make-sql-symbol (table-name (class-of obj)))
         (make-set-clause obj :columns columns)
         (sxql:where
          `(:and ,@(mapcar (lambda (key)
                             `(:= ,(unlispify key) ,(slot-value obj key)))
                           primary-key))))))
    (values))
  (:method :before ((obj record-timestamps-mixin) &key columns)
    (declare (ignore columns))
    (let ((now (local-time:now)))
      (setf (object-updated-at obj) now)))
  (:documentation "Update the object OBJ into the DB."))

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
    (values))
  (:documentation "Delete the object OBJ from the DB."))

(defgeneric delete-by-values (class &rest fields-and-values)
  (:method ((class symbol) &rest fields-and-values)
    (apply #'delete-by-values (find-class class) fields-and-values))
  (:method ((class dao-table-class) &rest fields-and-values)
    (let ((sxql:*sql-symbol-conversion* #'unlispify))
      (execute-sql
       (sxql:delete-from (sxql:make-sql-symbol (table-name class))
         (where-and fields-and-values class))))
    (values))
  (:documentation "Delete the records of class CLASS matching FIELDS-AND-VALUES from the DB.
For example: (mito:delete-by-values 'user :id 1)"))

(defgeneric save-dao (obj)
  (:method ((obj dao-class))
    (if (dao-synced obj)
        (update-dao obj)
        (insert-dao obj)))
  (:documentation "Save the object OBJ into the DB."))

(defstruct mito-cursor
  cursor
  fields
  class)

(defun select-by-sql-as-cursor (class sql &key binds)
  (multiple-value-bind (sql yield-binds)
      (ensure-sql sql)
    (let* ((cursor (dbi:make-cursor *connection* sql))
           (cursor (execute-with-retry cursor (or binds yield-binds))))
      (make-mito-cursor :cursor cursor
                        :fields (mapcar (lambda (column-name)
                                          (intern (lispify (string-upcase column-name)) :keyword))
                                        (dbi.driver:query-fields cursor))
                        :class class))))

(defun fetch-dao-from-cursor (cursor)
  (let ((fields (mito-cursor-fields cursor))
        (row (dbi:fetch (mito-cursor-cursor cursor)
                        :format :values)))
    (when row
      (apply #'make-dao-instance (mito-cursor-class cursor)
             (loop for field in fields
                   for value in row
                   collect field
                   collect value)))))

(defparameter *want-cursor* nil)

(defun select-by-sql (class sql &key binds)
  (if *want-cursor*
      (select-by-sql-as-cursor class sql :binds binds)
      (mapcar (lambda (result)
                (apply #'make-dao-instance class result))
              (retrieve-by-sql sql :binds binds))))

(defun include-foreign-objects (foreign-class records)
  (when records
    (let ((*want-cursor* nil)
          (foreign-class (ensure-class foreign-class)))
      (when (cdr (table-primary-key foreign-class))
        (error "Cannot use 'includes' with a class which has composite primary keys."))
      (let* ((class (class-of (first records)))
             (rel-slots (remove-duplicates
                         (remove-if-not (lambda (slot)
                                          (eq (table-column-type slot)
                                              (class-name foreign-class)))
                                        (database-column-slots class)))))
        (unless rel-slots
          (error "~S is not related to ~S" class foreign-class))
        (let* ((foreign-slot (table-column-references-column (first rel-slots)))
               (rel-slot-values (remove nil
                                        (loop for obj in records
                                              append
                                              (mapcar (lambda (rel-slot)
                                                        (slot-value obj (c2mop:slot-definition-name rel-slot)))
                                                      rel-slots)))))
          (unless rel-slot-values
            (return-from include-foreign-objects records))
          (let* ((sql (sxql:select :*
                        (sxql:from (sxql:make-sql-symbol (table-name foreign-class)))
                        (sxql:where
                         (:in (sxql:make-sql-symbol (table-column-name foreign-slot)) rel-slot-values))))
                 (results (select-by-sql foreign-class sql)))
            (dolist (obj records)
              (dolist (rel-slot rel-slots)
                (setf (slot-value obj (find-parent-column class rel-slot))
                      (find-if (lambda (result)
                                 (equal (slot-value result (c2mop:slot-definition-name foreign-slot))
                                        (slot-value obj (c2mop:slot-definition-name rel-slot))))
                               results)))))
          records)))))

(defun child-columns (column class)
  (let ((slot (find-slot-by-name class column :test #'string=)))
    (when (and slot (ghost-slot-p slot))
      (values
       (find-child-columns class slot)
       (table-column-type slot)))))

(defun slot-foreign-value (object class slot-name)
  (slot-value object
              (c2mop:slot-definition-name
               (table-column-references-column
                (find-slot-by-name class slot-name)))))

(defun expand-op (object class)
  "Expand relational columns if the operator is :=, :!= , :in or :not-in."
  (let ((obj (gensym "OBJ"))
        (children (gensym "CHILDREN"))
        (expected-type (gensym "EXPECTED-TYPE")))
    (trivia:match object
      ((or (cons (guard op (or (eql op :=)
                               (eql op :!=)))
                 (cons (guard x (keywordp x)) (cons y nil)))
           (cons (guard op (or (eql op :=)
                               (eql op :!=)))
                 (cons y
                       (cons (guard x (keywordp x))
                             nil))))
       `(multiple-value-bind (,children ,expected-type)
            (child-columns ,x ,class)
          (if ,children
              (progn
                (assert (typep ,y ,expected-type))
                (apply #'sxql:make-op
                       :and
                       (mapcar (lambda (,obj)
                                 (sxql:make-op ,op
                                               (sxql:make-sql-symbol (unlispify (symbol-name-literally ,obj)))
                                               (slot-foreign-value ,y ,class ,obj)))
                               ,children)))
              (sxql:make-op ,op ,x ,y))))
      ((cons (guard op (or (eql op :in)
                           (eql op :not-in)))
             (cons (guard x (keywordp x))
                   (cons y nil)))
       `(multiple-value-bind (,children ,expected-type)
            (child-columns ,x ,class)
          (cond
            ((and ,children
                  (cdr ,children))
             (error "Cannot specify a relational column which has composite primary keys"))
            (,children
             (every (lambda (,obj)
                      (assert (typep ,obj ,expected-type))) ,y)
             (sxql:make-op ,op
                           (sxql:make-sql-symbol (unlispify (symbol-name-literally (first ,children))))
                           (mapcar (lambda (,obj)
                                     (slot-foreign-value ,obj ,class (first ,children)))
                                   ,y)))
            (t (sxql:make-op ,op ,x ,y)))))
      ((cons (trivia:guard op (keywordp op))
             args)
       `(sxql:make-op ,op
                      ,@(mapcar (lambda (arg)
                                  (expand-op arg class)) args)))
      (otherwise object))))

(defmacro select-dao (class &body clauses)
  "Build custom queries with SxQL.

Example:

  (select-dao 'tweet
    (where (:like :status \"%Japan%\")))

You can use \"includes\" to eagerly load another table and prevent the \"N+1 query\" performance problem.

Example:

  (defvar *tweets-contain-japan*
    (select-dao 'tweet
      (includes 'user)
      (where (:like :status \"%Japan%\"))))

See the SxQL documentation for the available clauses and operators."
  (with-gensyms (sql clause results include-classes foreign-class)
    (once-only (class)
      `(#+sb-package-locks locally #+sb-package-locks (declare (sb-ext:disable-package-locks sxql:where))
        #-sb-package-locks cl-package-locks:with-packages-unlocked #-sb-package-locks (sxql)
        (progn
          (setf ,class (ensure-class ,class))
          (let* ((sxql:*sql-symbol-conversion* #'unlispify)
                 (,sql
                   (sxql:select :*
                                (sxql:from (sxql:make-sql-symbol (table-name ,class)))))
                 (,include-classes '()))
            (macrolet ((where (expression)
                         `(sxql:make-clause :where ,(expand-op expression ',class))))
              ;; ignore compiler-note when includes is not used
              #+sbcl (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
              (flet ((includes (&rest classes)
                       (appendf ,include-classes (mapcar #'ensure-class classes))
                       nil))
                (dolist (,clause (list ,@clauses))
                  (when ,clause
                    (add-child ,sql ,clause)))
                (let ((,results (select-by-sql ,class ,sql)))
                  (unless *want-cursor*
                    (dolist (,foreign-class (remove-duplicates ,include-classes))
                      (include-foreign-objects ,foreign-class ,results)))
                  (values ,results ,sql))))))))))

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
                                                 for column = (intern (table-column-name child) :keyword)
                                                 collect
                                                 (cond
                                                   ((null value)
                                                    (unless (table-column-not-null-p slot)
                                                      (warn "Slot ~S in table ~S is not null, but IS NULL condition is specified."
                                                            (table-column-name slot)
                                                            (table-name class)))
                                                    `(:is-null ,column))
                                                   (t
                                                    `(:= ,column
                                                      ,(slot-value value
                                                                   (c2mop:slot-definition-name
                                                                    (table-column-references-column child)))))))))))
                    else
                      collect (let ((db-value
                                      (dao-table-column-deflate slot value)))
                                (cond
                                  ((null db-value)
                                   (unless (table-column-not-null-p slot)
                                     (warn "Slot ~S in table ~S is not null, but IS NULL condition is specified."
                                           (table-column-name slot)
                                           (table-name class)))
                                   `(:is-null ,(unlispify field)))
                                  (t
                                   `(:= ,(unlispify field) ,db-value)))))))
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

(defmacro deftable (name direct-superclasses direct-slots &rest options)
  `(defclass ,name ,direct-superclasses
     ,direct-slots
     (:metaclass dao-table-class)
     ,@(unless (find :conc-name options :key #'car)
               `((:conc-name ,(intern (format nil "~@:(~A-~)" name) (symbol-package name)))))
     ,@options))

(defmacro do-select ((dao select &optional index) &body body)
  (with-gensyms (main main-body select-fn cursor i)
    `(block nil
       (labels ((,main-body (,dao ,(or index i))
                  ,@(and (not index)
                         `((declare (ignore ,i))))
                  ,@body)
                (,select-fn () ,select)
                (,main ()
                  (case (dbi:connection-driver-type *connection*)
                    (:postgres
                     (let ((,cursor (let ((*want-cursor* t))
                                      (,select-fn))))
                       (loop ,@(and index `(for ,i from 0))
                             for ,dao = (fetch-dao-from-cursor ,cursor)
                             while ,dao
                             do (,main-body ,dao ,i))))
                    (otherwise
                     (loop ,@(and index `(for ,i from 0))
                           for ,dao in (,select-fn)
                           do (,main-body ,dao ,i))))))
         (if (dbi:in-transaction *connection*)
             (,main)
             (dbi:with-transaction *connection*
               (,main)))))))
