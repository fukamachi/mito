(in-package :cl-user)
(defpackage mito.dao.table
  (:use #:cl
        #:mito.util
        #:mito.error)
  (:import-from #:mito.connection
                #:driver-type)
  (:import-from #:mito.class
                #:table-class
                #:table-name
                #:table-column-name
                #:table-column-type
                #:table-column-slots
                #:table-column-references-column
                #:create-table-sxql
                #:find-slot-by-name
                #:find-child-columns)
  (:import-from #:mito.dao.column
                #:dao-table-column-class
                #:dao-table-column-inflate)
  (:import-from #:mito.dao.mixin
                #:auto-pk-mixin
                #:record-timestamps-mixin)
  (:export #:dao-class
           #:dao-table-class

           #:dao-synced

           #:make-dao-instance
           #:table-definition

           #:depending-table-classes))
(in-package :mito.dao.table)

(defclass dao-class ()
  ((synced :type boolean
           :initform nil
           :accessor dao-synced)))

(defclass dao-table-class (table-class)
  ((auto-pk :initarg :auto-pk
            :initform '(t))
   (record-timestamps :initarg :record-timestamps
                      :initform '(t))))

(defmethod c2mop:direct-slot-definition-class ((class dao-table-class) &key)
  'dao-table-column-class)

(defun check-col-type (col-type)
  (etypecase col-type
    ((or null keyword) t)
    (symbol (typep (find-class col-type nil) 'dao-table-class))
    (cons
     (optima:ematch col-type
       ((or (list 'or :null (or (keyword) (cons (keyword) _)))
            (list 'or (or (keyword) (cons (keyword) _)) :null)
            (cons (keyword) _))
        t)))))

(defun initargs-enables-auto-pk (initargs)
  (first (or (getf initargs :auto-pk) '(t))))

(defun initargs-enables-record-timestamps (initargs)
  (first (or (getf initargs :record-timestamps) '(t))))

(defun initargs-contains-primary-key (initargs)
  (or (getf initargs :primary-key)
      (find-if (lambda (slot)
                 (getf slot :primary-key))
               (getf initargs :direct-slots))))

(defun make-dao-instance (class &rest initargs)
  (setf class (ensure-class class))

  (assert (and class
               (typep class 'dao-table-class)))

  (let* ((list (loop for (k v) on initargs by #'cddr
                    for column = (find-if (lambda (initargs)
                                            (find k initargs :test #'eq))
                                          (table-column-slots class)
                                          :key #'c2mop:slot-definition-initargs)
                    if column
                    append (list k
                                 (dao-table-column-inflate column v))
                    else
                    append (list k v)))
        (obj
          (apply #'make-instance class
                 :allow-other-keys t
                 list)))
    (setf (dao-synced obj) t)
    obj))

(defun make-relational-reader-method (func-name class slot-name rel-class)
  (let ((generic-function
          (ensure-generic-function func-name :lambda-list '(object))))
    (add-method
     generic-function
     (make-instance 'standard-method
                    :lambda-list '(object)
                    :qualifiers ()
                    :specializers (list class)
                    :function
                    (lambda (object &rest ignore)
                      (declare (ignore ignore))
                      ;; I don't know why but SBCL pass a CONS of the instance instead of the instance itself.
                      (when (consp object)
                        (setf object (first object)))
                      (if (slot-boundp object slot-name)
                          (slot-value object slot-name)
                          (let* ((child-columns (find-child-columns class
                                                                    (find-slot-by-name class slot-name)))
                                 (foreign-object
                                   (and (every (lambda (slot-name)
                                                 (and (slot-boundp object slot-name)
                                                      (slot-value object slot-name)))
                                               child-columns)
                                        (apply #'make-dao-instance rel-class
                                               (first
                                                (mito.db:retrieve-by-sql
                                                 (sxql:select :*
                                                   (sxql:from (sxql:make-sql-symbol (table-name rel-class)))
                                                   (sxql:where
                                                    `(:and
                                                      ,@(mapcar (lambda (slot-name)
                                                                  `(:= ,(sxql:make-sql-symbol
                                                                         (table-column-name
                                                                          (table-column-references-column
                                                                           (find-slot-by-name class slot-name))))
                                                                       ,(slot-value object slot-name)))
                                                                child-columns)))
                                                   (sxql:limit 1))))))))
                            (setf (slot-value object slot-name) foreign-object))))))))

(defun add-relational-readers (class initargs)
  (loop for column in (copy-seq (getf initargs :direct-slots)) ;; Prevent infinite-loop
        for (col-type . not-null) = (let ((col-type (getf column :col-type)))
                                      (optima:match col-type
                                        ((or (list 'or :null x)
                                             (list 'or x :null))
                                         (cons x t))
                                        (otherwise
                                         (cons col-type nil))))
        when (and (symbolp col-type)
                  (not (null col-type))
                  (not (keywordp col-type)))
          do (let* ((name (getf column :name))
                    ;; FIXME: find-class returns NIL if the class is this same class
                    (rel-class (find-class col-type)))
               (dolist (reader (getf column :readers))
                 (make-relational-reader-method reader class name rel-class)))))

(defun depending-table-classes (class)
  (let ((class-name (class-name class)))
    (delete-duplicates
     (loop for column in (table-column-slots class)
           if (mito.class.column:table-column-references column)
             append (let ((col-type (table-column-type column)))
                      (if (eq col-type class-name)
                          nil
                          (list (find-class col-type)))))
     :from-end t
     :test #'eq)))

(defmethod initialize-instance :around ((class dao-table-class) &rest initargs
                                        &key direct-superclasses &allow-other-keys)
  (when (and (initargs-enables-record-timestamps initargs)
             (not (contains-class-or-subclasses 'record-timestamps-mixin direct-superclasses)))
    (setf (getf initargs :direct-superclasses)
          (append (getf initargs :direct-superclasses)
                  (list (find-class 'record-timestamps-mixin)))))

  (unless (contains-class-or-subclasses 'dao-class direct-superclasses)
    (push (find-class 'dao-class) (getf initargs :direct-superclasses)))

  (when (and (initargs-enables-auto-pk initargs)
             (not (initargs-contains-primary-key initargs))
             (not (contains-class-or-subclasses 'auto-pk-mixin direct-superclasses)))
    (push (find-class 'auto-pk-mixin) (getf initargs :direct-superclasses)))

  (let ((class (apply #'call-next-method class initargs)))
    (add-relational-readers class initargs)
    class))

(defmethod reinitialize-instance :around ((class dao-table-class) &rest initargs
                                                                    &key direct-superclasses &allow-other-keys)
  (when (and (initargs-enables-record-timestamps initargs)
             (not (contains-class-or-subclasses 'record-timestamps-mixin direct-superclasses)))
    (setf (getf initargs :direct-superclasses)
          (append (getf initargs :direct-superclasses)
                  (list (find-class 'record-timestamps-mixin)))))

  (when (and (initargs-enables-auto-pk initargs)
             (not (initargs-contains-primary-key initargs))
             (not (contains-class-or-subclasses 'auto-pk-mixin direct-superclasses)))
    (push (find-class 'auto-pk-mixin) (getf initargs :direct-superclasses)))

  (let ((class (apply #'call-next-method class initargs)))
    (add-relational-readers class initargs)
    class))

(defmethod c2mop:ensure-class-using-class :around ((class dao-table-class) name &rest keys
                                                   &key direct-superclasses &allow-other-keys)
  (unless (contains-class-or-subclasses 'dao-class direct-superclasses)
    (setf (getf keys :direct-superclasses)
          (cons (find-class 'dao-class) direct-superclasses)))
  (apply #'call-next-method class name keys))

(defun table-definition (class &key if-not-exists)
  (setf class (ensure-class class))
  (check-type class table-class)
  (create-table-sxql class (driver-type)
                     :if-not-exists if-not-exists))
