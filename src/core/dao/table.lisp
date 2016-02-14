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
                #:table-primary-key
                #:create-table-sxql
                #:ghost-slot-p)
  (:import-from #:mito.dao.column
                #:dao-table-column-class
                #:dao-table-column-foreign-class
                #:dao-table-column-inflate
                #:dao-table-column-deflate)
  (:import-from #:mito.dao.mixin
                #:auto-pk-mixin
                #:record-timestamps-mixin)
  (:export #:dao-class
           #:dao-table-class

           #:dao-synced

           #:inflate
           #:deflate

           #:make-dao-instance
           #:table-definition))
(in-package :mito.dao.table)

(defun get-slot-by-slot-name (class slot-name)
  (find slot-name
        (table-column-slots (if (typep class 'symbol)
                                (find-class class)
                                class))
        :test #'eq
        :key #'c2mop:slot-definition-name))

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

(defmethod initialize-instance :after ((class dao-table-column-class) &rest initargs)
  (declare (ignore initargs))
  (unless (ghost-slot-p class)
    (check-col-type (table-column-type class))))

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

  (let* ((obj (allocate-instance class))
         (obj
           (apply #'make-instance class
                  :allow-other-keys t
                  (loop for (k v) on initargs by #'cddr
                        for column = (find-if (lambda (initargs)
                                                (find k initargs :test #'eq))
                                              (table-column-slots class)
                                              :key #'c2mop:slot-definition-initargs)
                        if column
                          append (list k
                                       (inflate obj (c2mop:slot-definition-name column) v))
                        else
                          append (list k v)))))
    (setf (dao-synced obj) t)
    obj))

(defun initialize-initargs (initargs)
  ;; Add relational column slots (ex. user-id)
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
          do (rplacd (cdr column)
                     `(:ghost t ,@(cddr column)))
             (let* ((name (getf column :name))
                    ;; FIXME: find-class returns NIL if the class is this same class
                    (rel-class (find-class col-type))
                    (pk-names (table-primary-key rel-class)))
               (flet ((rel-column-name (pk-name)
                        (intern
                         (format nil "~A-~A" col-type pk-name)
                         (symbol-package name))))
                 (dolist (pk-name pk-names)
                   (let ((rel-column-name (rel-column-name pk-name))
                         (pk (get-slot-by-slot-name rel-class pk-name)))
                     (rplacd (last (getf initargs :direct-slots))
                             `((:name ,rel-column-name
                                :initargs (,(intern (symbol-name rel-column-name) :keyword))
                                ;; Defer retrieving the relational column type until table-column-info
                                :col-type ,(getf column :col-type)
                                :rel-column-name ,(getf column :name)
                                :foreign-class ,rel-class
                                :rel-key ,pk
                                :rel-key-fn
                                ,(lambda (obj)
                                   (and (slot-boundp obj name)
                                        (slot-value (slot-value obj name) pk-name))))))))

                 (dolist (reader (getf column :readers))
                   (setf (fdefinition reader)
                         (lambda (object)
                           (if (slot-boundp object name)
                               (slot-value object name)
                               (apply #'make-dao-instance rel-class
                                      (first
                                       (mito.db:retrieve-by-sql
                                        (sxql:select :*
                                          (sxql:from (sxql:make-sql-symbol (table-name rel-class)))
                                          (sxql:where
                                           `(:and
                                             ,@(mapcar (lambda (pk-name)
                                                         `(:= ,(sxql:make-sql-symbol
                                                                (table-column-name
                                                                 (get-slot-by-slot-name rel-class pk-name)))
                                                              ,(slot-value object (rel-column-name pk-name))))
                                                       (table-primary-key rel-class))))
                                          (sxql:limit 1))))))))))
               (setf (getf column :readers) '())))
  initargs)

(defun expand-relational-keys (class slot-name)
  (let ((keys (slot-value class slot-name))
        (direct-slots (c2mop:class-direct-slots class)))
    (labels ((expand-key (key)
               (let ((slot (find key direct-slots
                                 :key #'c2mop:slot-definition-name
                                 :test #'eq)))
                 (if (ghost-slot-p slot)
                     (mapcar #'c2mop:slot-definition-name
                             (remove-if-not (lambda (ds)
                                              (eq (dao-table-column-foreign-class ds)
                                                  (find-class (table-column-type slot))))
                                            direct-slots))
                     (list key))))
             (expand-keys (keys)
               (loop for key in keys
                     append (expand-key key))))
      (setf (slot-value class slot-name)
            (loop for key in keys
                  if (listp key)
                    collect (expand-keys key)
                  else
                    append (expand-key key))))))

(defmethod initialize-instance :around ((class dao-table-class) &rest initargs
                                        &key direct-superclasses &allow-other-keys)
  (setf initargs (initialize-initargs initargs))

  (when (and (initargs-enables-record-timestamps initargs)
             (not (contains-class-or-subclasses 'record-timestamps-mixin direct-superclasses)))
    (push (find-class 'record-timestamps-mixin) (getf initargs :direct-superclasses)))

  (unless (contains-class-or-subclasses 'dao-class direct-superclasses)
    (push (find-class 'dao-class) (getf initargs :direct-superclasses)))

  (when (and (initargs-enables-auto-pk initargs)
             (not (initargs-contains-primary-key initargs))
             (not (contains-class-or-subclasses 'auto-pk-mixin direct-superclasses)))
    (push (find-class 'auto-pk-mixin) (getf initargs :direct-superclasses)))

  (let ((class (apply #'call-next-method class initargs)))
    (expand-relational-keys class 'mito.class.table::primary-key)
    (expand-relational-keys class 'mito.class.table::unique-keys)
    (expand-relational-keys class 'mito.class.table::keys)
    class))

(defmethod reinitialize-instance :around ((class dao-table-class) &rest initargs
                                                                    &key direct-superclasses &allow-other-keys)
  (setf initargs (initialize-initargs initargs))

  (when (and (initargs-enables-record-timestamps initargs)
             (not (contains-class-or-subclasses 'record-timestamps-mixin direct-superclasses)))
    (push (find-class 'record-timestamps-mixin) (getf initargs :direct-superclasses)))

  (when (and (initargs-enables-auto-pk initargs)
             (not (initargs-contains-primary-key initargs))
             (not (contains-class-or-subclasses 'auto-pk-mixin direct-superclasses)))
    (push (find-class 'auto-pk-mixin) (getf initargs :direct-superclasses)))

  (let ((class (apply #'call-next-method class initargs)))
    (expand-relational-keys class 'mito.class.table::primary-key)
    (expand-relational-keys class 'mito.class.table::unique-keys)
    (expand-relational-keys class 'mito.class.table::keys)
    class))

(defmethod c2mop:ensure-class-using-class :around ((class dao-table-class) name &rest keys
                                                   &key direct-superclasses &allow-other-keys)
  (unless (contains-class-or-subclasses 'dao-class direct-superclasses)
    (setf (getf keys :direct-superclasses)
          (cons (find-class 'dao-class) direct-superclasses)))
  (apply #'call-next-method class name keys))

(defgeneric inflate (object slot-name value)
  (:method (object slot-name value)
    (let* ((slot (get-slot-by-slot-name (class-of object) slot-name))
           (inflate (dao-table-column-inflate slot)))
      (if inflate
          (funcall inflate value)
          value)))
  (:method ((object record-timestamps-mixin) (slot-name (eql 'mito.dao.mixin::created-at)) value)
    (local-time:universal-to-timestamp value))
  (:method ((object record-timestamps-mixin) (slot-name (eql 'mito.dao.mixin::updated-at)) value)
    (local-time:universal-to-timestamp value)))

(defgeneric deflate (object slot-name value)
  (:method (object slot-name value)
    (let* ((slot (get-slot-by-slot-name (class-of object) slot-name))
           (deflate (dao-table-column-deflate slot)))
      (if deflate
          (funcall deflate value)
          value)))
  (:method ((object record-timestamps-mixin) (slot-name (eql 'mito.dao.mixin::created-at)) value)
    (local-time:format-timestring nil value
                                  :timezone local-time:+gmt-zone+))
  (:method ((object record-timestamps-mixin) (slot-name (eql 'mito.dao.mixin::updated-at)) value)
    (local-time:format-timestring nil value
                                  :timezone local-time:+gmt-zone+)))

(defun table-definition (class &key if-not-exists)
  (setf class (ensure-class class))
  (check-type class table-class)
  (create-table-sxql class (driver-type)
                     :if-not-exists if-not-exists))
