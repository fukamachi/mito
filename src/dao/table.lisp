(in-package :cl-user)
(defpackage mito.dao.table
  (:use #:cl
        #:mito.util)
  (:import-from #:mito.connection
                #:driver-type)
  (:import-from #:mito.class
                #:table-class
                #:table-name
                #:table-column-name
                #:table-column-slots
                #:table-primary-key
                #:create-table-sxql)
  (:import-from #:mito.dao.column
                #:dao-table-column-class
                #:dao-table-column-inflate
                #:dao-table-column-deflate)
  (:export #:dao-class
           #:dao-table-class

           #:object-id
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

(defclass dao-class () ())

(defclass dao-table-class (table-class)
  ((auto-pk :initarg :auto-pk
            :initform '(t))))

(defmethod c2mop:direct-slot-definition-class ((class table-class) &key)
  'dao-table-column-class)

(defparameter *oid-slot-definition*
  '(:name id :col-type :bigserial :primary-key t :readers (object-id)))

(declaim (ftype (function (t) *) dao-synced))
(declaim (ftype (function (t t) *) (setf dao-synced)))
(defparameter *synced-slot-definition*
  `(:name %synced :type boolean :initform nil :initfunction ,(lambda () nil) :readers (dao-synced) :writers ((setf dao-synced)) :ghost t))

(defun initargs-enables-auto-pk (initargs)
  (first (or (getf initargs :auto-pk) '(t))))

(defun initargs-contains-primary-key (initargs)
  (or (getf initargs :primary-key)
      (find-if (lambda (slot)
                 (getf slot :primary-key))
               (getf initargs :direct-slots))))

(defun make-dao-instance (class &rest initargs)
  (when (symbolp class)
    (setf class (find-class class)))

  (assert (and class
               (typep class 'dao-table-class)))

  (let* ((obj (make-instance class))
         (obj
           (apply #'make-instance class
                  (loop for (k v) on initargs by #'cddr
                        for column = (find-if (lambda (initargs)
                                                (find k initargs :test #'eq))
                                              (table-column-slots class)
                                              :key #'c2mop:slot-definition-initargs)
                        when column
                          append (list k
                                       (inflate obj (c2mop:slot-definition-name column) v))))))
    (setf (dao-synced obj) t)
    obj))

(defmethod initialize-instance :around ((class dao-table-class) &rest initargs
                                        &key direct-superclasses &allow-other-keys)
  (unless (or (not (initargs-enables-auto-pk initargs))
              (initargs-contains-primary-key initargs))
    (push *oid-slot-definition* (getf initargs :direct-slots)))

  (push *synced-slot-definition* (getf initargs :direct-slots))

  ;; Add relational column slots (ex. user-id)
  (loop for column in (getf initargs :direct-slots)
        for col-type = (getf column :col-type)
        when (and (symbolp col-type)
                  (not (null col-type))
                  (not (keywordp col-type)))
          do (rplacd (cdr column)
                     `(:ghost t ,@(cddr column)))
             (let* ((name (getf column :name))
                    ;; FIXME: find-class returns NIL if the class is this same class
                    (rel-class (find-class (getf column :col-type)))
                    (pk-names (table-primary-key rel-class)))
               (flet ((rel-column-name (pk-name)
                        (intern
                         (format nil "~A-~A" (getf column :col-type) pk-name)
                         (symbol-package name))))
                 (dolist (pk-name pk-names)
                   (let ((rel-column-name (rel-column-name pk-name))
                         (pk (get-slot-by-slot-name rel-class pk-name)))
                     (rplacd (last (getf initargs :direct-slots))
                             `((:name ,rel-column-name
                                :initargs (,(intern (symbol-name rel-column-name) :keyword))
                                ;; Defer retrieving the relational column type until table-column-info
                                :col-type nil
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

  (unless (contains-class-or-subclasses 'dao-class direct-superclasses)
    (setf (getf initargs :direct-superclasses)
          (cons (find-class 'dao-class) direct-superclasses)))
  (apply #'call-next-method class initargs))

(defmethod reinitialize-instance :around ((class dao-table-class) &rest initargs)
  (if (or (not (initargs-enables-auto-pk initargs))
          (initargs-contains-primary-key initargs))
      (setf (getf initargs :direct-slots)
            (remove 'id (getf initargs :direct-slots)
                    :key #'car
                    :test #'eq))
      (push *oid-slot-definition* (getf initargs :direct-slots)))

  (push *synced-slot-definition* (getf initargs :direct-slots))

  (apply #'call-next-method class initargs))

(defmethod c2mop:ensure-class-using-class :around ((class dao-table-class) name &rest keys
                                                   &key direct-superclasses &allow-other-keys)
  (unless (contains-class-or-subclasses 'dao-class direct-superclasses)
    (setf (getf keys :direct-superclasses)
          (cons (find-class 'dao-class) direct-superclasses)))
  (apply #'call-next-method class name keys))

(defgeneric inflate (object slot-name value)
  (:method ((object dao-class) slot-name value)
    (let* ((slot (get-slot-by-slot-name (class-of object) slot-name))
           (inflate (dao-table-column-inflate slot)))
      (if inflate
          (funcall inflate value)
          value))))

(defgeneric deflate (object slot-name value)
  (:method ((object dao-class) slot-name value)
    (let* ((slot (get-slot-by-slot-name (class-of object) slot-name))
           (deflate (dao-table-column-deflate slot)))
      (if deflate
          (funcall deflate value)
          value))))

(defun table-definition (class &key if-not-exists)
  (when (symbolp class)
    (setf class (find-class class)))
  (check-type class table-class)
  (create-table-sxql class (driver-type)
                     :if-not-exists if-not-exists))
