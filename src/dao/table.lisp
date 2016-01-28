(in-package :cl-user)
(defpackage mito.dao.table
  (:use #:cl
        #:mito.util)
  (:import-from #:mito.connection
                #:driver-type)
  (:import-from #:mito.class
                #:table-class
                #:table-column-class
                #:table-column-type
                #:database-column-slots
                #:create-table-sxql)
  (:import-from #:mito.dao.column
                #:dao-table-column-class
                #:dao-table-column-inflate
                #:dao-table-column-deflate
                #:relational-column-type-p)
  (:export #:dao-class
           #:dao-table-class

           #:getoid
           #:dao-synced

           #:inflate
           #:deflate

           #:table-definition))
(in-package :mito.dao.table)

(defclass dao-class () ())

(defclass dao-table-class (table-class)
  ((auto-pk :initarg :auto-pk
            :initform '(t))))

(defmethod c2mop:direct-slot-definition-class ((class table-class) &key)
  'dao-table-column-class)

(defparameter *oid-slot-definition*
  '(:name %oid :col-type :bigserial :primary-key t :readers (getoid)))

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

(defmethod initialize-instance :around ((class dao-table-class) &rest initargs
                                        &key direct-superclasses &allow-other-keys)
  (unless (or (not (initargs-enables-auto-pk initargs))
              (initargs-contains-primary-key initargs))
    (push *oid-slot-definition* (getf initargs :direct-slots)))

  (push *synced-slot-definition* (getf initargs :direct-slots))

  (loop for slot in (getf initargs :direct-slots)
        when (relational-column-type-p (getf slot :col-type))
          do (push `(:name ,(intern
                             (format nil "~A-~A"
                                     (getf slot :col-type)
                                     (first (mito.class:table-primary-key (find-class (getf slot :col-type)))))
                             (symbol-package (getf slot :name)))
                     :ghost t)
                   (getf initargs :direct-slots)))

  (unless (contains-class-or-subclasses 'dao-class direct-superclasses)
    (setf (getf initargs :direct-superclasses)
          (cons (find-class 'dao-class) direct-superclasses)))
  (apply #'call-next-method class initargs))

(defmethod reinitialize-instance :around ((class dao-table-class) &rest initargs)
  (if (or (not (initargs-enables-auto-pk initargs))
          (initargs-contains-primary-key initargs))
      (setf (getf initargs :direct-slots)
            (remove '%oid (getf initargs :direct-slots)
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

(defgeneric relational-column-slots (class)
  (:method ((class dao-table-class))
    (remove-if-not #'relational-column-type-p
                   (database-column-slots class)
                   :key #'table-column-type)))
