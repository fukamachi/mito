(in-package :cl-user)
(defpackage mito.dao
  (:use #:cl)
  (:import-from #:mito.connection
                #:driver-type)
  (:import-from #:mito.class
                #:table-class
                #:table-column-class
                #:create-table-sxql)
  (:export #:dao-class
           #:dao-table-class

           #:getoid
           #:dao-synced

           #:inflate
           #:deflate

           #:table-definition))
(in-package :mito.dao)

(defclass dao-class () ())

(defclass dao-table-class (table-class)
  ((auto-pk :initarg :auto-pk
            :initform '(t))))

(defclass dao-table-column-class (table-column-class)
  ((inflate :type (or function null)
            :initarg :inflate
            :initform nil
            :reader dao-table-column-inflate)
   (deflate :type (or function null)
            :initarg :deflate
            :initform nil
            :reader dao-table-column-deflate)))

(defmethod c2mop:direct-slot-definition-class ((class table-class) &key)
  'dao-table-column-class)

(defparameter *oid-slot-definition*
  '(:name %oid :col-type :bigserial :primary-key t :readers (getoid)))

(declaim (ftype (function (t) *) dao-synced))
(declaim (ftype (function (t t) *) (setf dao-synced)))
(defparameter *synced-slot-definition*
  `(:name %synced :type boolean :initform nil :initfunction ,(lambda () nil) :readers (dao-synced) :writers ((setf dao-synced)) :ghost t))

(defun class-inherit-p (target parent)
  (not (null
        (member parent
                (c2mop:class-direct-superclasses target)
                :test #'eq))))

(defun contains-class-or-subclasses (class target-classes)
  (let ((class (if (typep class 'class)
                   class
                   (find-class class))))
    (find-if (lambda (target-class)
               (let ((target-class (if (typep target-class 'class)
                                       target-class
                                       (find-class target-class nil))))
                 (and target-class
                      (or (eq target-class class)
                          (class-inherit-p target-class class)))))
             target-classes)))

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

(defun get-slot-by-slot-name (obj slot-name)
  (find slot-name
        (c2mop:class-direct-slots (if (typep obj 'symbol)
                                      (find-class obj)
                                      (class-of obj)))
        :test #'eq
        :key #'c2mop:slot-definition-name))

(defgeneric inflate (object slot-name value)
  (:method ((object dao-class) slot-name value)
    (let* ((slot (get-slot-by-slot-name object slot-name))
           (inflate (dao-table-column-inflate slot)))
      (if inflate
          (funcall inflate value)
          value))))

(defgeneric deflate (object slot-name value)
  (:method ((object dao-class) slot-name value)
    (let* ((slot (get-slot-by-slot-name object slot-name))
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
