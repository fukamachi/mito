(in-package :cl-user)
(defpackage mito.dao
  (:use #:cl)
  (:import-from #:mito.class
                #:table-class)
  (:export #:dao-class
           #:dao-table-class

           #:getoid))
(in-package :mito.dao)

(defclass dao-class () ())

(defclass dao-table-class (table-class)
  ((auto-pk :initarg :auto-pk
            :initform '(t))))

(defparameter *oid-slot-definition*
  '(:name %oid :col-type :bigserial :primary-key t :readers (getoid)))

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

  (apply #'call-next-method class initargs))

(defmethod c2mop:ensure-class-using-class :around ((class dao-table-class) name &rest keys
                                                   &key direct-superclasses &allow-other-keys)
  (unless (contains-class-or-subclasses 'dao-class direct-superclasses)
    (setf (getf keys :direct-superclasses)
          (cons (find-class 'dao-class) direct-superclasses)))
  (apply #'call-next-method class name keys))
