(in-package :cl-user)
(defpackage mito.dao.table
  (:use #:cl
        #:mito.util
        #:mito.error)
  (:import-from #:mito.dao.view
                #:table-definition)
  (:import-from #:mito.connection
                #:driver-type)
  (:import-from #:mito.class
                #:table-class
                #:table-column-type
                #:table-column-slots
                #:table-primary-key
                #:create-table-sxql)
  (:import-from #:mito.dao.column
                #:inflate
                #:inflate-if-bound
                #:deflate-if-bound
                #:deflate
                #:dao-table-column-class
                #:dao-table-column-standard-effective-slot-definitions
                #:dao-table-column-inflate)
  (:import-from #:mito.dao.mixin
                #:dao-table-mixin
                #:dao-class
                #:serial-pk-mixin
                #:uuid-pk-mixin
                #:record-timestamps-mixin
                #:add-relational-readers)
  (:export #:dao-table-class
           #:depending-table-classes))
(in-package :mito.dao.table)

(defclass dao-table-class (dao-table-mixin)
  ((auto-pk :initarg :auto-pk
            :initform '(:serial))
   (record-timestamps :initarg :record-timestamps
                      :initform '(t))))

(defmethod c2mop:direct-slot-definition-class ((class dao-table-class) &key)
  (find-class 'dao-table-column-class))

(defmethod c2mop:effective-slot-definition-class ((class dao-table-class) &rest initargs)
  (declare (ignorable initargs))
  (find-class 'dao-table-column-standard-effective-slot-definitions))

(defmethod c2mop:validate-superclass ((class dao-table-class) (super standard-class))
  t)

(defmethod c2mop:compute-effective-slot-definition
    :around ((class dao-table-class) name direct-slot-definitions)
  (declare (ignorable name))
  (let ((result (call-next-method)))
    (when result
      ;; set here all the relevant slots.
      ;; set here inflate and deflate from dao-table-column-standard-effective-slot-definitions
      (setf (inflate result)
            (some #'inflate-if-bound direct-slot-definitions))
      (setf (deflate result)
            (some #'deflate-if-bound direct-slot-definitions))
      result)))

(defun initargs-enables-auto-pk (initargs)
  (first (or (getf initargs :auto-pk) '(:serial))))

(defun initargs-enables-record-timestamps (initargs)
  (first (or (getf initargs :record-timestamps) '(t))))

(defun initargs-contains-primary-key (initargs)
  (or (getf initargs :primary-key)
      (find-if (lambda (slot)
                 (getf slot :primary-key))
               (getf initargs :direct-slots))))

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

(defun append-record-timestamp-mixin-to-direct-superclasses-if-needed (initargs direct-superclasses)
  (when (and (initargs-enables-record-timestamps initargs)
             (not (contains-class-or-subclasses 'record-timestamps-mixin direct-superclasses)))
    (setf (getf initargs :direct-superclasses)
          (append (getf initargs :direct-superclasses)
                  (list (find-class 'record-timestamps-mixin))))))

(defun append-auto-pk-class-to-direct-superclasses-if-needed (initargs direct-superclasses)
  (let ((auto-pk-type (initargs-enables-auto-pk initargs)))
    (when auto-pk-type
      (let ((auto-pk-class (ecase auto-pk-type
                             (:serial 'serial-pk-mixin)
                             (:uuid 'uuid-pk-mixin)
                             ('t 'serial-pk-mixin))))
        (when (and (not (initargs-contains-primary-key initargs))
                   (not (contains-class-or-subclasses auto-pk-class direct-superclasses))
                   (not (mapcan #'table-primary-key
                                (remove-if-not (lambda (c)
                                                 (typep c 'table-class))
                                               direct-superclasses))))
          (push (find-class auto-pk-class) (getf initargs :direct-superclasses)))))))

(defmethod initialize-instance :around ((class dao-table-class) &rest initargs
                                                                &key direct-superclasses &allow-other-keys)
  (append-record-timestamp-mixin-to-direct-superclasses-if-needed initargs direct-superclasses)
  (unless (contains-class-or-subclasses 'dao-class direct-superclasses)
    (push (find-class 'dao-class) (getf initargs :direct-superclasses)))
  (append-auto-pk-class-to-direct-superclasses-if-needed initargs direct-superclasses)
  (apply #'call-next-method class initargs))

(defmethod reinitialize-instance :around ((class dao-table-class) &rest initargs
                                          &key direct-superclasses &allow-other-keys)
  (append-record-timestamp-mixin-to-direct-superclasses-if-needed initargs direct-superclasses)
  (append-auto-pk-class-to-direct-superclasses-if-needed initargs direct-superclasses)
  (apply #'call-next-method class initargs))

(defmethod c2mop:ensure-class-using-class :around ((class dao-table-class) name &rest keys
                                                   &key direct-superclasses &allow-other-keys)
  (unless (contains-class-or-subclasses 'dao-class direct-superclasses)
    (setf (getf keys :direct-superclasses)
          (cons (find-class 'dao-class) direct-superclasses)))
  (apply #'call-next-method class name keys))

(defmethod table-definition ((class dao-table-class) &key if-not-exists &allow-other-keys)
  (create-table-sxql class (driver-type)
                     :if-not-exists if-not-exists))
