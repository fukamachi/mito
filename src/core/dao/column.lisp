(in-package :cl-user)
(defpackage mito.dao.column
  (:use #:cl
        #:mito.util)
  (:import-from #:mito.class.column
                #:table-column-class
                #:table-column-type
                #:table-column-info)
  (:import-from #:mito.class
                #:table-primary-key
                #:find-slot-by-name)
  (:import-from #:alexandria
                #:ensure-car
                #:ensure-list
                #:when-let)
  (:export #:dao-table-column-class
           #:dao-table-column-inflate
           #:dao-table-column-deflate
           #:dao-table-column-foreign-class
           #:dao-table-column-foreign-slot))
(in-package :mito.dao.column)

(deftype references ()
  '(or null symbol (cons symbol (or null (cons symbol null)))))

(defclass dao-table-column-class (table-column-class)
  ((inflate :type (or function null)
            :initarg :inflate
            :initform nil
            :reader dao-table-column-inflate)
   (deflate :type (or function null)
            :initarg :deflate
            :initform nil
            :reader dao-table-column-deflate)
   (references :type references
               :initarg :references
               :initform nil
               :reader dao-table-column-references)))

(defgeneric dao-table-column-foreign-class (column)
  (:method ((column dao-table-column-class))
    (when-let (foreign-class-name (ensure-car (dao-table-column-references column)))
      (find-class foreign-class-name))))

(defgeneric dao-table-column-foreign-slot (column)
  (:method ((column dao-table-column-class))
    (when-let (foreign-class (dao-table-column-foreign-class column))
      (let ((foreign-slot-name (second (ensure-list (dao-table-column-references column)))))
        (unless foreign-slot-name
          (let ((pk-names (table-primary-key foreign-class)))
            (unless pk-names
              (error "Foreign class ~S has no primary keys and no slot name is specified to :references"
                     (class-name foreign-class)))
            (when (cdr pk-names)
              (error "Foreign class ~S has a composite primary key and failed to detect which to use for :references"
                     (class-name foreign-class)))
            (setf foreign-slot-name (first pk-names))))
        (or (find-slot-by-name foreign-class foreign-slot-name
                               :test #'string=)
            (error "No slot named ~S in foreign class ~S"
                   foreign-slot-name
                   (class-name foreign-class)))))))

(defmethod initialize-instance :around ((object dao-table-column-class) &rest rest-initargs
                                        &key name initargs ghost
                                        &allow-other-keys)
  (when (and (not ghost)
             (not (find (symbol-name name) initargs :test #'string=)))
    ;; Add the default initarg.
    (push (intern (symbol-name name) :keyword)
          (getf rest-initargs :initargs)))

  (apply #'call-next-method object rest-initargs))

(defmethod table-column-info ((column dao-table-column-class) driver-type)
  (if (dao-table-column-foreign-slot column)
      (let* ((column-info (call-next-method))
             (rel-column-info (table-column-info (dao-table-column-foreign-slot column) driver-type))
             (new-col-type (getf (cdr rel-column-info) :type)))
        (setf (getf (cdr column-info) :type)
              (ecase driver-type
                (:mysql
                 (case new-col-type
                   (:bigserial :bigint)
                   (:serial '(:int () :unsigned))
                   (otherwise new-col-type)))
                (:postgres
                 (case new-col-type
                   (:bigserial :bigint)
                   (:serial :int)
                   (otherwise new-col-type)))
                (:sqlite3
                 (case new-col-type
                   ((:bigserial :serial) :integer)
                   (otherwise new-col-type)))))
        (setf (getf (cdr column-info) :not-null)
              (optima:match (table-column-type column)
                ((or (list 'or :null _)
                     (list 'or _ :null))
                 nil)
                (otherwise t)))
        column-info)
      (call-next-method)))
