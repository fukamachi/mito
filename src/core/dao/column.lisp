(in-package :cl-user)
(defpackage mito.dao.column
  (:use #:cl
        #:mito.util)
  (:import-from #:mito.class.column
                #:table-column-class
                #:table-column-type
                #:table-column-info)
  (:export #:dao-table-column-class
           #:dao-table-column-inflate
           #:dao-table-column-deflate
           #:dao-table-column-foreign-class
           #:dao-table-column-rel-column-name
           #:dao-table-column-rel-key
           #:dao-table-column-rel-key-fn))
(in-package :mito.dao.column)

(defclass dao-table-column-class (table-column-class)
  ((inflate :type (or function null)
            :initarg :inflate
            :initform nil
            :reader dao-table-column-inflate)
   (deflate :type (or function null)
            :initarg :deflate
            :initform nil
            :reader dao-table-column-deflate)
   (foreign-class :initarg :foreign-class
                  :initform nil
                  :reader dao-table-column-foreign-class)
   (rel-column-name :initarg :rel-column-name
                    :initform nil
                    :reader dao-table-column-rel-column-name)
   (rel-key :initarg :rel-key
            :initform nil
            :reader dao-table-column-rel-key)
   (rel-key-fn :type (or function null)
               :initarg :rel-key-fn
               :initform nil
               :reader dao-table-column-rel-key-fn)))

(defmethod initialize-instance :around ((object dao-table-column-class) &rest rest-initargs &key name initargs ghost &allow-other-keys)
  (when (and (not ghost)
             (null initargs))
    ;; Add initargs if no ones are defined for a database slot.
    (setf (getf rest-initargs :initargs)
          (list (intern (symbol-name name) :keyword))))

  (apply #'call-next-method object rest-initargs))

(defmethod table-column-info ((column dao-table-column-class) driver-type)
  (if (dao-table-column-rel-key column)
      (let* ((column-info (call-next-method))
             (rel-column-info (table-column-info (dao-table-column-rel-key column) driver-type))
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
