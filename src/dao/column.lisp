(in-package :cl-user)
(defpackage mito.dao.column
  (:use #:cl
        #:mito.util)
  (:import-from #:mito.class.column
                #:table-column-class
                #:table-column-info)
  (:export #:dao-table-column-class
           #:dao-table-column-inflate
           #:dao-table-column-deflate
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
   (rel-key :initarg :rel-key
            :initform nil
            :reader dao-table-column-rel-key)
   (rel-key-fn :type (or function null)
               :initarg :rel-key-fn
               :initform nil
               :reader dao-table-column-rel-key-fn)))

(defmethod table-column-info ((column dao-table-column-class) driver-type)
  (if (dao-table-column-rel-key column)
      (let ((column-info (call-next-method))
            (rel-column-info (table-column-info (dao-table-column-rel-key column) driver-type)))
        (setf (getf (cdr column-info) :type)
              (getf (cdr rel-column-info) :type))
        column-info)
      (call-next-method)))
