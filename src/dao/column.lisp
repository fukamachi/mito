(in-package :cl-user)
(defpackage mito.dao.column
  (:use #:cl
        #:mito.util)
  (:import-from #:mito.class.column
                #:table-column-class
                #:table-column-name
                #:table-column-info
                #:table-column-type)
  (:import-from #:mito.class
                #:table-primary-key
                #:database-column-slots)
  (:export #:dao-table-column-class
           #:dao-table-column-inflate
           #:dao-table-column-deflate
           #:relational-column-type-p
           #:relational-column-type
           #:relational-column-name))
(in-package :mito.dao.column)

(defclass dao-table-column-class (table-column-class)
  ((inflate :type (or function null)
            :initarg :inflate
            :initform nil
            :reader dao-table-column-inflate)
   (deflate :type (or function null)
            :initarg :deflate
            :initform nil
            :reader dao-table-column-deflate)))

(defmethod table-column-info ((column dao-table-column-class) driver-type)
  (let ((col-type (table-column-type column)))
    (if (relational-column-type-p col-type)
        (let ((column-info (call-next-method)))
          (setf (getf (cdr column-info) :type)
                (relational-column-type column driver-type))
          (list*
           (relational-column-name column)
           (cdr column-info)))
        (call-next-method))))

(defun relational-column-type-p (col-type)
  (and (symbolp col-type)
       (not (null col-type))
       (not (keywordp col-type))))

(defun relational-column-name (column)
  (let* ((col-type (table-column-type column))
         (rel-class (find-class col-type))
         (rel-pk (table-primary-key rel-class)))
    (when (cdr rel-pk)
      (error "Cannot make a relational column to the class which has multiple primary keys."))
    (let* ((rel-pk (get-slot-by-slot-name rel-class (first rel-pk)))
           (rel-column-name (table-column-name rel-pk)))
      (unlispify
       (format nil "~A-~A" (symbol-name-literally (class-name rel-class))
               rel-column-name)))))

(defun relational-column-type (column driver-type)
  (let* ((col-type (table-column-type column))
         (rel-class (find-class col-type))
         (rel-pk (table-primary-key rel-class)))
    (when (cdr rel-pk)
      (error "Cannot make a relational column to the class which has multiple primary keys."))
    (let* ((rel-pk (get-slot-by-slot-name rel-class (first rel-pk)))
           (rel-column-info (table-column-info rel-pk driver-type)))
      (getf (cdr rel-column-info) :type))))
