(in-package :cl-user)
(defpackage mito.dao
  (:use #:cl)
  (:import-from #:mito.dao.table
                #:dao-class
                #:dao-table-class

                #:getoid
                #:dao-synced

                #:inflate
                #:deflate

                #:table-definition)
  (:import-from #:mito.dao.column
                #:dao-table-column-class
                #:dao-table-column-rel-key
                #:dao-table-column-rel-key-fn)
  (:import-from #:mito.class
                #:database-column-slots)
  (:export #:mito.dao.table
           #:dao-class
           #:dao-table-class
           #:dao-table-column-class

           #:getoid
           #:dao-synced

           #:inflate
           #:deflate

           #:table-definition

           #:dao-table-column-rel-key-fn

           #:make-dao-instance))
(in-package :mito.dao)

(defun make-dao-instance (class &rest initargs)
  (when (symbolp class)
    (setf class (find-class class)))

  (assert (and class
               (typep class 'dao-table-class)))

  (let ((obj (make-instance class)))
    ;; Ignore columns which is not defined in defclass as a slot.
    (loop with undef = '#:undef
          for column in (database-column-slots class)
          for column-name = (c2mop:slot-definition-name column)
          for val = (loop for (k v) on initargs by #'cddr
                          when (string= k column-name)
                            do (return v)
                          finally (return undef))
          unless (eq val undef)
            do (setf (slot-value obj column-name)
                     (inflate obj column-name val)))
    (setf (dao-synced obj) t)
    obj))
