(in-package #:cl-user)
(defpackage #:mito.dao.view
  (:use #:cl)
  (:import-from #:mito.class
                #:table-class
                #:table-name)
  (:import-from #:mito.dao.column
                #:dao-table-column-class)
  (:import-from #:sxql)
  (:export #:dao-table-view
           #:dao-table-view-as-query
           #:table-definition))
(in-package #:mito.dao.view)

(defclass dao-table-view (table-class)
  ((as :initarg :as
       :initform (error ":as query is required for dao-table-view")
       :reader dao-table-view-as-query)))

(defmethod c2mop:direct-slot-definition-class ((class dao-table-view) &key)
  'dao-table-column-class)

(defstruct (create-view (:include sxql.sql-type:sql-statement (sxql.sql-type:name "CREATE VIEW"))
                        (:constructor make-create-view (view-name &key or-replace as)))
  view-name
  or-replace
  as)

(defmethod sxql:make-statement ((statement-name (eql :create-view)) &rest args)
  (destructuring-bind (view-name &key or-replace as)
      args
    (make-create-view (sxql.operator:detect-and-convert view-name) :or-replace or-replace :as as)))

(defmethod sxql:yield ((statement create-view))
  (sxql.sql-type:with-yield-binds
    (format nil "CREATE~:[~; OR REPLACE~] VIEW ~A AS ~A"
            (create-view-or-replace statement)
            (sxql:yield (create-view-view-name statement))
            (create-view-as statement))))

(defgeneric table-definition (class &key if-not-exists or-replace)
  (:method ((class symbol) &rest args &key if-not-exists or-replace)
    (declare (ignore if-not-exists or-replace))
    (apply #'table-definition (find-class class) args))
  (:method ((class dao-table-view) &key or-replace &allow-other-keys)
    (list
     (sxql:make-statement :create-view
                          (sxql:make-sql-symbol (table-name class))
                          :or-replace or-replace
                          :as (first (dao-table-view-as-query class))))))
