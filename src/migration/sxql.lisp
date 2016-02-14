(in-package :cl-user)
(defpackage mito.migration.sxql
  (:use #:cl)
  (:import-from #:sxql
                #:yield)
  (:import-from #:sxql.sql-type
                #:sql-clause
                #:expression-clause
                #:sql-statement
                #:name
                #:with-yield-binds)
  (:import-from #:sxql.operator
                #:detect-and-convert)
  (:import-from #:sxql.clause
                #:make-clause)
  (:import-from #:sxql.statement
                #:make-statement)
  (:documentation "Extansions of SxQL for Mito.Migration"))
(in-package :mito.migration.sxql)

(defstruct (set-default (:include expression-clause (name "SET DEFAULT"))
                        (:constructor make-set-default (expression))))
(defmethod make-clause ((clause-name (eql :set-default)) &rest args)
  (make-set-default (detect-and-convert (first args))))

(defstruct (create-sequence (:include sql-statement (name "CREATE SEQUENCE"))
                            (:constructor make-create-sequence (sequence-name)))
  sequence-name)
(defmethod yield ((statement create-sequence))
  (with-yield-binds
    (format nil "CREATE SEQUENCE ~A" (yield (create-sequence-sequence-name statement)))))
(defmethod make-statement ((statement-name (eql :create-sequence)) &rest args)
  (make-create-sequence (detect-and-convert (first args))))

(defstruct (drop-sequence (:include sql-statement (name "DROP SEQUENCE"))
                          (:constructor make-drop-sequence
                              (sequence-name &key if-exists)))
  sequence-name
  if-exists)
(defmethod yield ((statement drop-sequence))
  (with-yield-binds
    (format nil "DROP SEQUENCE~:[~; IF EXISTS~] ~A"
            (drop-sequence-if-exists statement)
            (yield (drop-sequence-sequence-name statement)))))
(defmethod make-statement ((statement-name (eql :drop-sequence)) &rest args)
  (destructuring-bind (sequence-name &key if-exists)
      args
    (make-drop-sequence (detect-and-convert sequence-name)
                        :if-exists if-exists)))
