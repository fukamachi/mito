(in-package :cl-user)
(defpackage mito.dao.mixin
  (:use #:cl)
  (:import-from #:mito.class
                #:table-class)
  (:import-from #:mito.dao.column
                #:dao-table-column-class)
  (:export #:dao-table-mixin

           #:auto-pk-mixin
           #:record-timestamps-mixin

           #:object-id
           #:object=
           #:object-created-at
           #:object-updated-at))
(in-package :mito.dao.mixin)

(defclass dao-table-mixin (table-class) ())

(defmethod c2mop:direct-slot-definition-class ((class dao-table-mixin) &key)
  'dao-table-column-class)

(defclass auto-pk-mixin ()
  ((id :col-type :bigserial
       :initarg :id
       :primary-key t
       :reader object-id))
  (:metaclass dao-table-mixin))

(defgeneric object= (object1 object2)
  (:method ((object1 auto-pk-mixin) (object2 auto-pk-mixin))
    (and (eq (class-of object1) (class-of object2))
         (eql (object-id object1) (object-id object2)))))

(defclass record-timestamps-mixin ()
  ((created-at :col-type (or :timestamp :null)
               :initarg :created-at
               :inflate (lambda (value)
                          (etypecase value
                            (integer
                             (local-time:universal-to-timestamp value))
                            (string
                             (local-time:parse-timestring value :date-time-separator #\Space))
                            (null nil)))
               :accessor object-created-at)
   (updated-at :col-type (or :timestamp :null)
               :initarg :updated-at
               :inflate (lambda (value)
                          (etypecase value
                            (integer
                             (local-time:universal-to-timestamp value))
                            (string
                             (local-time:parse-timestring value :date-time-separator #\Space))
                            (null nil)))
               :accessor object-updated-at))
  (:metaclass dao-table-mixin))
