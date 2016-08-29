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
           #:object-updated-at

           #:inflate-timestamp
           #:deflate-timestamp

           #:+sql-datetime-format+))
(in-package :mito.dao.mixin)

(defparameter +sql-datetime-format+
  (append local-time:+iso-8601-date-format+
          (list #\space)
          local-time:+iso-8601-time-format+))

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

(defgeneric inflate-timestamp (value)
  (:method ((value integer))
   (local-time:universal-to-timestamp value))
  (:method ((value string))
   (local-time:parse-timestring value :date-time-separator #\space))
  (:method ((value null))
   nil))

(defgeneric deflate-timestamp (value)
  (:method ((value local-time:timestamp))
   (local-time:format-timestring nil value
                                 :format +sql-datetime-format+
                                 :timezone local-time:+utc-zone+)))

(defclass record-timestamps-mixin ()
  ((created-at :col-type (or :datetime :null)
               :initarg :created-at
               :inflate #'inflate-timestamp
               :deflate #'deflate-timestamp
               :accessor object-created-at)
   (updated-at :col-type (or :datetime :null)
               :initarg :updated-at
               :inflate #'inflate-timestamp
               :deflate #'deflate-timestamp
               :accessor object-updated-at))
  (:metaclass dao-table-mixin))
