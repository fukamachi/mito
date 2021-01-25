(in-package :cl-user)
(defpackage mito.dao.mixin
  (:use #:cl)
  (:import-from #:mito.class
                #:table-class)
  (:import-from #:mito.dao.column
                #:dao-table-column-class)
  (:import-from #:uuid
                #:make-v4-uuid)
  (:export #:dao-table-mixin

           #:serial-pk-mixin
           #:uuid-pk-mixin
           #:record-timestamps-mixin

           #:object-id
           #:object=
           #:object-created-at
           #:object-updated-at))
(in-package :mito.dao.mixin)

(defclass dao-table-mixin (table-class) ())

(defmethod c2mop:direct-slot-definition-class ((class dao-table-mixin) &key)
  'dao-table-column-class)

(defclass serial-pk-mixin ()
  ((id :col-type :bigserial
       :initarg :id
       :primary-key t
       :accessor %object-id))
  (:metaclass dao-table-mixin))

(defun generate-uuid ()
  (string-downcase (print-object (uuid:make-v4-uuid) nil)))

(defclass uuid-pk-mixin ()
  ((id :col-type (:varchar 36)
       :initform (generate-uuid)
       :accessor %object-uuid
       :primary-key t))
  (:metaclass dao-table-mixin))

(defgeneric object-id (object)
  (:method ((object serial-pk-mixin))
    (if (slot-boundp object 'id)
        (%object-id object)
        nil))
  (:method ((object uuid-pk-mixin))
    (if (slot-boundp object 'id)
        (%object-uuid object)
        nil)))

(defgeneric (setf object-id) (id object)
  (:method (id (object serial-pk-mixin))
    (setf (%object-id object) id))
  (:method (id (object uuid-pk-mixin))
    (setf (%object-uuid object) id)))

(defgeneric object= (object1 object2)
  (:method (object1 object2)
    (and (eq (class-of object1) (class-of object2))
         (eql (object-id object1) (object-id object2)))))

(defclass record-timestamps-mixin ()
  ((created-at :col-type (or :timestamptz :null)
               :initarg :created-at
               :accessor object-created-at)
   (updated-at :col-type (or :timestamptz :null)
               :initarg :updated-at
               :accessor object-updated-at))
  (:metaclass dao-table-mixin))
