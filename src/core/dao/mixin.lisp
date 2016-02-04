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

(defclass record-timestamps-mixin ()
  ((created-at :col-type (or :datetime :null)
               :initarg :created-at
               :accessor object-created-at)
   (updated-at :col-type (or :datetime :null)
               :initarg :updated-at
               :accessor object-updated-at))
  (:metaclass dao-table-mixin))
