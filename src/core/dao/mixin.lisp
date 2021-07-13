(in-package :cl-user)
(defpackage mito.dao.mixin
  (:use #:cl
        #:mito.util)
  (:import-from #:mito.class.column
                #:table-column-type)
  (:import-from #:mito.class.table
                #:table-direct-column-slots)
  (:import-from #:mito.class
                #:table-class
                #:table-name
                #:table-column-slots
                #:find-child-columns
                #:find-slot-by-name
                #:table-column-name
                #:table-column-references-column)
  (:import-from #:mito.dao.column
                #:dao-table-column-class
                #:dao-table-column-inflate
                #:*conc-name*)
  (:import-from #:uuid
                #:make-v4-uuid)
  (:import-from #:sxql)
  (:export #:dao-table-mixin
           #:dao-class
           #:dao-synced
           #:make-dao-instance

           #:serial-pk-mixin
           #:uuid-pk-mixin
           #:record-timestamps-mixin

           #:object-id
           #:object=
           #:object-created-at
           #:object-updated-at))
(in-package :mito.dao.mixin)

(defclass dao-class ()
  ((synced :type boolean
           :initform nil
           :accessor dao-synced)))

(defclass dao-table-mixin (table-class) ())

(defmethod c2mop:direct-slot-definition-class ((class dao-table-mixin) &key)
  'dao-table-column-class)

(defgeneric make-dao-instance (class &rest initargs)
  (:method ((class-name symbol) &rest initargs)
    (apply #'make-dao-instance
           (find-class class-name)
           initargs))
  
  (:method ((class table-class) &rest initargs)
    (let* ((list (loop for (k v) on initargs by #'cddr
                       for column = (find-if (lambda (initargs)
                                               (find k initargs :test #'eq))
                                             (table-column-slots class)
                                             :key #'c2mop:slot-definition-initargs)
                       if column
                         append (list k
                                      (dao-table-column-inflate column v))
                       else
                         append (list k v)))
           (obj (allocate-instance class))
           (obj (apply #'shared-initialize obj nil list)))
      (setf (dao-synced obj) t)
      obj)))

(defun make-relational-reader-method (func-name class slot-name rel-class)
  (let ((generic-function
          (ensure-generic-function func-name :lambda-list '(object))))
    (add-method
     generic-function
     (make-instance 'standard-method
                    :lambda-list '(object)
                    :qualifiers ()
                    :specializers (list class)
                    :function
                    (let ((calledp nil))
                      (lambda (object &rest ignore)
                        (declare (ignore ignore))
                        ;; I don't know why but SBCL pass a CONS of the instance instead of the instance itself.
                        (when (consp object)
                          (setf object (first object)))
                        (if (and (slot-boundp object slot-name)
                                 (or calledp
                                     (not (null (slot-value object slot-name)))))
                            (slot-value object slot-name)
                            (let* ((child-columns (find-child-columns class
                                                                      (find-slot-by-name class slot-name)))
                                   (foreign-object
                                     (and (every (lambda (slot-name)
                                                   (and (slot-boundp object slot-name)
                                                        (slot-value object slot-name)))
                                                 child-columns)
                                          (let ((result
                                                  (first
                                                   (mito.db:retrieve-by-sql
                                                    (sxql:select :*
                                                      (sxql:from (sxql:make-sql-symbol (table-name rel-class)))
                                                      (sxql:where
                                                       `(:and
                                                         ,@(mapcar (lambda (slot-name)
                                                                     `(:= ,(sxql:make-sql-symbol
                                                                            (table-column-name
                                                                             (table-column-references-column
                                                                              (find-slot-by-name class slot-name))))
                                                                          ,(slot-value object slot-name)))
                                                                   child-columns)))
                                                      (sxql:limit 1))))))
                                            (and result
                                                 (apply #'make-dao-instance rel-class result))))))
                              (setf calledp t
                                    (slot-value object slot-name) foreign-object)))))))))

(defun add-relational-readers (class initargs)
  (loop for column in (table-direct-column-slots class)
        for col-type = (table-column-type column)
        when (and (symbolp col-type)
                  (not (null col-type))
                  (not (keywordp col-type)))
        do (let* ((name (c2mop:slot-definition-name column))
                  ;; FIXME: find-class returns NIL if the class is this same class
                  (rel-class (find-class col-type)))
             (dolist (reader (c2mop:slot-definition-readers column))
               (make-relational-reader-method reader class name rel-class)))))

(defmethod initialize-instance :around ((class dao-table-mixin) &rest initargs
                                        &key conc-name &allow-other-keys)
  (let ((*conc-name* (first conc-name)))
    (let ((class (apply #'call-next-method class initargs)))
      (add-relational-readers class initargs)
      class)))

(defmethod reinitialize-instance :around ((class dao-table-mixin) &rest initargs
                                          &key conc-name &allow-other-keys)
  (let ((*conc-name* (first conc-name)))
    (let ((class (apply #'call-next-method class initargs)))
      (add-relational-readers class initargs)
      class)))

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
