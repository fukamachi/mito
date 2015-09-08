(in-package :cl-user)
(defpackage mito.class.table
  (:use #:cl
        #:mito.util)
  (:import-from #:mito.class.column
                #:table-column-class
                #:table-column-type
                #:primary-key-p
                #:ghost-slot-p)
  (:export #:table-class
           #:table-name
           #:table-primary-key
           #:table-serial-key
           #:table-indices-info
           #:database-column-slots))
(in-package :mito.class.table)

(defclass table-class (standard-class)
  ((primary-key :initarg :primary-key
                :initform nil)
   (unique-keys :initarg :unique-keys
                :initform nil)
   (keys :initarg :keys
         :initform nil)
   (table-name :initarg :table-name
               :initform nil)))

(defmethod reinitialize-instance :around ((class table-class) &rest initargs)
  (unless (getf initargs :primary-key)
    (setf (getf initargs :primary-key) nil))
  (unless (getf initargs :unique-keys)
    (setf (getf initargs :unique-keys) nil))
  (unless (getf initargs :keys)
    (setf (getf initargs :keys) nil))
  (unless (getf initargs :table-name)
    (setf (getf initargs :table-name) nil))
  (apply #'call-next-method class initargs))

(defmethod c2mop:direct-slot-definition-class ((class table-class) &key)
  'table-column-class)

(defmethod c2mop:validate-superclass ((class table-class) (super standard-class))
  t)

(defgeneric table-name (class)
  (:method ((class table-class))
    (if (slot-value class 'table-name)
        (string (car (slot-value class 'table-name)))
        (let ((class-name (lispify (symbol-name-literally (class-name class)))))
          (unlispify
           (if (and (char= (aref class-name 0) #\<)
                    (char= (aref class-name (1- (length class-name))) #\>))
               (subseq class-name 1 (1- (length class-name)))
               class-name))))))

(defgeneric table-primary-key (class)
  (:method ((class table-class))
    (or (slot-value class 'primary-key)
        (let ((primary-slot (find-if
                             #'primary-key-p
                             (database-column-slots class))))
          (if primary-slot
              (list (c2mop:slot-definition-name primary-slot))
              nil)))))

(defgeneric table-serial-key (class)
  (:method ((class table-class))
    (let* ((primary-key (table-primary-key class))
           (slot (find-if
                  (lambda (slot)
                    (and
                     ;; AUTO INCREMENT slot
                     (member (table-column-type slot) '(:serial :bigserial)
                                 :test #'eq)
                     (member (c2mop:slot-definition-name slot)
                             primary-key :test #'eq)))
                  (database-column-slots class))))
      (if slot
          (c2mop:slot-definition-name slot)
          nil))))

(defgeneric database-column-slots (class)
  (:method ((class table-class))
    (remove-if #'ghost-slot-p
               (c2mop:class-direct-slots class))))

(defgeneric table-indices-info (class driver-type)
  (:method (class driver-type)
    (flet ((unlispify-keys (keys)
             (if (listp keys)
                 (mapcar #'unlispify keys)
                 (unlispify keys))))
      (append
       (when (slot-value class 'primary-key)
         (let ((primary-keys (slot-value class 'primary-key)))
           (list
            (list "PRIMARY"
                  :unique-key t
                  :primary-key t
                  :columns (unlispify-keys primary-keys)))))
       (when (slot-value class 'unique-keys)
         (mapcar (lambda (key)
                   (list (format nil "UNIQUE-~A" key)
                         :unique-key t
                         :primary-key nil
                         :columns (unlispify-keys key)))
                 (slot-value class 'unique-keys)))
       ;; Ignore :keys when using SQLite3
       (when (and (slot-value class 'keys)
                  (not (eq driver-type :sqlite3)))
         (mapcar (lambda (key)
                   (list (format nil "KEY-~A" key)
                         :unique-key nil
                         :primary-key nil
                         :columns (unlispify-keys key)))
                 (slot-value class 'keys)))))))
