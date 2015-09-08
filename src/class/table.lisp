(in-package :cl-user)
(defpackage mito.class.table
  (:use #:cl
        #:mito.util)
  (:import-from #:mito.class.column
                #:table-column
                #:ghost-slot-p)
  (:export #:table-class
           #:table-name
           #:table-indices-info
           #:database-column-slots))
(in-package :mito.class.table)

(defclass table-class (standard-class)
  ((primary-key :initarg :primary-key)
   (unique-keys :initarg :unique-keys)
   (keys :initarg :keys)
   (table-name :initarg :table-name
               :initform nil)))

(defmethod c2mop:direct-slot-definition-class ((class table-class) &key)
  'table-column)

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
      (when (slot-boundp class 'primary-key)
        (let ((primary-keys (first (slot-value class 'primary-key))))
          (list
           (list "PRIMARY"
                 :unique-key t
                 :primary-key t
                 :columns (unlispify-keys primary-keys)))))
      (when (slot-boundp class 'unique-keys)
        (mapcar (lambda (key)
                  (list (format nil "UNIQUE-~A" key)
                        :unique-key t
                        :primary-key nil
                        :column (unlispify-keys key)))
                (slot-value class 'unique-keys)))
      ;; Ignore :keys when using SQLite3
      (when (and (slot-boundp class 'keys)
                 (not (eq driver-type :sqlite3)))
        (mapcar (lambda (key)
                  (list (format nil "KEY-~A" key)
                        :unique-key nil
                        :primary-key nil
                        :column (unlispify-keys key)))
                (slot-value class 'keys))))))
