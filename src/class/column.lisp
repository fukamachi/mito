(in-package :cl-user)
(defpackage mito.class.column
  (:use #:cl
        #:mito.util)
  (:export #:table-column
           #:table-column-type
           #:table-column-name
           #:primary-key-p
           #:ghost-slot-p
           #:table-column-info))
(in-package :mito.class.column)

(defclass table-column (c2mop:standard-direct-slot-definition)
  ((col-type :type (or symbol cons)
             :initarg :col-type
             :initform (error ":col-type is required")
             :accessor table-column-type)
   (primary-key :type boolean
                :initarg :primary-key
                :initform nil
                :accessor primary-key-p)
   (auto-increment :type boolean
                   :initform nil)
   (not-null :type boolean
             :initarg :not-null
             :initform nil)
   (ghost :type boolean
          :initarg :ghost
          :initform nil
          :accessor ghost-slot-p
          :documentation "Option to specify slots as ghost slots. Ghost slots do not depend on a database.")))

(defgeneric table-column-name (column)
  (:method ((column table-column))
    (unlispify (c2mop:slot-definition-name column))))

(defgeneric table-column-info (column driver-type)
  (:method (column (driver-type (eql :sqlite3)))
    (let ((col-type (table-column-type column))
          (auto-increment (slot-value column 'auto-increment))
          (not-null (slot-value column 'not-null)))
      (when (or (eq col-type :serial)
                (eq col-type :bigserial))
        (setf col-type :integer
              auto-increment t
              not-null t))
      (when auto-increment
        (unless (primary-key-p column)
          (warn "SQLite3 supports AUTOINCREMENT for PRIMARY KEYs. Ignoring :auto-increment.")
          (setf auto-increment nil))
        (unless (eq col-type :integer)
          (warn "SQLite3 supports AUTOINCREMENT only for INTEGER columns. Ignoring :auto-increment.")
          (setf auto-increment nil)))

      `(,(string (table-column-name column))
        :type ,col-type
        :auto-increment ,auto-increment
        :primary-key ,(primary-key-p column)
        :not-null ,(or not-null
                       (primary-key-p column)))))
  (:method (column (driver-type (eql :mysql)))
    (let ((col-type (table-column-type column))
          (auto-increment (slot-value column 'auto-increment))
          (not-null (slot-value column 'not-null)))
      (cond
        ((eq col-type :bigserial)
         ;; BIGSERIAL is not allowed in MySQL.
         (setf col-type :serial
               not-null t))
        ((eq col-type :serial)
         (setf not-null t)))
      `(,(string (table-column-name column))
        :type ,col-type
        :auto-increment ,auto-increment
        :primary-key ,(primary-key-p column)
        :not-null ,(or not-null
                       (primary-key-p column)))))
  (:method (column (driver-type (eql :postgres)))
    (let ((col-type (table-column-type column))
          (auto-increment (slot-value column 'auto-increment))
          (not-null (slot-value column 'not-null)))
      (cond
        ((eq col-type :bigserial)
         (setf auto-increment t
               not-null t))
        ((eq col-type :serial)
         (setf auto-increment t
               not-null t)))
      `(,(string (table-column-name column))
        :type ,col-type
        :auto-increment ,auto-increment
        :primary-key ,(primary-key-p column)
        :not-null ,(or not-null
                       (primary-key-p column))))))
