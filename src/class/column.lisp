(in-package :cl-user)
(defpackage mito.class.column
  (:use #:cl
        #:mito.util
        #:mito.error)
  (:import-from #:alexandria
                #:delete-from-plist)
  (:export #:table-column-class
           #:table-column-type
           #:table-column-name
           #:primary-key-p
           #:ghost-slot-p
           #:table-column-info
           #:table-column-info-for-create-table))
(in-package :mito.class.column)

(defclass table-column-class (c2mop:standard-direct-slot-definition)
  ((col-type :type (or symbol cons null)
             :initarg :col-type
             :initform nil
             :accessor table-column-type)
   (primary-key :type boolean
                :initarg :primary-key
                :initform nil
                :accessor primary-key-p)
   (ghost :type boolean
          :initarg :ghost
          :initform nil
          :accessor ghost-slot-p
          :documentation "Option to specify slots as ghost slots. Ghost slots do not depend on a database.")))

(defmethod initialize-instance :after ((class table-column-class) &rest initargs)
  (declare (ignore initargs))
  (when (and (not (slot-boundp class 'col-type))
             (not (ghost-slot-p class)))
    (error 'col-type-required
           :slot class)))

(defun parse-col-type (col-type)
  (optima:match col-type
    ((or (list 'or :null x)
         (list 'or x :null))
     (values x nil))
    (otherwise
     (values col-type t))))

(defgeneric table-column-name (column)
  (:method ((column table-column-class))
    (unlispify (symbol-name-literally (c2mop:slot-definition-name column)))))

(defgeneric table-column-info (column driver-type)
  (:method (column (driver-type (eql :sqlite3)))
    (let (auto-increment)
      (multiple-value-bind (col-type not-null)
          (parse-col-type (table-column-type column))
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

        `(,(table-column-name column)
          :type ,col-type
          :auto-increment ,auto-increment
          :primary-key ,(primary-key-p column)
          :not-null ,(or not-null
                         (primary-key-p column))))))
  (:method (column (driver-type (eql :mysql)))
    (let (auto-increment)
      (multiple-value-bind (col-type not-null)
          (parse-col-type (table-column-type column))
        (cond
          ((eq col-type :bigserial)
           (setf col-type '(:bigint () :unsigned)
                 auto-increment t
                 not-null t))
          ((eq col-type :serial)
           (setf col-type '(:int () :unsigned)
                 auto-increment t
                 not-null t)))
        `(,(table-column-name column)
          :type ,col-type
          :auto-increment ,auto-increment
          :primary-key ,(primary-key-p column)
          :not-null ,(or not-null
                         (primary-key-p column))))))
  (:method (column (driver-type (eql :postgres)))
    (let (auto-increment)
      (multiple-value-bind (col-type not-null)
          (parse-col-type (table-column-type column))
        (cond
          ((eq col-type :bigserial)
           (setf auto-increment t
                 not-null t))
          ((eq col-type :serial)
           (setf auto-increment t
                 not-null t)))
        `(,(table-column-name column)
          :type ,col-type
          :auto-increment ,auto-increment
          :primary-key ,(primary-key-p column)
          :not-null ,(or not-null
                         (primary-key-p column)))))))

(defgeneric table-column-info-for-create-table (column driver-type)
  (:documentation "Similar to table-column-info except the return value is for sxql:make-create-table.")
  (:method (column driver-type)
    (table-column-info column driver-type))
  (:method :around (column driver-type)
    (let ((column-info (call-next-method)))
      (rplaca column-info
              (sxql:make-sql-symbol (car column-info)))
      column-info))
  (:method (column (driver-type (eql :mysql)))
    (let ((column-info (table-column-info column driver-type)))
      (when (and (getf (cdr column-info) :auto-increment)
                 (member (getf (cdr column-info) :type) '(:serial :bigserial) :test #'eq))
        (setf (getf (cdr column-info) :auto-increment) nil))
      column-info))
  (:method (column (driver-type (eql :sqlite3)))
    (let ((column-info (table-column-info column driver-type)))
      (when (getf (cdr column-info) :auto-increment)
        (rplaca (member :auto-increment (cdr column-info) :test #'eq)
                :autoincrement)
        ;; NOT NULL cannot be specified for an AUTOINCREMENT column
        (setf (cdr column-info) (delete-from-plist (cdr column-info) :not-null)))
      column-info))
  (:method (column (driver-type (eql :postgres)))
    (let ((column-info (table-column-info column driver-type)))
      (setf (getf (cdr column-info) :auto-increment) nil)
      column-info)))
