(in-package :cl-user)
(defpackage mito.class.column
  (:use #:cl
        #:mito.util
        #:mito.error)
  (:import-from #:alexandria
                #:delete-from-plist
                #:ensure-car)
  (:export #:table-column-class
           #:table-column-type
           #:table-column-not-null-p
           #:table-column-name
           #:primary-key-p
           #:ghost-slot-p
           #:table-column-references
           #:table-column-references-column
           #:table-column-info
           #:table-column-info-for-create-table))
(in-package :mito.class.column)

(deftype references ()
  '(or null symbol (cons symbol (or null (cons symbol null)))))

(defun parse-col-type (col-type)
  (trivia:match col-type
    ((or (list 'or :null x)
         (list 'or x :null))
     (values x nil))
    (otherwise
     (values col-type t))))

(defclass table-column-class (c2mop:standard-direct-slot-definition)
  ((col-type :type (or symbol cons null)
             :initarg :col-type
             :accessor %table-column-type)
   (references :type references
               :initarg :references
               :initform nil
               :reader table-column-references)
   (primary-key :type boolean
                :initarg :primary-key
                :initform nil
                :accessor primary-key-p)
   (ghost :type boolean
          :initarg :ghost
          :initform nil
          :accessor ghost-slot-p
          :documentation "Option to specify slots as ghost slots. Ghost slots do not depend on a database.")))

(defgeneric table-column-type (column)
  (:method ((column table-column-class))
    (values
     (parse-col-type (%table-column-type column)))))

(defgeneric table-column-not-null-p (column)
  (:method ((column table-column-class))
    (nth-value 1 (parse-col-type (%table-column-type column)))))

(defgeneric table-column-name (column)
  (:method ((column table-column-class))
    (unlispify (symbol-name-literally (c2mop:slot-definition-name column)))))

(defmethod initialize-instance :around ((class table-column-class) &rest rest-initargs
                                        &key name initargs ghost
                                        &allow-other-keys)

  (unless (find (symbol-name name) initargs :test #'string=)
    ;; Add the default initarg.
    (push (intern (symbol-name name) :keyword)
          (getf rest-initargs :initargs)))

  (let ((class (apply #'call-next-method class rest-initargs)))
    (unless (slot-boundp class 'col-type)
      (if (or (ghost-slot-p class)
              (slot-value class 'references))
          (setf (slot-value class 'col-type) nil)
          (error 'col-type-required
                 :slot class)))
    class))

(defgeneric table-column-references-column (column))

(defgeneric table-column-info (column driver-type)
  (:method (column (driver-type (eql :sqlite3)))
    (let (auto-increment
          (col-type (table-column-type column))
          (not-null (table-column-not-null-p column)))
      (cond
        ((or (eq col-type :serial)
             (eq col-type :bigserial))
         (setf col-type :integer
               auto-increment t
               not-null t))
        ((eq col-type :timestamptz)
         (setf col-type :timestamp)))
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
                       (primary-key-p column)))))
  (:method (column (driver-type (eql :mysql)))
    (let (auto-increment
          (col-type (table-column-type column))
          (not-null (table-column-not-null-p column)))
      (cond
        ((eq col-type :bigserial)
         (setf col-type '(:bigint () :unsigned)
               auto-increment t
               not-null t))
        ((eq col-type :serial)
         (setf col-type '(:int () :unsigned)
               auto-increment t
               not-null t))
        ((eq col-type :bytea)
         (setf col-type :binary))
        ((eq col-type :timestamptz)
         (setf col-type :timestamp)))
      `(,(table-column-name column)
        :type ,col-type
        :auto-increment ,auto-increment
        :primary-key ,(primary-key-p column)
        :not-null ,(or not-null
                       (primary-key-p column)))))
  (:method (column (driver-type (eql :postgres)))
    (let (auto-increment
          (col-type (table-column-type column))
          (not-null (table-column-not-null-p column)))
      (cond
        ((eq col-type :bigserial)
         (setf auto-increment t
               not-null t))
        ((eq col-type :serial)
         (setf auto-increment t
               not-null t))
        ((eq (ensure-car col-type) :binary)
         (setf col-type :bytea))
        ((eq (ensure-car col-type) :datetime)
         (setf col-type :timestamp)))
      `(,(table-column-name column)
        :type ,col-type
        :auto-increment ,auto-increment
        :primary-key ,(primary-key-p column)
        :not-null ,(or not-null
                       (primary-key-p column)))))
  (:method :around (column driver-type)
    (let ((rel-column (table-column-references-column column)))
      (if rel-column
          (let* ((info (call-next-method))
                 (rel-col-type (getf (cdr (table-column-info rel-column driver-type)) :type)))
            (setf (getf (cdr info) :type)
                  (ecase driver-type
                    (:mysql
                     (case rel-col-type
                       (:bigserial :bigint)
                       (:serial '(:int () :unsigned))
                       (otherwise rel-col-type)))
                    (:postgres
                     (case rel-col-type
                       (:bigserial :bigint)
                       (:serial :int)
                       (otherwise rel-col-type)))
                    (:sqlite3
                     (case rel-col-type
                       ((:bigserial :serial) :integer)
                       (otherwise rel-col-type)))))
            info)
          (call-next-method)))))

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
