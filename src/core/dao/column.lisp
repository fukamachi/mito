(in-package :cl-user)
(defpackage mito.dao.column
  (:use #:cl
        #:mito.util)
  (:import-from #:mito.class.column
                #:table-column-class
                #:table-column-type
                #:table-column-info)
  (:import-from #:mito.class
                #:table-primary-key
                #:find-slot-by-name)
  (:import-from #:alexandria
                #:ensure-car
                #:ensure-list
                #:when-let)
  (:export #:dao-table-column-class
           #:dao-table-column-inflate
           #:dao-table-column-deflate
           #:dao-table-column-foreign-class
           #:dao-table-column-foreign-slot
           #:inflate-for-col-type
           #:deflate-for-col-type))
(in-package :mito.dao.column)

(deftype references ()
  '(or null symbol (cons symbol (or null (cons symbol null)))))

(defclass dao-table-column-class (table-column-class)
  ((inflate :type (or function null)
            :initarg :inflate)
   (deflate :type (or function null)
            :initarg :deflate)
   (references :type references
               :initarg :references
               :initform nil
               :reader dao-table-column-references)))

(defgeneric dao-table-column-foreign-class (column)
  (:method ((column dao-table-column-class))
    (when-let (foreign-class-name (or (ensure-car (dao-table-column-references column))
                                      (and (typep (table-column-type column)
                                                  '(and symbol (not null) (not keyword)))
                                           (table-column-type column))))
      (find-class foreign-class-name))))

(defgeneric dao-table-column-foreign-slot (column)
  (:method ((column dao-table-column-class))
    (when-let (foreign-class (dao-table-column-foreign-class column))
      (let ((foreign-slot-name (second (ensure-list (dao-table-column-references column)))))
        (unless foreign-slot-name
          (let ((pk-names (table-primary-key foreign-class)))
            (unless pk-names
              (error "Foreign class ~S has no primary keys and no slot name is specified to :references"
                     (class-name foreign-class)))
            (when (cdr pk-names)
              (error "Foreign class ~S has a composite primary key and failed to detect which to use for :references"
                     (class-name foreign-class)))
            (setf foreign-slot-name (first pk-names))))
        (or (find-slot-by-name foreign-class foreign-slot-name
                               :test #'string=)
            (error "No slot named ~S in foreign class ~S"
                   foreign-slot-name
                   (class-name foreign-class)))))))

(defmethod initialize-instance :after ((class dao-table-column-class) &rest initargs
                                       &key col-type &allow-other-keys)
  (declare (ignore initargs))
  ;; Make :col-type optional when :references is specified
  (when (and (null col-type)
             (slot-value class 'references))
    (setf (table-column-type class) nil)))

(defmethod initialize-instance :around ((object dao-table-column-class) &rest rest-initargs
                                        &key name initargs ghost inflate deflate
                                        &allow-other-keys)
  (when (and (not ghost)
             (not (find (symbol-name name) initargs :test #'string=)))
    ;; Add the default initarg.
    (push (intern (symbol-name name) :keyword)
          (getf rest-initargs :initargs)))

  (when inflate
    (setf (getf rest-initargs :inflate) (eval inflate)))
  (when deflate
    (setf (getf rest-initargs :deflate) (eval deflate)))

  (apply #'call-next-method object rest-initargs))

(defmethod table-column-info ((column dao-table-column-class) driver-type)
  (if (dao-table-column-foreign-slot column)
      (let* ((column-info (call-next-method))
             (rel-column-info (table-column-info (dao-table-column-foreign-slot column) driver-type))
             (new-col-type (getf (cdr rel-column-info) :type)))
        (setf (getf (cdr column-info) :type)
              (ecase driver-type
                (:mysql
                 (case new-col-type
                   (:bigserial :bigint)
                   (:serial '(:int () :unsigned))
                   (otherwise new-col-type)))
                (:postgres
                 (case new-col-type
                   (:bigserial :bigint)
                   (:serial :int)
                   (otherwise new-col-type)))
                (:sqlite3
                 (case new-col-type
                   ((:bigserial :serial) :integer)
                   (otherwise new-col-type)))))
        (setf (getf (cdr column-info) :not-null)
              (optima:match (table-column-type column)
                ((or (list 'or :null _)
                     (list 'or _ :null))
                 nil)
                (otherwise t)))
        column-info)
      (call-next-method)))

(defgeneric dao-table-column-inflate (column value)
  (:method ((column dao-table-column-class) value)
    (if (slot-boundp column 'inflate)
        (funcall (slot-value column 'inflate) value)
        (inflate-for-col-type
         (mito.class.column::parse-col-type (table-column-type column))
         value))))

(defgeneric dao-table-column-deflate (column value)
  (:method ((column dao-table-column-class) value)
    (if (slot-boundp column 'deflate)
        (funcall (slot-value column 'deflate) value)
        (deflate-for-col-type
          (mito.class.column::parse-col-type (table-column-type column))
          value))))

(defgeneric inflate-for-col-type (col-type value)
  (:method (col-type value)
    (declare (ignore col-type))
    (identity value))
  (:method ((col-type (eql :datetime)) value)
    (etypecase value
      (integer
       (local-time:universal-to-timestamp value))
      (string
       (local-time:parse-timestring value :date-time-separator #\Space))
      (null nil)))
  (:method ((col-type (eql :date)) value)
    (inflate-for-col-type :datetime value))
  (:method ((col-type (eql :timestamp)) value)
    (inflate-for-col-type :datetime value))
  (:method ((col-type (eql :time)) value)
    (flet ((v (key)
             (second (assoc key value))))
      (if (consp value)
          (format nil "~2,'0D:~2,'0D:~2,'0D~:[.~3,'0D~;~]"
                  (v :hours) (v :minutes) (v :seconds) (= (v :microseconds) 0) (v :microseconds))
          value)))
  (:method ((col-type (eql :boolean)) value)
    (cond
      ;; MySQL & SQLite3
      ((typep value 'integer)
       (not (= value 0)))
      ;; PostgreSQL
      ((typep value 'boolean)
       value)
      (t
       (error "Unexpected value for boolean column: ~S" value)))))

(defgeneric deflate-for-col-type (col-type value)
  (:method (col-type value)
    (declare (ignore col-type))
    (identity value))
  (:method ((col-type (eql :datetime)) value)
    (etypecase value
      (integer
       (local-time:format-timestring nil (local-time:universal-to-timestamp value)
                                     :format
                                     '((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2) #\Space (:HOUR 2) #\: (:MIN 2) #\: (:SEC 2))))
      (local-time:timestamp
       (local-time:format-timestring nil value
                                     :format
                                     '((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2) #\Space (:HOUR 2) #\: (:MIN 2) #\: (:SEC 2))))
      (string value)
      (null nil)))
  (:method ((col-type (eql :date)) value)
    (deflate-for-col-type :datetime value))
  (:method ((col-type (eql :timestamp)) value)
    (deflate-for-col-type :datetime value))
  (:method ((col-type (eql :boolean)) value)
    (ecase value
      (t 1)
      ('nil 0))))
