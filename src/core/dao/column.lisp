(in-package :cl-user)
(defpackage mito.dao.column
  (:use #:cl
        #:mito.util)
  (:import-from #:mito.class.column
                #:column-standard-effective-slot-definitions
                #:table-column-class
                #:table-column-type)
  (:import-from #:local-time)
  (:import-from #:cl-ppcre)
  (:export #:dao-table-column-class
           #:dao-table-column-inflate
           #:dao-table-column-standard-effective-slot-definitions
           #:dao-table-column-deflate
           #:inflate
           #:inflate-if-bound
           #:deflate
           #:deflate-if-bound
           #:inflate-for-col-type
           #:deflate-for-col-type))
(in-package :mito.dao.column)

(defparameter *conc-name* nil)

(defclass dao-table-column-slot-definitions ()
  ((inflate :type (or function null)
            :accessor inflate
            :initarg :inflate)
   (deflate :type (or function null)
            :accessor deflate
            :initarg :deflate)))

(defgeneric inflate (obj))
(defmethod inflate (ob) nil)
(defgeneric inflate-if-bound (ob))
(defmethod inflate-if-bound (obj) nil)
(defmethod inflate-if-bound ((obj dao-table-column-slot-definitions))
  (when (slot-boundp obj 'mito.dao.column:inflate)
    (slot-value obj 'mito.dao.column:inflate)))
(defgeneric deflate (obj))
(defmethod deflate (ob) nil)
(defgeneric deflate-if-bound (ob))
(defmethod deflate-if-bound (obj) nil)
(defmethod deflate-if-bound ((obj dao-table-column-slot-definitions))
  (when (slot-boundp obj 'mito.dao.column:deflate)
    (slot-value obj 'mito.dao.column:deflate)))

(defclass dao-table-column-class (dao-table-column-slot-definitions table-column-class)
  ())

(defclass dao-table-column-standard-effective-slot-definitions
    (dao-table-column-slot-definitions
     column-standard-effective-slot-definitions)
  ())

(defmethod initialize-instance :around ((object dao-table-column-class) &rest rest-initargs
                                        &key name readers writers inflate deflate
                                        &allow-other-keys)
  (when *conc-name*
    (let ((accessor (intern
                     (format nil "~:@(~A~A~)" *conc-name* name)
                     *package*)))
      (unless readers
        (pushnew accessor readers)
        (setf (getf rest-initargs :readers) readers))
      (unless writers
        (pushnew `(setf ,accessor) writers)
        (setf (getf rest-initargs :writers) writers))))

  (when inflate
    (setf (getf rest-initargs :inflate) (eval inflate)))
  (when deflate
    (setf (getf rest-initargs :deflate) (eval deflate)))

  (apply #'call-next-method object rest-initargs))

(defgeneric dao-table-column-inflate (column value)
  (:method ((column dao-table-column-class) value)
    (if (slot-boundp column 'inflate)
        (funcall (slot-value column 'inflate) value)
        (inflate-for-col-type
         (table-column-type column)
         value))))

(defgeneric dao-table-column-deflate (column value)
  (:method ((column dao-table-column-class) value)
    (if (slot-boundp column 'deflate)
        (funcall (slot-value column 'deflate) value)
        (deflate-for-col-type
          (table-column-type column)
          value))))

(defgeneric inflate-for-col-type (col-type value)
  (:method (col-type value)
    (declare (ignore col-type))
    (identity value))
  (:method ((col-type cons) value)
    (inflate-for-col-type (first col-type) value))
  (:method ((col-type (eql :datetime)) value)
    (etypecase value
      (integer
       (local-time:universal-to-timestamp value))
      (float
       (multiple-value-bind (sec nsec)
           (truncate value)
         (local-time:universal-to-timestamp sec :nsec (* (round (* nsec 1000000)) 1000))))
      (string
       (local-time:parse-timestring value :date-time-separator #\Space))
      (null nil)))
  (:method ((col-type (eql :date)) value)
    (etypecase value
      (integer
       (local-time:universal-to-timestamp value))
      (string
       (ppcre:register-groups-bind ((#'parse-integer year month day))
           ("^(\\d{4})-(\\d{2})-(\\d{2})$" value)
         (local-time:universal-to-timestamp
           (encode-universal-time 0 0 0 day month year))))
      (null nil)))
  (:method ((col-type (eql :timestamp)) value)
    (inflate-for-col-type :datetime value))
  (:method ((col-type (eql :timestamptz)) value)
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

(defvar *db-datetime-format*
  '((:year 4) #\- (:month 2) #\- (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2) #\. (:usec 6) :gmt-offset-or-z))

(defvar *db-datetime-format-with-out-timezone*
  '((:year 4) #\- (:month 2) #\- (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2) #\. (:usec 6)))

(defvar *db-date-format*
  '((:year 4) #\- (:month 2) #\- (:day 2)))

(defgeneric deflate-for-col-type (col-type value)
  (:method (col-type value)
    (declare (ignore col-type))
    (identity value))
  (:method ((col-type cons) value)
    (deflate-for-col-type (first col-type) value))
  (:method ((col-type (eql :datetime)) value)
    (etypecase value
      (integer
        (local-time:universal-to-timestamp value))
      (local-time:timestamp
        value)
      (string value)
      (null nil)))
  (:method ((col-type (eql :date)) value)
    (etypecase value
      (local-time:timestamp
        value)
      (string value)
      (null nil)))
  (:method ((col-type (eql :timestamp)) value)
    (deflate-for-col-type :datetime value))
  (:method ((col-type (eql :timestamptz)) value)
    (deflate-for-col-type :datetime value)))
