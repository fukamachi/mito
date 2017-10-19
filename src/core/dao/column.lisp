(in-package :cl-user)
(defpackage mito.dao.column
  (:use #:cl
        #:mito.util)
  (:import-from #:mito.class.column
                #:table-column-class
                #:table-column-type)
  (:export #:dao-table-column-class
           #:dao-table-column-inflate
           #:dao-table-column-deflate
           #:inflate-for-col-type
           #:deflate-for-col-type))
(in-package :mito.dao.column)

(defclass dao-table-column-class (table-column-class)
  ((inflate :type (or function null)
            :initarg :inflate)
   (deflate :type (or function null)
            :initarg :deflate)))

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
    (let ((offset (local-time::timestamp-subtimezone value local-time:*default-timezone*)))
      (etypecase value
        (integer
         (local-time:universal-to-timestamp (- value offset)))
        (string
         (local-time:parse-timestring value :date-time-separator #\Space :offset offset))
        (null nil))))
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

(defvar *db-datetime-format*
  '((:year 4) #\- (:month 2) #\- (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2) #\. (:nsec)))

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
       (local-time:format-timestring nil (local-time:universal-to-timestamp value)
                                     :format *db-datetime-format*))
      (local-time:timestamp
       (local-time:format-timestring nil value
                                     :format *db-datetime-format*))
      (string value)
      (null nil)))
  (:method ((col-type (eql :date)) value)
    (etypecase value
      (integer
       (local-time:format-timestring nil (local-time:universal-to-timestamp value)
                                     :format *db-date-format*
                                     :timezone local-time:+gmt-zone+))
      (local-time:timestamp
       (local-time:format-timestring nil value
                                     :format *db-date-format*
                                     :timezone local-time:+gmt-zone+))
      (string value)
      (null nil)))
  (:method ((col-type (eql :timestamp)) value)
    (deflate-for-col-type :datetime value))
  (:method ((col-type (eql :boolean)) value)
    (ecase value
      (t 1)
      ('nil 0))))
