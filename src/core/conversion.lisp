(defpackage mito.conversion
  (:use :cl)
  (:import-from :local-time)
  (:export :convert-for-driver-type))
(in-package :mito.conversion)

(defvar *db-datetime-format*
  '((:year 4) #\- (:month 2) #\- (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2) #\. (:usec 6) :gmt-offset-or-z))

(defvar *db-datetime-format-without-timezone*
  '((:year 4) #\- (:month 2) #\- (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2) #\. (:usec 6)))

(defvar *db-date-format*
  '((:year 4) #\- (:month 2) #\- (:day 2)))

(defgeneric convert-for-driver-type (driver-type col-type value)
  (:method (driver-type col-type value)
    (declare (ignore driver-type col-type))
    value)
  (:method (driver-type col-type (value string))
    (declare (ignore driver-type col-type))
    value)
  (:method ((driver-type (eql :mysql)) (col-type (eql :boolean)) value)
    (ecase value
      (t 1)
      ('nil 0)))
  (:method ((driver-type (eql :mysql)) (col-type (eql :datetime)) (value local-time:timestamp))
    (local-time:format-timestring nil value
                                  :format *db-datetime-format-without-timezone*))
  (:method (driver-type (col-type (eql :datetime)) (value local-time:timestamp))
    (local-time:format-timestring nil value
                                  :format *db-datetime-format*
                                  :timezone local-time:+gmt-zone+))
  (:method (driver-type (col-type (eql :date)) (value local-time:timestamp))
    (local-time:format-timestring nil value
                                  :format *db-date-format*))
  (:method (driver-type (col-type (eql :timestamp)) value)
    (convert-for-driver-type driver-type :datetime value))
  (:method (driver-type (col-type (eql :timestamptz)) value)
    (convert-for-driver-type driver-type :datetime value))
  (:method ((driver-type (eql :sqlite3)) (col-type (eql :boolean)) value)
    (ecase value
      (t 1)
      ('nil 0)))
  (:method ((driver-type (eql :postgres)) (col-type (eql :boolean)) value)
    (ecase value
      (t "true")
      ('nil "false"))))
