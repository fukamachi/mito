(in-package :cl-user)
(defpackage mito.connection
  (:use #:cl
        #:mito.error)
  (:import-from #:dbi
                #:connect
                #:disconnect
                #:connection-driver-type)
  (:export #:*connection*
           #:driver-type
           #:connection-database-name
           #:connected-p
           #:check-connected
           #:connect-toplevel
           #:disconnect-toplevel
           #:connection-quote-character
           #:with-quote-char))
(in-package :mito.connection)

(defvar *connection* nil)

(defun connection-database-name (&optional conn)
  "Return the name of the current connection, or the one given as argument."
  (dbi:connection-database-name (or conn *connection*)))

(defun connected-p ()
  (not (null *connection*)))

(defun check-connected ()
  (or (connected-p)
      (error 'connection-not-established)))

(defun driver-type (&optional conn)
  (unless conn
    (check-connected)
    (setf conn *connection*))
  (dbi:connection-driver-type conn))

(defun connect-toplevel (driver-name &rest args &key database-name &allow-other-keys)
  (declare (ignore database-name))
  (setf *connection* (apply #'dbi:connect driver-name args)))

(defun disconnect-toplevel ()
  (when (connected-p)
    (dbi:disconnect *connection*)
    (setf *connection* nil)))

(defun connection-quote-character (conn)
  (ecase (connection-driver-type conn)
    (:mysql #\`)
    (:postgres #\")
    (:sqlite3 #\")))

(defmacro with-quote-char (&body body)
  `(let ((sxql:*quote-character* (or sxql:*quote-character*
                                     (connection-quote-character *connection*))))
     ,@body))
