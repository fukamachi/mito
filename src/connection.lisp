(in-package :cl-user)
(defpackage mito.connection
  (:use #:cl)
  (:import-from #:dbi
                #:connect-cached
                #:disconnect
                #:connection-driver-type)
  (:export #:*connection*
           #:driver-type
           #:connected-p
           #:check-connected
           #:connect-toplevel
           #:disconnect-toplevel
           #:with-connection
           #:connection-quote-character))
(in-package :mito.connection)

(defvar *connection*)

(defun connected-p ()
  (and (boundp '*connection*)
       (not (null *connection*))))

(defun check-connected ()
  (or (connected-p)
      (error "Connection is not established yet.")))

(defun driver-type (&optional conn)
  (unless conn
    (check-connected)
    (setf conn *connection*))
  (dbi:connection-driver-type conn))

(defun connect-toplevel (driver-name &rest args &key database-name &allow-other-keys)
  (declare (ignore database-name))
  (setf *connection* (apply #'dbi:connect-cached driver-name args)))

(defun disconnect-toplevel ()
  (when (connected-p)
    (dbi:disconnect *connection*)
    (makunbound '*connection*)))

(defmacro with-connection (conn &body body)
  `(let ((*connection* ,conn))
     ,@body))

(defun connection-quote-character (conn)
  (ecase (connection-driver-type conn)
    (:mysql #\`)
    (:postgres #\")
    (:sqlite3 #\")))
