(in-package :cl-user)
(defpackage mito.error
  (:use #:cl)
  (:export #:mito-error
           #:invalid-definition
           #:col-type-required
           #:no-primary-keys
           #:connection-not-established))
(in-package :mito.error)

(define-condition mito-error (error) ())

(define-condition invalid-definition (mito-error) ())

(define-condition col-type-required (invalid-definition)
  ((slot :initarg :slot))
  (:report (lambda (condition stream)
             (with-slots (slot) condition
               (format stream
                       ":col-type is missing at ~S."
                       (c2mop:slot-definition-name slot))))))

(define-condition no-primary-keys (mito-error)
  ((table :initarg :table))
  (:report (lambda (condition stream)
             (with-slots (table) condition
               (format stream
                       "No primary keys in ~S."
                       table)))))

(define-condition connection-not-established (mito-error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream
                     "Connection is not established yet."))))
