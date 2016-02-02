(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-bind (#+sbcl (warning #'muffle-warning))
    (defpackage mito
      (:use #:cl))))
(in-package :mito)

(cl-reexport:reexport-from :mito.core)
(cl-reexport:reexport-from :mito.migration)
