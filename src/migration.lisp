(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-bind (#+sbcl (warning #'muffle-warning))
    (defpackage mito.migration
      (:use #:cl))))
(in-package :mito.migration)

(cl-reexport:reexport-from :mito.migration.table)
(cl-reexport:reexport-from :mito.migration.versions)
