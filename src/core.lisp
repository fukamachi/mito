(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-bind (#+sbcl (warning #'muffle-warning))
    (defpackage mito.core
      (:use #:cl))))
(in-package :mito.core)

(cl-reexport:reexport-from :mito.connection
                           :include '(#:*connection*
                                      #:connect-toplevel
                                      #:disconnect-toplevel))
(cl-reexport:reexport-from :mito.dao
                           :include '(#:dao-class
                                      #:dao-table-class
                                      #:object-id
                                      #:dao-synced
                                      #:make-dao-instance
                                      #:table-definition

                                      #:dao-table-mixin

                                      #:insert-dao
                                      #:update-dao
                                      #:create-dao
                                      #:delete-dao
                                      #:save-dao
                                      #:select-dao
                                      #:includes
                                      #:find-dao
                                      #:retrieve-dao
                                      #:count-dao
                                      #:recreate-table
                                      #:ensure-table-exists))
(cl-reexport:reexport-from :mito.db
                           :include '(#:execute-sql
                                      #:retrieve-by-sql))
(cl-reexport:reexport-from :mito.logger
                           :include '(#:logger-stream))
(cl-reexport:reexport-from :mito.error)
