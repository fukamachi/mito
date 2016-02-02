(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-bind (#+sbcl (warning #'muffle-warning))
    (defpackage mito.core
      (:use #:cl))))
(in-package :mito.core)

(cl-reexport:reexport-from :mito.connection
                           :include '(#:*connection*
                                      #:connect-toplevel
                                      #:disconnect-toplevel
                                      #:with-connection))
(cl-reexport:reexport-from :mito.dao
                           :include '(#:dao-class
                                      #:dao-table-class
                                      #:object-id
                                      #:dao-synced
                                      #:inflate
                                      #:deflate
                                      #:make-dao-instance
                                      #:table-definition

                                      #:insert-dao
                                      #:update-dao
                                      #:create-dao
                                      #:delete-dao
                                      #:save-dao
                                      #:select-dao
                                      #:find-dao
                                      #:recreate-table
                                      #:ensure-table-exists))
(cl-reexport:reexport-from :mito.db
                           :include '(#:execute-sql
                                      #:retrieve-by-sql))
(cl-reexport:reexport-from :mito.logger
                           :include '(#:logger-stream))
(cl-reexport:reexport-from :mito.error)
