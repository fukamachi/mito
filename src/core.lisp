(uiop:define-package #:mito.core
  (:use #:cl)
  (:use-reexport #:mito.dao
                 #:mito.error)
  (:import-from #:mito.db
                #:*use-prepare-cached*
                #:execute-sql
                #:retrieve-by-sql)
  (:import-from #:mito.connection
                #:*connection*
                #:connect-toplevel
                #:disconnect-toplevel
                #:connection-database-name)
  (:import-from #:mito.logger
                #:*mito-logger-stream*
                #:*mito-migration-logger-stream*
                #:*trace-sql-hooks*)
  (:export #:*use-prepare-cached*
           #:execute-sql
           #:retrieve-by-sql
           #:*connection*
           #:connect-toplevel
           #:disconnect-toplevel
           #:connection-database-name
           #:*mito-logger-stream*
           #:*mito-migration-logger-stream*
           #:*trace-sql-hooks*))
(in-package #:mito.core)
