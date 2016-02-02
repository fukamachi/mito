(in-package :cl-user)
(defpackage mito.migration.versions
  (:use #:cl
        #:sxql)
  (:import-from #:mito.migration.table
                #:migration-expressions)
  (:import-from #:mito.dao
                #:dao-class
                #:dao-table-class
                #:table-definition)
  (:import-from #:mito.connection
                #:*connection*
                #:check-connected)
  (:import-from #:mito.class
                #:table-name)
  (:import-from #:mito.db
                #:execute-sql
                #:retrieve-by-sql
                #:table-exists-p)
  (:export #:all-migration-expressions
           #:current-migration-version
           #:update-migration-version))
(in-package :mito.migration.versions)

(defun initialize-migrations-table ()
  (check-connected)
  (execute-sql
   (sxql:create-table (:schema_migrations :if-not-exists t)
       ((version :type '(:varchar 255)
                 :primary-key t)))))

(defun all-dao-classes ()
  (remove-if-not (lambda (class)
                   (typep class 'dao-table-class))
                 (c2mop:class-direct-subclasses (find-class 'dao-class))))

(defun all-migration-expressions ()
  (check-connected)
  (mapcan (lambda (class)
            (if (table-exists-p *connection* (table-name class))
                (migration-expressions class)
                (list (table-definition class))))
          (all-dao-classes)))

(defun current-migration-version ()
  (initialize-migrations-table)
  (getf (first (retrieve-by-sql
                (sxql:select :version
                  (sxql:from :schema_migrations)
                  (sxql:order-by (:desc :version))
                  (sxql:limit 1))))
        :version))

(defun update-migration-version (version)
  (execute-sql
   (sxql:insert-into :schema_migrations
     (sxql:set= :version version))))
