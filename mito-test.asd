(defsystem "mito-test"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on ("mito"
               "dbd-mysql"
               "dbd-postgres"
               "dbd-sqlite3"
               "rove")
  :components ((:module "t"
                :components
                ((:file "util")
                 (:file "db/main")
                 (:file "db/sqlite3")
                 (:file "db/mysql")
                 (:file "db/postgres")
                 (:file "class")
                 (:file "dao")
                 (:file "migration/sqlite3")
                 (:file "migration/mysql")
                 (:file "migration/postgres")
                 (:file "postgres-types")
                 (:file "mixin"))))
  :description "Test system for mito"
  :perform (test-op (op c) (symbol-call :rove :run c)))
