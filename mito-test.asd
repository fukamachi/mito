(defsystem "mito-test"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("mito"
               "prove")
  :components ((:module "t"
                :components
                ((:file "util")
                 (:file "db/main")
                 (:test-file "db/sqlite3")
                 (:test-file "db/mysql")
                 (:test-file "db/postgres")
                 (:test-file "class")
                 (:test-file "dao")
                 (:test-file "migration/sqlite3")
                 (:test-file "migration/mysql")
                 (:test-file "migration/postgres")
                 (:test-file "postgres-types")
                 (:test-file "mixin"))))
  :description "Test system for mito"

  :defsystem-depends-on ("prove-asdf")
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
