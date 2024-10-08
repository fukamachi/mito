(defsystem "mito-migration"
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on ("mito-core"
               "sxql"
               "dbi"
               "closer-mop"
               "esrap"
               "alexandria"
               "uiop"
               "chipz")
  :components ((:file "src/migration" :depends-on ("migration-components"))
               (:module "migration-components"
                :pathname "src/migration"
                :components
                ((:file "table" :depends-on ("sxql"))
                 (:file "versions" :depends-on ("table" "sql-parse" "util"))
                 (:file "sxql")
                 (:file "sql-parse")
                 (:file "util")))))
