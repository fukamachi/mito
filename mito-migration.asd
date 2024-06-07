(defsystem "mito-migration"
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
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
