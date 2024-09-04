(defsystem "mito-core"
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on ((:version "dbi" "0.11.1")
               "sxql"
               "cl-ppcre"
               "closer-mop"
               "dissect"
               "trivia"
               "local-time"
               "uuid"
               "alexandria")
  :components ((:file "src/core" :depends-on ("core-components"))
               (:module "core-components"
                :pathname "src/core"
                :components
                ((:file "dao" :depends-on ("dao-components"))
                 (:module "dao-components"
                  :pathname "dao"
                  :depends-on ("connection" "class" "db" "conversion" "logger" "util")
                  :components
                  ((:file "table" :depends-on ("column" "mixin" "view"))
                   (:file "view" :depends-on ("column"))
                   (:file "mixin" :depends-on ("column"))
                   (:file "column")))
                 (:file "class" :depends-on ("class-components"))
                 (:module "class-components"
                  :pathname "class"
                  :depends-on ("error" "util")
                  :components
                  ((:file "table" :depends-on ("column"))
                   (:file "column")))
                 (:file "connection" :depends-on ("error"))
                 (:file "type" :depends-on ("db"))
                 (:file "db" :depends-on ("db-drivers" "connection" "class" "util"))
                 (:module "db-drivers"
                  :pathname "db"
                  :depends-on ("logger" "util")
                  :components
                  ((:file "mysql")
                   (:file "postgres")
                   (:file "sqlite3")))
                 (:file "conversion")
                 (:file "logger")
                 (:file "error")
                 (:file "util")))))
