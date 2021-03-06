(defsystem "mito-core"
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ((:version "dbi" "0.9.5")
               "sxql"
               "cl-ppcre"
               "closer-mop"
               "dissect"
               "trivia"
               "cl-reexport"
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
                  :depends-on ("connection" "class" "db" "logger" "util")
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
                 (:file "logger")
                 (:file "error")
                 (:file "util")))))
