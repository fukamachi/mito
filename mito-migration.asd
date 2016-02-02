(in-package :cl-user)
(defpackage mito-migration-asd
  (:use :cl :asdf))
(in-package :mito-migration-asd)

(defsystem mito-migration
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:mito-core
               :sxql
               :cl-reexport
               :alexandria)
  :components ((:file "src/migration" :depends-on ("migration-components"))
               (:module "migration-components"
                :pathname "src/migration"
                :components
                ((:file "table" :depends-on ("sxql"))
                 (:file "sxql")))))
