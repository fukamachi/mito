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
               :alexandria)
  :components ((:file "src/migration")))
