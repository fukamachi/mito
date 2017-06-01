#|
  This file is a part of mito project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage mito-test-asd
  (:use :cl :asdf))
(in-package :mito-test-asd)

(defsystem mito-test
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:mito
               :prove)
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

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
