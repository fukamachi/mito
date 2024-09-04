(defsystem "mito"
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on ("mito-core"
               "mito-migration"
               "lack-middleware-mito"
               (:feature :sb-package-locks "cl-package-locks"))
  :components ((:file "src/mito"))
  :description "Abstraction layer for DB schema"
  :in-order-to ((test-op (test-op "mito-test"))))
