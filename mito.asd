(defsystem "mito"
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("mito-core"
               "mito-migration"
               "lack-middleware-mito"
               (:feature :sb-package-locks "cl-package-locks"))
  :components ((:file "src/mito"))
  :description "Abstraction layer for DB schema"
  :in-order-to ((test-op (test-op "mito-test"))))
