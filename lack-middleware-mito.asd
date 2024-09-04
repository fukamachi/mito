(defsystem "lack-middleware-mito"
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on ("mito-core"
               "dbi")
  :components ((:module "src"
                :components
                ((:file "middleware")))))
