(defsystem "lack-middleware-mito"
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on ("mito-core"
               "dbi")
  :components ((:module "src"
                :components
                ((:file "middleware")))))
