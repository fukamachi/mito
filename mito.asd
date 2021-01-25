(in-package :cl-user)
(defpackage mito-asd
  (:use :cl :asdf))
(in-package :mito-asd)

(defsystem mito
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:mito-core
               :mito-migration
               :lack-middleware-mito
               :cl-reexport
               #-sb-package-locks :cl-package-locks)
  :components ((:file "src/mito"))
  :description "Abstraction layer for DB schema"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input
                            :element-type #+lispworks :default #-lispworks 'character
                            :external-format #+clisp charset:utf-8 #-clisp :utf-8)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op mito-test))))
