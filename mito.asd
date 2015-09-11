#|
  This file is a part of mito project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage mito-asd
  (:use :cl :asdf))
(in-package :mito-asd)

(defsystem mito
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:dbi
               :sxql
               :cl-ppcre
               :closer-mop
               :alexandria
               :split-sequence)
  :components ((:module "src"
                :components
                ((:file "mito" :depends-on ("class" "dao" "db" "connection" "error" "util"))
                 (:file "dao" :depends-on ("class"))
                 (:file "class" :depends-on ("class-components"))
                 (:module "class-components"
                  :pathname "class"
                  :depends-on ("error" "util")
                  :components
                  ((:file "table" :depends-on ("column"))
                   (:file "column")))
                 (:file "connection" :depends-on ("error"))
                 (:file "migration" :depends-on ("db" "type" "util"))
                 (:file "type" :depends-on ("db"))
                 (:file "db" :depends-on ("db-drivers" "connection" "dao" "class"))
                 (:module "db-drivers"
                  :pathname "db"
                  :depends-on ("util")
                  :components
                  ((:file "mysql")
                   (:file "postgres")
                   (:file "sqlite3")))
                 (:file "error")
                 (:file "util"))))
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
