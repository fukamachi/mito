(in-package :cl-user)
(defpackage mito.type
  (:use #:cl)
  (:export #:parse-dbtype))
(in-package :mito.type)

(defun parse-type-vars (vars)
  (flet ((db-string-p (var)
           (and (/= 0 (length var))
                (or (and (char= (aref var 0) #\')
                         (char= (aref var (1- (length var))) #\'))
                    (and (char= (aref var 0) #\")
                         (char= (aref var (1- (length var))) #\")))))
         (db-number-p (var)
           (ppcre:scan "\\d+(?:\\.\\d*)?" var)))
    (loop for var in (ppcre:split "\\s*,\\s*" vars)
          if (db-string-p var)
            collect (subseq var 1 (1- (length var)))
          else if (db-number-p var)
            collect (read-from-string var)
          else
            collect var)))

(defun parse-dbtype (dbtype)
  (flet ((parse (type)
           (let ((match
                     (nth-value 1
                                (ppcre:scan-to-strings "^([^(]+?)(?:\\s*\\(([^)]+)\\))?(?:\\s+(.+))?$" type))))
             (unless match
               (error "Invalid DB type: ~A" type))
             (values (aref match 0)
                     (parse-type-vars (aref match 1))
                     (ppcre:split "\\s+" (aref match 2))))))
    (multiple-value-bind (name vars rest)
        (parse dbtype)
      `(,(intern (string-upcase name) :keyword)
        ,vars
        ,@rest))))
