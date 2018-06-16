(in-package :cl-user)
(defpackage mito.type
  (:use #:cl)
  (:import-from #:mito.db
                #:column-definitions)
  (:import-from #:dbi
                #:do-sql)
  (:import-from #:sxql
                #:yield
                #:drop-table
                #:create-table)
  (:export #:parse-dbtype
           #:get-column-real-type))
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
      `(,(string-upcase name)
        ,vars
        ,@rest))))

(defvar *real-type-cache* (make-hash-table :test 'equalp))

(defun get-column-real-type (conn name)
  (symbol-macrolet ((real-type
                      (gethash (list (dbi:connection-driver-type conn) name)
                               *real-type-cache*)))
    (or real-type
        (setf real-type
              (progn
                (let ((*error-output* (make-broadcast-stream)))
                  (dbi:do-sql conn
                    (sxql:yield
                     (sxql:drop-table :get_column_real_type :if-exists t))))
                (dbi:do-sql conn
                  (sxql:yield
                   (sxql:create-table :get_column_real_type
                       ((test :type name)))))
                (getf (cdr (assoc "test" (mito.db:column-definitions conn "get_column_real_type")
                                  :test #'string=))
                      :type))))))
