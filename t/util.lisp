(in-package :cl-user)
(defpackage mito-test.util
  (:use #:cl
        #:sxql)
  (:import-from #:mito.class
                #:create-table-sxql)
  (:import-from #:dbi
                #:disconnect
                #:connect
                #:connection-driver-type
                #:connection-database-name)
  (:export #:disconnect-from-testdb
           #:connect-to-testdb
           #:reconnect-to-testdb
           #:is-table-class))
(in-package :mito-test.util)

(defun sqlite3-disconnect-from-testdb (conn)
  (when conn
    (ignore-errors (dbi:disconnect conn))
    (let ((db-path (dbi:connection-database-name conn)))
      (when (probe-file db-path)
        (delete-file db-path)))))

(defun sqlite3-connect-to-testdb ()
  (dbi:connect :sqlite3 :database-name (asdf:system-relative-pathname :mito #P"t/test.db")))

(defun postgres-disconnect-from-testdb (conn)
  (dbi:disconnect conn))

(defun postgres-connect-to-testdb ()
  (dbi:connect-cached :postgres
                      :database-name "mito"
                      :host (or (uiop:getenv "POSTGRES_HOST") "localhost")
                      :port (parse-integer
                              (or (uiop:getenv "POSTGRES_PORT")
                                  "5432"))
                      :username (or (uiop:getenv "POSTGRES_USER") "nobody")
                      :password (or (uiop:getenv "POSTGRES_PASS") "nobody")
                      :microsecond-precision t))

(defun mysql-disconnect-from-testdb (conn)
  (dbi:disconnect conn))

(defun mysql-connect-to-testdb ()
  (dbi:connect :mysql
               :database-name "mito"
               :host (or (uiop:getenv "MYSQL_HOST") "localhost")
               :port (parse-integer
                       (or (uiop:getenv "MYSQL_PORT")
                           "3306"))
               :username (or (uiop:getenv "MYSQL_USER") "nobody")
               :password (or (uiop:getenv "MYSQL_PASS") "nobody")))

(defun disconnect-from-testdb (conn)
  (funcall
   (ecase (connection-driver-type conn)
     (:sqlite3  #'sqlite3-disconnect-from-testdb)
     (:mysql    #'mysql-disconnect-from-testdb)
     (:postgres #'postgres-disconnect-from-testdb))
   conn))

(defun connect-to-testdb (driver-type)
  (funcall
   (ecase driver-type
     (:sqlite3  #'sqlite3-connect-to-testdb)
     (:mysql    #'mysql-connect-to-testdb)
     (:postgres #'postgres-connect-to-testdb))))

(defun reconnect-to-testdb (conn)
  (disconnect-from-testdb conn)
  (connect-to-testdb (connection-driver-type conn)))

(defmacro is-table-class (driver class-definition create-table &optional desc)
  (let ((class (gensym "CLASS")))
    `(let ((,class ,class-definition))
       (prove:is (let ((sxql:*use-placeholder* nil))
                   (mapcar #'sxql:yield (create-table-sxql ,class ,driver)))
                 (alexandria:ensure-list ,create-table)
                 (format nil "~A (~S)~:[~;~:* ~A~]" (class-name ,class) ,driver ,desc)))))
