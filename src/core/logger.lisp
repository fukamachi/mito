(in-package :cl-user)
(defpackage mito.logger
  (:use #:cl)
  (:import-from #:dbi
                #:*sql-execution-hooks*)
  (:import-from #:alexandria
                #:delete-from-plist)
  (:export #:*mito-logger-stream*
           #:*mito-migration-logger-stream*
           #:*trace-sql-hooks*
           #:mito-sql-logger
           #:with-trace-sql
           #:with-sql-logging))
(in-package :mito.logger)

(defvar *mito-logger-stream* nil)

(defvar *mito-migration-logger-stream* (make-synonym-stream '*standard-output*)
  "Stream to output sql generated during migrations.")

(defun get-prev-stack ()
  (labels ((stack-call (stack)
             (let ((call (dissect:call stack)))
               (typecase call
                 (symbol call)
                 (cons
                   (case (first call)
                     (:method (second call))
                     ((lambda flet labels) nil)
                     (otherwise (second call)))))))
           #+sbcl
           (sbcl-package-p (package)
             (let ((name (package-name package)))
               (eql (mismatch "SB-" name) 3)))
           (system-call-p (call)
             (when call
               (let ((package (symbol-package call)))
                 (or #+sbcl (sbcl-package-p package)
                     (find (package-name package)
                           '(:common-lisp :mito.logger :mito.db :mito.dao :mito.util :dbi.logger :dbi.driver)
                           :test #'string=)))))
           (users-stack-p (stack)
             (let ((call (stack-call stack)))
               (and call
                    (or (not (symbolp call))
                        (not (system-call-p call)))))))

    (loop with prev-stack = nil
          for stack in (dissect:stack)
          when (users-stack-p stack)
            do (return (stack-call stack)))))

(defun mito-sql-logger (sql params row-count took-ms prev-stack)
  (when *mito-logger-stream*
    (format *mito-logger-stream*
            "~&~<;; ~@;~A (~{~S~^, ~}) ~@[[~D row~:P]~]~@[ (~Dms)~]~:[~;~:* | ~S~]~:>~%"
            (list sql
                  (mapcar (lambda (param)
                            (if (typep param '(simple-array (unsigned-byte 8) (*)))
                                (map 'string #'code-char param)
                                param))
                          params)
                  row-count
                  took-ms
                  prev-stack))))

(defvar *trace-sql-hooks* (list #'mito-sql-logger))

(defun trace-sql (sql params row-count took-ms)
  (when *trace-sql-hooks*
    (let ((prev-stack (get-prev-stack)))
      (dolist (hook *trace-sql-hooks*)
        (funcall hook sql params row-count took-ms prev-stack)))))

(defmacro with-trace-sql (&body body)
  `(let ((dbi:*sql-execution-hooks* (cons #'trace-sql
                                          dbi:*sql-execution-hooks*)))
     ,@body))

(defmacro with-sql-logging (&body body)
  `(let ((*mito-logger-stream* *mito-migration-logger-stream*))
     (with-trace-sql ,@body)))
