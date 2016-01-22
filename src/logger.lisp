(in-package :cl-user)
(defpackage mito.logger
  (:use #:cl)
  (:import-from #:alexandria
                #:delete-from-plist)
  (:export #:enable-sql-logger
           #:disable-sql-logger
           #:with-sql-logging
           #:trace-sql))
(in-package :mito.logger)

(defun get-prev-stack ()
  (labels ((stack-call (stack)
             (let ((call (dissect:call stack)))
               (typecase call
                 (symbol call)
                 (cons
                  (when (eq (first call) :method)
                    (second call))))))
           #+sbcl
           (sbcl-package-p (package)
             (let ((name (package-name package)))
               (eql (mismatch "SB-" name) 3)))
           (system-call-p (call)
             (when call
               (let ((package (symbol-package call)))
                 (or #+sbcl (sbcl-package-p package)
                     (find (package-name package)
                           '(:common-lisp :mito.logger)
                           :test #'string=)))))
           (users-stack-p (stack)
             (let ((call (stack-call stack)))
               (and call
                    (not (system-call-p call))))))

    (loop with prev-stack = nil
          repeat 5
          for stack in (dissect:stack)
          when (users-stack-p stack)
            do (setf prev-stack stack)
          finally (return (when prev-stack
                            (stack-call prev-stack))))))

(defun enable-sql-logger ()
  (vom:config :mito.logger :debug))

(defun disable-sql-logger ()
  (setf vom::*config*
        (delete-from-plist vom::*config* :mito.logger))
  (getf vom::*config* t))

(defmacro with-sql-logging (&body body)
  `(let ((vom::*config* (append '(:mito.logger :debug) vom::*config*)))
     ,@body))

(defun trace-sql (sql params &optional results)
  (vom:debug "~A (~{~S~^, ~}) [~D row~:P]~:[~;~:* | ~S~]~%"
             sql
             params
             (length results)
             (get-prev-stack)))
