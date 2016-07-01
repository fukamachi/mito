(in-package :cl-user)
(defpackage mito.logger
  (:use #:cl)
  (:import-from #:alexandria
                #:delete-from-plist)
  (:export #:logger-stream
           #:with-sql-logging
           #:trace-sql))
(in-package :mito.logger)

(defvar *mito-logger-stream* nil)

(defun logger-stream ()
  (etypecase *mito-logger-stream*
    (symbol (symbol-value *mito-logger-stream*))
    (stream *mito-logger-stream*)
    (null (make-broadcast-stream))
    ((eql t) *standard-output*)))

(defun (setf logger-stream) (stream)
  (setf *mito-logger-stream* stream))

(defmacro with-sql-logging (&body body)
  `(let ((*mito-logger-stream* *standard-output*))
     ,@body))

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

(defun trace-sql (sql params &optional results)
  (when *mito-logger-stream*
    (format (logger-stream)
            "~&~<;; ~@; ~A (~{~S~^, ~}) [~D row~:P]~:[~;~:* | ~S~]~:>~%"
            (list sql
                  (mapcar (lambda (param)
                            (if (typep param '(simple-array (unsigned-byte 8) (*)))
                                (map 'string #'code-char param)
                                param))
                          params)
                  (length results)
                  (get-prev-stack)))))
