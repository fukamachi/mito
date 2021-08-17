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
  (labels ((normalize-call (call)
             (typecase call
               (symbol call)
               (cons
                 (case (first call)
                   (:method (second call))
                   ((lambda flet labels) nil)
                   (otherwise (second call))))))
           #+sbcl
           (sbcl-package-p (package)
             (let ((name (package-name package)))
               (eql (mismatch "SB-" name) 3)))
           (system-call-p (call)
             (when call
               (let ((package (symbol-package call)))
                 (and package
                      (or #+sbcl (sbcl-package-p package)
                          (find (package-name package)
                                '(:common-lisp :mito.logger :mito.db :mito.dao :mito.util :dbi.logger :dbi.driver)
                                :test #'string=))))))
           (users-call-p (call)
             (and call
                  (or (not (symbolp call))
                      (not (system-call-p call))))))

    #+sbcl
    (do ((frame (sb-di:frame-down (sb-di:top-frame))
                (sb-di:frame-down frame)))
        ((null frame))
      (multiple-value-bind (call args info)
          (sb-debug::frame-call frame)
        (let ((call (normalize-call call)))
          (when (users-call-p call)
            (return call)))))
    #+ccl
    (block nil
      (let ((i 0))
        (ccl:map-call-frames
          (lambda (pointer context)
            (let* ((function (ccl:frame-function pointer context))
                   (call (normalize-call (or (ccl:function-name function) function))))
              (when (users-call-p call)
                (return call)))
            (incf i))
          :start-frame-number 1)))
    #-(or sbcl ccl)
    (loop with prev-stack = nil
          for stack in (dissect:stack)
          for call = (let ((call (dissect:call stack)))
                       (normalize-call call))
          when (users-call-p call)
          do (return call))))

(defun mito-sql-logger (sql params row-count took-usec prev-stack)
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
                  took-usec
                  prev-stack))))

(defvar *trace-sql-hooks* (list #'mito-sql-logger))

(defun trace-sql (sql params row-count took-usec)
  (when *trace-sql-hooks*
    (let ((prev-stack (get-prev-stack)))
      (dolist (hook *trace-sql-hooks*)
        (funcall hook sql params row-count took-usec prev-stack)))))

(defmacro with-trace-sql (&body body)
  `(let ((dbi:*sql-execution-hooks* (cons #'trace-sql
                                          dbi:*sql-execution-hooks*)))
     ,@body))

(defmacro with-sql-logging (&body body)
  `(let ((*mito-logger-stream* *mito-migration-logger-stream*))
     (with-trace-sql ,@body)))
