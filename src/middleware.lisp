(defpackage #:mito.middleware
  (:nicknames #:lack.middleware.mito)
  (:use #:cl)
  (:import-from #:mito.core
                #:*connection*)
  (:import-from #:dbi
                #:connect
                #:connect-cached)
  (:export #:*lack-middleware-mito*))
(in-package #:mito.middleware)

(defparameter *lack-middleware-mito*
  (lambda (app db-config &key (no-cache nil no-cache-specified-p))
    (let* ((driver (first db-config))
           (no-cache (cond
                       (no-cache-specified-p no-cache)
                       ((eq driver :sqlite3) t)
                       (t nil))))
      (if db-config
          (if no-cache
              (lambda (env)
                (let ((mito.core:*connection* (apply #'dbi:connect db-config)))
                  (unwind-protect
                      (funcall app env)
                    (dbi:disconnect mito.core:*connection*))))
              (lambda (env)
                (let ((mito.core:*connection* (apply #'dbi:connect-cached db-config)))
                  (funcall app env))))
          app))))
