(defpackage #:mito.middleware
  (:nicknames #:lack.middleware.mito)
  (:use #:cl)
  (:import-from #:mito.core
                #:*connection*)
  (:import-from #:dbi
                #:connect-cached)
  (:export #:*lack-middleware-mito*))
(in-package #:mito.middleware)

(defparameter *lack-middleware-mito*
  (lambda (app db-config)
    (if db-config
        (lambda (env)
          (let ((mito.core:*connection* (apply #'dbi:connect-cached db-config)))
            (funcall app env)))
        app)))
