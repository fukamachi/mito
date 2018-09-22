(defpackage #:mito.middleware
  (:nicknames #:lack.middleware.mito)
  (:use #:cl)
  (:import-from #:mito
                #:*connection*)
  (:import-from #:dbi
                #:connect-cached)
  (:export #:*lack-middleware-mito*))
(in-package #:mito.middleware)

(defparameter *lack-middleware-mito*
  (lambda (app db-config)
    (lambda (env)
      (let ((mito:*connection* (apply #'dbi:connect-cached db-config)))
        (funcall app env)))))
