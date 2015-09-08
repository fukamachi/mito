(in-package :cl-user)
(defpackage mito.util
  (:use #:cl)
  (:export #:group-by-plist
           #:lispify
           #:unlispify
           #:symbol-name-literally))
(in-package :mito.util)

(defun group-by-plist (plists &key key (test #'equal))
  (loop with map = (list)
        for plist in plists
        for found = (assoc (getf plist key) map :test test)
        if found
          do (push plist (cdr found))
        else
          do (setf map (cons (cons (getf plist key)
                                   (list plist))
                             map))
        finally
           (return
             (mapc (lambda (record)
                     (rplacd record
                             (nreverse (cdr record))))
                   (nreverse map)))))

(defun escaped-symbol-p (symbol)
  (declare (optimize speed)
           (type symbol symbol))
  (not (string= symbol (string-upcase symbol))))

(defun symbol-name-literally (symbol)
  (if (escaped-symbol-p symbol)
      (symbol-name symbol)
      (string-downcase symbol)))

(defun lispify (object)
  (etypecase object
    (symbol (intern (lispify (string-upcase object))
                    (symbol-package object)))
    (string (substitute #\- #\_ object))))

(defun unlispify (object)
  (etypecase object
    (symbol (intern (unlispify (symbol-name-literally object))
                    (symbol-package object)))
    (string (substitute #\_ #\- object))))
