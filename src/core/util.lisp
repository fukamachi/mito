(in-package :cl-user)
(defpackage mito.util
  (:use #:cl)
  (:import-from #:closer-mop)
  (:export #:group-by-plist
           #:list-diff
           #:lispify
           #:unlispify
           #:symbol-name-literally
           #:contains-class-or-subclasses
           #:ensure-class))
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

(defun %list-diff (a b &key (key #'identity) (test #'string=))
  (cond
    ((null a)
     (values nil nil b))
    ((null b)
     (values nil a nil))
    ((funcall test
              (funcall key (car a))
              (funcall key (car b)))
     (multiple-value-bind (intersection sub-a sub-b)
         (%list-diff (cdr a) (cdr b)
                     :key key
                     :test test)
       (values (cons (car a) intersection)
               sub-a
               sub-b)))
    (T (let ((pos (position (funcall key (car a)) (cdr b)
                            :key key
                            :test test)))
         (if pos
             (multiple-value-bind (intersection sub-a sub-b)
                 (%list-diff (cdr a) (nthcdr (+ 2 pos) b)
                             :key key
                             :test test)
               (values (cons (car a) intersection)
                       sub-a
                       (append (subseq b 0 (1+ pos)) sub-b)))
             (multiple-value-bind (intersection sub-a sub-b)
                 (%list-diff (cdr a) b
                             :key key
                             :test test)
                 (values intersection
                         (cons (car a) sub-a)
                         sub-b)))))))

(defun list-diff (a b &key sort-key sort-key-a sort-key-b (sort-fn #'string<) (key #'identity) (test #'string=))
  "Compute differences two lists.
Note this can be applied for a list of string-designators."
  (%list-diff (sort (copy-list a) sort-fn :key (or sort-key-a sort-key key))
              (sort (copy-list b) sort-fn :key (or sort-key-b sort-key key))
              :key key
              :test test))

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

(defun contains-class-or-subclasses (class target-classes)
  (let ((class (if (typep class 'class)
                   class
                   (find-class class))))
    (find-if (lambda (target-class)
               (let ((target-class (if (typep target-class 'class)
                                       target-class
                                       (find-class target-class nil))))
                 (and target-class
                      (or (eq target-class class)
                          (subtypep target-class class)))))
             target-classes)))

(defun ensure-class (class-or-class-name)
  (etypecase class-or-class-name
    (symbol (find-class class-or-class-name))
    (standard-class class-or-class-name)))
