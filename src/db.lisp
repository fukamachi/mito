(in-package :cl-user)
(defpackage mito.db
  (:use #:cl)
  (:import-from #:dbi
                #:connection-driver-type)
  (:export #:last-insert-id
           #:table-indices
           #:column-definitions))
(in-package :mito.db)

(defun last-insert-id (conn table-name serial-key-name)
  (ecase (dbi:connection-driver-type conn)
    (:mysql    (mito.db.mysql:last-insert-id conn table-name serial-key-name))
    (:postgres (mito.db.postgres:last-insert-id conn table-name serial-key-name))
    (:sqlite3  (mito.db.sqlite3:last-insert-id conn table-name))))

(defun table-indices (conn table-name)
  (sort
   (funcall
    (ecase (dbi:connection-driver-type conn)
      (:mysql    #'mito.db.mysql:table-indices)
      (:postgres #'mito.db.postgres:table-indices)
      (:sqlite3  #'mito.db.sqlite3:table-indices))
    conn table-name)
   (lambda (a b)
     (cond
       ((getf a :primary-key)
        (not (getf b :primary-key)))
       ((getf b :primary-key) nil)
       ((getf a :unique-key)
        (or (not (getf b :unique-key))
            (string< (prin1-to-string a) (prin1-to-string b))))
       (t
        (string< (prin1-to-string a) (prin1-to-string b)))))
   :key #'cdr))

(defun column-definitions (conn table-name)
  (funcall
   (ecase (dbi:connection-driver-type conn)
     (:mysql    #'mito.db.mysql:column-definitions)
     (:postgres #'mito.db.postgres:column-definitions)
     (:sqlite3  #'mito.db.sqlite3:column-definitions))
   conn table-name))
