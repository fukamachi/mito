(in-package :cl-user)
(defpackage mito.db.sqlite3
  (:use #:cl
        #:sxql)
  (:import-from #:dbi
                #:prepare
                #:execute
                #:fetch
                #:fetch-all)
  (:export #:last-insert-id
           #:column-definitions
           #:table-indices))
(in-package :mito.db.sqlite3)

(defun table-info (conn table-name)
  (let* ((sql (format nil "PRAGMA table_info(\"~A\")" table-name)))
    (or (dbi:fetch-all (dbi:execute (dbi:prepare conn sql)))
        (error "Table \"~A\" doesn't exist." table-name))))

(defun last-insert-id (conn table-name)
  (let ((primary-keys (table-primary-keys conn table-name)))
    (when (rest primary-keys)
      (error "last-insert-id doesn't support composite primary keys."))
    (let ((primary-key (intern (first primary-keys) :keyword)))

      (getf (dbi:fetch
             (dbi:execute
              (dbi:prepare conn
                           (sxql:yield
                            (select ((:as primary-key :last_insert_id))
                              (from (intern table-name :keyword))
                              (order-by (:desc primary-key))
                              (limit 1))))))
            :|last_insert_id|
            0))))

(defun column-definitions (conn table-name)
  ;; FIXME: quote
  (flet ((column-auto-increment-p (column)
           (and (= (getf column :|pk|) 1)
                (string-equal (getf column :|type|) "INTEGER"))))
    (loop for column in (table-info conn table-name)
          collect (list (getf column :|name|)
                        :type (getf column :|type|)
                        :auto-increment (column-auto-increment-p column)
                        :primary-key (= (getf column :|pk|) 1)
                        :not-null (or (= (getf column :|pk|) 1)
                                      (not (= (getf column :|notnull|) 0)))))))

(defun table-primary-keys (conn table-name)
  (mapcar #'(lambda (column) (getf column :|name|))
          (remove-if-not (lambda (column)
                           (= (getf column :|pk|) 1))
                         (table-info conn table-name))))

(defun table-indices (conn table-name)
  (let ((primary-keys (table-primary-keys conn table-name))
        (query (dbi:execute
                (dbi:prepare conn (format nil "PRAGMA index_list(~A)" table-name)))))
    (append
     (loop for index = (dbi:fetch query)
           while index
           collect
           (let* ((columns (mapcar
                            (lambda (info) (getf info :|name|))
                            (dbi:fetch-all
                             (dbi:execute (dbi:prepare conn (format nil "PRAGMA index_info('~A')"
                                                                    (getf index :|name|)))))))
                  (unique-key (= (getf index :|unique|) 1))
                  (primary-key (and unique-key
                                    primary-keys
                                    (equal columns primary-keys))))
             (when primary-key
               (setf primary-keys nil))
             (list (getf index :|name|)
                   :unique-key unique-key
                   :primary-key primary-key
                   :columns columns)))
     (if primary-keys
         (list (list "PRIMARY" :unique-key t :primary-key t :columns primary-keys))
         nil))))
