(in-package :cl-user)
(defpackage mito.db.sqlite3
  (:use #:cl
        #:mito.util
        #:sxql)
  (:import-from #:dbi
                #:execute
                #:fetch
                #:fetch-all)
  (:export #:last-insert-id
           #:column-definitions
           #:table-indices))
(in-package :mito.db.sqlite3)

(defun table-info (conn table-name)
  (let* ((sql (format nil "PRAGMA table_info(\"~A\")" table-name)))
    (with-prepared-query query (conn sql)
      (or (dbi:fetch-all (dbi:execute query) :format :plist)
          (error "Table \"~A\" doesn't exist." table-name)))))

(defun last-insert-id (conn table-name)
  (declare (ignore table-name))
  (with-prepared-query query (conn "SELECT last_insert_rowid() AS last_insert_id")
    (or (first (dbi:fetch
                (dbi:execute query)
                :format :values))
        0)))

(defun autoincrement-p (conn table-name)
  (with-prepared-query query (conn (format nil
                                           "SELECT 1 FROM sqlite_master WHERE tbl_name = '~A' AND sql LIKE '%AUTOINCREMENT%'"
                                           table-name))
    (and (first (dbi:fetch (dbi:execute query) :format :values))
         t)))

(defun column-definitions (conn table-name)
  (labels ((column-primary-key-p (column)
             (not (= (getf column :|pk|) 0)))
           (column-auto-increment-p (column)
             (and (column-primary-key-p column)
                  (string-equal (getf column :|type|) "INTEGER")
                  (autoincrement-p conn table-name))))
    (loop with pk-count = 0
          for column in (table-info conn table-name)
          if (column-primary-key-p column)
            do (incf pk-count)
          collect (list (getf column :|name|)
                        :type (getf column :|type|)
                        :auto-increment (column-auto-increment-p column)
                        :primary-key (column-primary-key-p column)
                        :not-null (or (column-primary-key-p column)
                                      (not (= (getf column :|notnull|) 0)))
                        :default (let ((default (getf column :|dflt_value|)))
                                   (if (stringp default)
                                       (read-from-string default)
                                       default)))
            into definitions
          finally
             (return
               (if (< 1 pk-count)
                   (mapc (lambda (def)
                           (setf (getf (cdr def) :auto-increment) nil)
                           (setf (getf (cdr def) :primary-key) nil))
                         definitions)
                   definitions)))))

(defun table-primary-keys (conn table-name)
  (mapcar #'(lambda (column) (getf column :|name|))
          (remove-if (lambda (column)
                       (= (getf column :|pk|) 0))
                     (table-info conn table-name))))

(defun table-indices (conn table-name)
  (let ((primary-keys (table-primary-keys conn table-name)))
    (with-prepared-query query (conn (format nil "PRAGMA index_list(\"~A\")" table-name))
      (append
       (loop with results = (dbi:execute query)
             for index = (dbi:fetch results :format :plist)
             while index
             collect
             (let* ((columns (mapcar
                              (lambda (info) (getf info :|name|))
                              (dbi:fetch-all
                               (dbi:execute (dbi:prepare conn (format nil "PRAGMA index_info(\"~A\")"
                                                                      (getf index :|name|))))
                               :format :plist)))
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
           nil)))))
