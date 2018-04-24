(in-package :cl-user)
(defpackage mito-test.mixin
  (:use #:cl
        #:prove
        #:mito-test.util
        #:mito.connection
        #:mito.class
        #:mito.dao.mixin
        #:mito.dao.view
        #:mito.dao.table))
(in-package :mito-test.mixin)

(plan 2)

(subtest "record-timestamps"
  (setf *connection* (connect-to-testdb :mysql))
  (when (find-class 'tweet nil)
    (setf (find-class 'tweet) nil))

  (defclass tweet (record-timestamps-mixin)
    ((status :col-type (:varchar 140)
             :initarg :status
             :accessor tweet-status))
    (:metaclass dao-table-class))

  (is (mapcar #'sxql:yield (table-definition 'tweet))
      '("CREATE TABLE tweet (
    id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
    status VARCHAR(140) NOT NULL,
    created_at TIMESTAMP,
    updated_at TIMESTAMP
)"))

  (mito:execute-sql "DROP TABLE IF EXISTS tweet")
  (mito:ensure-table-exists 'tweet)

  (let ((obj (mito:create-dao 'tweet :status "Hi")))
    (is (object-id obj) 1)
    (let ((obj (mito:find-dao 'tweet :id 1)))
      (is-type (object-created-at obj) 'local-time:timestamp)
      (is-type (object-updated-at obj) 'local-time:timestamp)
      (is (object-created-at obj)
          (object-updated-at obj)
          :test #'local-time:timestamp=)
      (ok (<= (local-time:timestamp-to-universal (object-created-at obj))
              (+ (local-time:timestamp-to-universal (local-time:now)) 1)))
      (setf (tweet-status obj) "Hi, again")
      (sleep 2)
      (mito:save-dao obj))
    (let ((obj (mito:find-dao 'tweet :id 1)))
      (ok (local-time:timestamp/= (object-created-at obj)
                                  (object-updated-at obj)))))

  (disconnect-toplevel))

(subtest "custom mixin"
  (setf *connection* (connect-to-testdb :mysql))
  (when (find-class 'tweet nil)
    (setf (find-class 'tweet) nil))

  (defclass user ()
    ()
    (:metaclass dao-table-class))
  (defclass base-tweet ()
    ((user :col-type user
           :initarg :user
           :accessor tweet-user)
     (status :col-type (:varchar 140)
             :initarg :status
             :accessor tweet-status))
    (:metaclass dao-table-mixin))
  (defclass tweet (base-tweet)
    ()
    (:metaclass dao-table-class))

  (is (mapcar #'sxql:yield (table-definition 'tweet))
      '("CREATE TABLE tweet (
    id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
    user_id BIGINT UNSIGNED NOT NULL,
    status VARCHAR(140) NOT NULL,
    created_at TIMESTAMP,
    updated_at TIMESTAMP
)"))

  (disconnect-toplevel))

(finalize)
