(in-package :cl-user)
(defpackage mito-test.dao
  (:use #:cl))
(in-package :mito-test.dao)

;;; separate packages to avoid conflicting defclasses for testing

(defpackage mito-test.dao.1
  (:use #:cl
        #:prove
        #:mito.dao
        #:mito.connection
        #:mito-test.util
        #:sxql)
  (:import-from #:alexandria
                #:make-keyword
                #:compose))
(in-package :mito-test.dao.1)

(plan nil)

(subtest "dao-table-class inheritance"

  (when (find-class 'tweet nil)
    (setf (find-class 'tweet) nil))

  (defclass tweet () ()
    (:metaclass dao-table-class))

  (ok (find (find-class 'dao-class) (c2mop:class-direct-superclasses (find-class 'tweet)))
      "dao-table-class inherits dao-table implicitly")

  (defclass my-dao-class (dao-class) ())

  (defclass tweet (my-dao-class) ()
    (:metaclass dao-table-class))

  (ok (not (find (find-class 'dao-class) (c2mop:class-direct-superclasses (find-class 'tweet))))
      "Not inherit dao-class directly")
  (ok (find (find-class 'my-dao-class) (c2mop:class-direct-superclasses (find-class 'tweet)))
      "Inherit my-dao-class")

  (is-table-class :mysql
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass dao-table-class)
                    (:record-timestamps nil))
                  "CREATE TABLE tweet (
    id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
    status TEXT NOT NULL,
    user INTEGER NOT NULL
)"
                  "auto-pk")

  (is-table-class :mysql
                  (defclass tweet ()
                    ((id :col-type :serial
                         :primary-key t)
                     (status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass dao-table-class)
                    (:record-timestamps nil))
                  "CREATE TABLE tweet (
    id INT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
    status TEXT NOT NULL,
    user INTEGER NOT NULL
)"
                  "add original PK")

  (is-table-class :mysql
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass dao-table-class)
                    (:record-timestamps nil))
                  "CREATE TABLE tweet (
    id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
    status TEXT NOT NULL,
    user INTEGER NOT NULL
)"
                  "redefinition w/o PK")

  (is-table-class :mysql
                  (defclass tweet ()
                    ((status :col-type :text)
                     (user :col-type :integer))
                    (:metaclass dao-table-class)
                    (:auto-pk nil)
                    (:record-timestamps nil))
                  "CREATE TABLE tweet (
    status TEXT NOT NULL,
    user INTEGER NOT NULL
)"
                  "auto-pk is nil"))


(defpackage mito-test.dao.2
  (:use #:cl
        #:prove
        #:mito.dao
        #:mito.connection
        #:mito-test.util
        #:sxql)
  (:import-from #:alexandria
                #:make-keyword
                #:compose))
(in-package :mito-test.dao.2)

(plan nil)

(subtest "relation"
  (setf *connection* (connect-to-testdb :mysql))
  (when (find-class 'user nil)
    (setf (find-class 'user) nil))
  (when (find-class 'user-setting nil)
    (setf (find-class 'user-setting) nil))
  (when (find-class 'tweet nil)
    (setf (find-class 'tweet) nil))

  (defclass user-setting ()
    ()
    (:metaclass dao-table-class))

  (defclass user ()
    ((id :col-type :serial
         :primary-key t)
     (name :col-type :text
           :initarg :name)
     (setting :col-type (or user-setting :null)
              :initarg :setting
              :accessor user-setting))
    (:metaclass dao-table-class)
    (:record-timestamps nil))

  (defclass tweet ()
    ((status :col-type :text
             :initarg :status
             :accessor tweet-status)
     (user :col-type user
           :initarg :user
           :accessor tweet-user))
    (:metaclass dao-table-class)
    (:record-timestamps nil))

  (is (mapcar #'sxql:yield (table-definition 'tweet))
      '("CREATE TABLE tweet (
    id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
    status TEXT NOT NULL,
    user_id INT UNSIGNED NOT NULL
)"))
  (mito:execute-sql "DROP TABLE IF EXISTS tweet")
  (mito:execute-sql "DROP TABLE IF EXISTS user")
  (mito:execute-sql "DROP TABLE IF EXISTS user_setting")
  (mito:ensure-table-exists 'user-setting)
  (mito:ensure-table-exists 'user)
  (mito:ensure-table-exists 'tweet)
  (let ((user (mito:create-dao 'user :name "Eitaro")))
    (mito:create-dao 'tweet :status "Hello" :user user))
  (let ((user (mito:create-dao 'user :name "Yoshimi")))
    (mito:create-dao 'tweet :status "こんにちは" :user user))

  (is (mito:count-dao 'tweet) 2)

  (let ((tweets (mito:select-dao 'tweet)))
    (is (length tweets) 2)
    (is-type (first tweets) 'tweet)
    (is-type (tweet-user (first tweets)) 'user)

    (diag "deleting the related foreign object")
    (dbi:with-transaction mito:*connection*
      (mito:delete-dao (tweet-user (first tweets)))
      (slot-makunbound (first tweets) 'user)
      ;; related foreign object is nil
      (is (tweet-user (first tweets)) nil)
      (slot-makunbound (first tweets) 'user)
      (setf (tweet-status (first tweets)) "Hello, World")

      ;; can update
      (mito:update-dao (first tweets))
      (isnt (slot-value (mito:find-dao 'tweet :id (mito:object-id (first tweets))) 'user-id) nil)
      (dbi:rollback mito:*connection*)))

  (ok (every (lambda (tweet)
               (not (slot-boundp tweet 'user)))
             (mito:select-dao 'tweet))
      "foreign slots are not loaded")
  (ok (every (lambda (tweet)
               (slot-boundp tweet 'user))
             (mito:select-dao 'tweet
               (mito:includes 'user)))
      "foreign slots are loaded eagerly with 'includes'.")

  (defclass friend-relationship ()
    ((user-a :col-type user
             :initarg :user-a)
     (user-b :col-type user
             :initarg :user-b))
    (:metaclass dao-table-class))
  (mito:execute-sql "DROP TABLE IF EXISTS friend_relationshiop")
  (mito:ensure-table-exists 'friend-relationship)
  (mito:create-dao 'friend-relationship
                   :user-a (mito:create-dao 'user :name "userA")
                   :user-b (mito:create-dao 'user :name "userB"))

  (ok (every (lambda (rel)
               (and (slot-boundp rel 'user-a)
                    (slot-boundp rel 'user-b)))
             (mito:select-dao 'friend-relationship
               (mito:includes 'user)))
      "foreign slots are loaded eagerly with 'includes'.")

  (let ((user (mito:find-dao 'user)))
    (ok user)
    (is-type (mito:find-dao 'tweet :user user)
             'tweet
             "Can find an object by a foreign object")
    (ok
     (mito.dao:select-dao 'tweet
       (where (:= :user user))))
    (ok
     (mito.dao:select-dao 'tweet
       (where (:in :user (list user))))))

  (is (user-setting (mito:find-dao 'user)) nil)

  (defclass tweet2 (tweet) ()
    (:metaclass dao-table-class)
    (:record-timestamps nil))
  (mito:execute-sql "DROP TABLE IF EXISTS tweet2")
  (mito:ensure-table-exists 'tweet2)

  (let ((user (mito:find-dao 'user :name "Eitaro")))
    (ok (mito:create-dao 'tweet2 :status "Hello" :user user)))

  (let ((tweet (mito:find-dao 'tweet :id 1)))
    (is (mito:count-dao 'tweet) 2)
    (ok tweet)
    (mito:delete-dao tweet)
    (is (mito:count-dao 'tweet) 1)
    (mito:delete-by-values 'tweet :id 2)
    (is (mito:count-dao 'tweet) 0))

  (disconnect-toplevel))

(defpackage mito-test.dao.3
  (:use #:cl
        #:prove
        #:mito.dao
        #:mito.connection
        #:mito-test.util
        #:sxql)
  (:import-from #:alexandria
                #:make-keyword
                #:compose))
(in-package :mito-test.dao.3)

(plan nil)

(subtest "foreign slots"
  (setf *connection* (connect-to-testdb :mysql))
  (defclass user ()
    ()
    (:metaclass dao-table-class))
  (defclass tweet ()
    ()
    (:metaclass dao-table-class))
  (defclass tweet-tag ()
    ((user :col-type user)
     (tweet :col-type tweet))
    (:metaclass dao-table-class)
    (:primary-key user tweet))
  (is (mapcar #'sxql:yield (table-definition 'tweet-tag))
      '("CREATE TABLE tweet_tag (
    user_id BIGINT UNSIGNED NOT NULL,
    tweet_id BIGINT UNSIGNED NOT NULL,
    created_at TIMESTAMP,
    updated_at TIMESTAMP,
    PRIMARY KEY (user_id, tweet_id)
)"))
  (defclass tweet-tag ()
    ((user :col-type user)
     (tweet :col-type tweet))
    (:metaclass dao-table-class)
    (:keys (user tweet)))
  (is (mapcar #'sxql:yield (table-definition 'tweet-tag))
      '("CREATE TABLE tweet_tag (
    id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
    user_id BIGINT UNSIGNED NOT NULL,
    tweet_id BIGINT UNSIGNED NOT NULL,
    created_at TIMESTAMP,
    updated_at TIMESTAMP,
    KEY (user_id, tweet_id)
)"))

  (disconnect-toplevel))

(defpackage mito-test.dao.4
  (:use #:cl
        #:prove
        #:mito.dao
        #:mito.connection
        #:mito-test.util
        #:sxql)
  (:import-from #:alexandria
                #:make-keyword
                #:compose))
(in-package :mito-test.dao.4)

(plan nil)

(dolist (driver '(:mysql :postgres :sqlite3))
  (subtest (format nil "inflate & deflate (~A)" driver)
    (setf *connection* (connect-to-testdb driver))
    (defclass user ()
      ((id :col-type :serial
           :primary-key t)
       (name :col-type :text
             :initarg :name)
       (is-admin :col-type :boolean
                 :initform nil
                 :initarg :is-admin)
       (role :col-type (:varchar 12)
             :initarg :role
             :deflate #'string-downcase
             :inflate (compose #'make-keyword #'string-upcase)))
      (:metaclass dao-table-class))
    (mito:execute-sql
     (sxql:drop-table :user :if-exists t))
    (mito:ensure-table-exists 'user)

    (let ((mito:*mito-logger-stream* t))
      (mito:create-dao 'user
                       :name "Admin User A"
                       :is-admin t
                       :role :manager)
      (mito:create-dao 'user
                       :name "User B"
                       :is-admin nil
                       :role :end-user)

      (let ((user (mito:find-dao 'user :id 1)))
        (is (slot-value user 'is-admin) t)
        (is-type (mito:object-created-at user) 'local-time:timestamp))
      (let ((user (mito:find-dao 'user :id 2)))
        (is (slot-value user 'is-admin) nil)
        (is-type (mito:object-created-at user) 'local-time:timestamp))
      (let ((user (mito:find-dao 'user :role :manager)))
        (ok user)))
    (disconnect-toplevel)))

(defpackage mito-test.dao.5
  (:use #:cl
        #:prove
        #:mito.dao
        #:mito.connection
        #:mito-test.util
        #:sxql)
  (:import-from #:alexandria
                #:make-keyword
                #:compose))
(in-package :mito-test.dao.5)

(plan nil)

(subtest "timestamp with milliseconds (PostgreSQL)"
  (setf *connection* (connect-to-testdb :postgres))
  (defclass user ()
    ((registered-at :col-type :timestamp))
    (:metaclass dao-table-class)
    (:record-timestamps nil))
  (mito:execute-sql
   (sxql:drop-table :user :if-exists t))
  (mito:ensure-table-exists 'user)

  (let ((now (local-time:now)))
    (mito:create-dao 'user :registered-at now)
    (let ((user (mito:find-dao 'user :id 1)))
      (is-type (slot-value user 'registered-at) 'local-time:timestamp)
      (ok (/= 0 (local-time:nsec-of (slot-value user 'registered-at))))))
  (disconnect-toplevel))

(defpackage mito-test.dao.6
  (:use #:cl
        #:prove
        #:mito.dao
        #:mito.connection
        #:mito-test.util
        #:sxql)
  (:import-from #:alexandria
                #:make-keyword
                #:compose))
(in-package :mito-test.dao.6)

(plan nil)

(defclass parent ()
  ()
  (:metaclass dao-table-class))

(defclass child ()
  ((parent :col-type parent
           :initarg :parent
           :accessor child-parent))
  (:metaclass dao-table-class))

(subtest "accessor"
  (setf *connection* (connect-to-testdb :postgres))
  (mito:execute-sql (sxql:drop-table :parent :if-exists t))
  (mito:execute-sql (sxql:drop-table :child :if-exists t))
  (mito:ensure-table-exists 'parent)
  (mito:ensure-table-exists 'child)
  (mito:create-dao 'child :parent (mito:create-dao 'parent))
  (child-parent (mito:find-dao 'child))
  (ok (object= (child-parent (mito:find-dao 'child))
               (mito:find-dao 'parent))))

(finalize)
