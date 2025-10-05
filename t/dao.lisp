(defpackage #:mito-test.dao
  (:use #:cl
        #:rove
        #:mito.dao
        #:mito.connection
        #:mito-test.util
        #:sxql)
  (:import-from #:alexandria
                #:make-keyword
                #:compose))
(in-package #:mito-test.dao)

(deftest dao-table-class-inheritance
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
                  "auto-pk is nil")

  (setf (find-class 'tweet) nil))

(deftest relation
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
           :initarg :name
           :accessor user-name)
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

  (ok (equal (mapcar #'sxql:yield (table-definition 'tweet))
             '("CREATE TABLE tweet (
    id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
    status TEXT NOT NULL,
    user_id INT UNSIGNED NOT NULL
)")))
  (mito:execute-sql "DROP TABLE IF EXISTS tweet")
  (mito:execute-sql "DROP TABLE IF EXISTS user")
  (mito:execute-sql "DROP TABLE IF EXISTS user_setting")
  (mito:ensure-table-exists 'user-setting)
  (mito:ensure-table-exists 'user)
  (mito:ensure-table-exists 'tweet)
  (let ((user (mito:create-dao 'user
                               :name "Eitaro")))
    (mito:create-dao 'tweet :status "Hello" :user user))
  (let ((user (mito:create-dao 'user
                               :name "Yoshimi"
                               :setting (mito:create-dao 'user-setting))))
    (mito:create-dao 'tweet :status "こんにちは" :user user))

  (ok (= (mito:count-dao 'tweet) 2))

  (dbi:with-transaction mito:*connection*
    (let ((tweet (first (mito:select-dao 'tweet (sxql:limit 1)))))
      (setf (tweet-status tweet) "Goodbye, World")
      (setf (tweet-user tweet) (mito:find-dao 'user :name "Yoshimi"))
      (mito:update-dao tweet :columns '(:status))
      (ok (equal (user-name (tweet-user (first (mito:select-dao 'tweet (sxql:limit 1)))))
                 "Eitaro"))
      (mito:update-dao tweet)
      (ok (equal (user-name (tweet-user (first (mito:select-dao 'tweet (sxql:limit 1)))))
                 "Yoshimi")))
    (dbi:rollback mito:*connection*))

  (let ((tweets (mito:select-dao 'tweet)))
    (ok (= (length tweets) 2))
    (ok (typep (first tweets) 'tweet))
    (ok (typep (tweet-user (first tweets)) 'user))

    (diag "deleting the related foreign object")
    (dbi:with-transaction mito:*connection*
      (mito:delete-dao (tweet-user (first tweets)))
      (slot-makunbound (first tweets) 'user)
      ;; related foreign object is nil
      (ok (null (tweet-user (first tweets))))
      (slot-makunbound (first tweets) 'user)
      (setf (tweet-status (first tweets)) "Hello, World")

      ;; can update
      (mito:update-dao (first tweets))
      (ok (slot-value (mito:find-dao 'tweet :id (mito:object-id (first tweets))) 'user-id))
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
    (ok (typep (mito:find-dao 'tweet :user user)
               'tweet)
        "Can find an object by a foreign object")
    (ok
     (mito.dao:select-dao 'tweet
       (where (:= :user user))))
    (ok
     (mito.dao:select-dao 'tweet
       (where (:in :user (list user))))))
  (testing "Can generate IS NULL query by find-dao"
    (ok (mito:find-dao 'user :name "Eitaro" :setting nil))
    (ok (eql 4 (mito:count-dao 'user)))
    (ok (eql 3 (mito:count-dao 'user :setting nil))))

  (ok (null (user-setting (mito:find-dao 'user :name "Eitaro"))))

  (defclass tweet2 (tweet) ()
    (:metaclass dao-table-class)
    (:record-timestamps nil))
  (mito:execute-sql "DROP TABLE IF EXISTS tweet2")
  (mito:ensure-table-exists 'tweet2)

  (let ((user (mito:find-dao 'user :name "Eitaro")))
    (ok (mito:create-dao 'tweet2 :status "Hello" :user user)))

  (let ((tweet (mito:find-dao 'tweet :id 1)))
    (ok (= (mito:count-dao 'tweet) 2))
    (ok tweet)
    (mito:delete-dao tweet)
    (ok (= (mito:count-dao 'tweet) 1))
    (mito:delete-by-values 'tweet :id 2)
    (ok (= (mito:count-dao 'tweet) 0)))

  (dolist (class-name '(user-setting user tweet friend-relationship tweet2))
    (setf (find-class class-name) nil))

  (disconnect-toplevel))

(deftest cursor
  (setf *connection* (connect-to-testdb :postgres))
  (when (find-class 'user nil)
    (setf (find-class 'user) nil))
  (defclass user ()
    ((name :col-type :text
           :initarg :name))
    (:metaclass dao-table-class))
  (mito:execute-sql "DROP TABLE IF EXISTS \"user\"")
  (mito:ensure-table-exists 'user)
  (mito:create-dao 'user :name "Eitaro")
  (mito:create-dao 'user :name "Btaro")
  (mito:create-dao 'user :name "Charlie")
  (dbi:with-transaction *connection*
    (let* ((mito.dao::*want-cursor* t)
           (cursor (mito.dao:select-dao 'user
                     (where (:like :name "%aro")))))
      (ok (typep cursor 'mito.dao::mito-cursor))
      (let ((row (mito.dao::fetch-dao-from-cursor cursor)))
        (ok (typep row 'user))
        (ok (equal (slot-value row 'name) "Eitaro")))
      (let ((row (mito.dao::fetch-dao-from-cursor cursor)))
        (ok (typep row 'user))
        (ok (equal (slot-value row 'name) "Btaro")))
      (ok (null (mito.dao::fetch-dao-from-cursor cursor)))))

  (let ((records '()))
    (do-select (user (mito.dao:select-dao 'user) i)
      (push (cons i user) records)
      (when (<= 1 i)
        (return)))
    (ok (= (length records) 2))
    (ok (every (lambda (record)
                 (typep (cdr record) 'user))
               records)))

  (when (find-class 'user nil)
    (setf (find-class 'user) nil))
  (disconnect-toplevel))

(deftest foreign-slots
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
  (ok (equal (mapcar #'sxql:yield (table-definition 'tweet-tag))
             '("CREATE TABLE tweet_tag (
    user_id BIGINT UNSIGNED NOT NULL,
    tweet_id BIGINT UNSIGNED NOT NULL,
    created_at TIMESTAMP,
    updated_at TIMESTAMP,
    PRIMARY KEY (user_id, tweet_id)
)")))
  (defclass tweet-tag ()
    ((user :col-type user)
     (tweet :col-type tweet))
    (:metaclass dao-table-class)
    (:keys (user tweet)))
  (ok (equal (mapcar #'sxql:yield (table-definition 'tweet-tag))
             '("CREATE TABLE tweet_tag (
    id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
    user_id BIGINT UNSIGNED NOT NULL,
    tweet_id BIGINT UNSIGNED NOT NULL,
    created_at TIMESTAMP,
    updated_at TIMESTAMP,
    KEY (user_id, tweet_id)
)")))

  (dolist (class-name '(user tweet tweet-tag))
    (setf (find-class class-name) nil))
  (disconnect-toplevel))

(deftest inflate-deflate
  (dolist (driver '(:mysql :postgres :sqlite3))
    (testing (format nil "inflate & deflate (~A)" driver)
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
          (ok (slot-value user 'is-admin))
          (ok (typep (mito:object-created-at user) 'local-time:timestamp)))
        (let ((user (mito:find-dao 'user :id 2)))
          (ok (null (slot-value user 'is-admin)))
          (ok (typep (mito:object-created-at user) 'local-time:timestamp)))
        (let ((user (mito:find-dao 'user :role :manager)))
          (ok user)))

      (setf (find-class 'user) nil)
      (disconnect-toplevel))))

(deftest timestamp-with-milliseconds
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
      (ok (typep (slot-value user 'registered-at) 'local-time:timestamp))
      (ok (/= 0 (local-time:nsec-of (slot-value user 'registered-at))))))
  (setf (find-class 'user) nil)
  (disconnect-toplevel))

(deftest accessor
  (defclass parent ()
    ()
    (:metaclass dao-table-class))

  (defclass child ()
    ((parent :col-type parent
             :initarg :parent
             :accessor child-parent))
    (:metaclass dao-table-class))

  (setf *connection* (connect-to-testdb :postgres))
  (mito:execute-sql (sxql:drop-table :parent :if-exists t))
  (mito:execute-sql (sxql:drop-table :child :if-exists t))
  (mito:ensure-table-exists 'parent)
  (mito:ensure-table-exists 'child)
  (mito:create-dao 'child :parent (mito:create-dao 'parent))
  (child-parent (mito:find-dao 'child))
  (ok (object= (child-parent (mito:find-dao 'child))
               (mito:find-dao 'parent)))

  (dolist (class-name '(parent child))
    (setf (find-class class-name) nil)))

(deftest joins
  (setf *connection* (connect-to-testdb :sqlite3))
  (when (find-class 'user nil)
    (setf (find-class 'user) nil))
  (when (find-class 'tweet nil)
    (setf (find-class 'tweet) nil))

  (defclass user ()
    ((name :col-type :text
           :initarg :name
           :accessor user-name)
     (status :col-type (:varchar 20)
             :initarg :status
             :accessor user-status))
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

  (mito:execute-sql "DROP TABLE IF EXISTS tweet")
  (mito:execute-sql "DROP TABLE IF EXISTS user")
  (mito:ensure-table-exists 'user)
  (mito:ensure-table-exists 'tweet)

  (let ((active-user (mito:create-dao 'user :name "Active User" :status "active"))
        (inactive-user (mito:create-dao 'user :name "Inactive User" :status "inactive")))
    (mito:create-dao 'tweet :status "Tweet from active" :user active-user)
    (mito:create-dao 'tweet :status "Another from active" :user active-user)
    (mito:create-dao 'tweet :status "Tweet from inactive" :user inactive-user))

  (testing "Basic INNER JOIN"
    (let ((tweets (mito:select-dao 'tweet
                    (mito:joins 'user)
                    (where (:= :user.status "active")))))
      (ok (= (length tweets) 2)
          "Returns tweets from active users only")
      (ok (every (lambda (tweet)
                   (not (slot-boundp tweet 'user)))
                 tweets)
          "Ghost slots are NOT populated by joins alone")))

  (testing "LEFT JOIN"
    (let ((tweets (mito:select-dao 'tweet
                    (mito:joins 'user :type :left)
                    (where (:= :user.status "active")))))
      (ok (= (length tweets) 2)
          "LEFT JOIN returns correct results")))

  (testing "Combining joins and includes"
    (let ((tweets (mito:select-dao 'tweet
                    (mito:joins 'user)
                    (mito:includes 'user)
                    (where (:= :user.status "active")))))
      (ok (= (length tweets) 2)
          "Returns tweets from active users")
      (ok (every (lambda (tweet)
                   (slot-boundp tweet 'user))
                 tweets)
          "Ghost slots ARE populated when using includes")
      (ok (every (lambda (tweet)
                   (equal (user-status (tweet-user tweet)) "active"))
                 tweets)
          "Loaded users have correct status")))

  (testing "Error on non-existent relationship"
    (defclass post ()
      ((title :col-type :text))
      (:metaclass dao-table-class))
    (ok (signals
         (mito:select-dao 'tweet
           (mito:joins 'post)))
        "Raises error when no relationship exists"))

  (dolist (class-name '(user tweet post))
    (setf (find-class class-name) nil))
  (disconnect-toplevel))
