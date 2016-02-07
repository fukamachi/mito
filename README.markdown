# Mito

[![Build Status](https://travis-ci.org/fukamachi/mito.svg?branch=master)](https://travis-ci.org/fukamachi/mito)
[![Coverage Status](https://coveralls.io/repos/fukamachi/mito/badge.svg?branch=master&service=github)](https://coveralls.io/github/fukamachi/mito?branch=master)
[![Quicklisp dist](http://quickdocs.org/badge/mito.svg)](http://quickdocs.org/mito/)

Mito is yet another object relational mapper and it aims to be a successor of [Integral](https://github.com/fukamachi/integral).

* Supports MySQL, PostgreSQL and SQLite3
* Migrations
* DB schema versioning

## Warning

This software is still ALPHA quality. The APIs will be likely to change.

Should work fine with MySQL, PostgreSQL and SQLite3 on SBCL/Clozure CL.

## Usage

```common-lisp
(mito:connect-toplevel :mysql :database-name "myapp" :username "fukamachi" :password "c0mon-1isp")
;=> #<DBD.MYSQL:<DBD-MYSQL-CONNECTION> {100691BFF3}>

(defclass user ()
  ((name :col-type (:varchar 64)
         :initarg :name
         :accessor user-name)
   (email :col-type (or (:varchar 128) :null)
          :initarg :email
          :accessor user-email))
  (:metaclass mito:dao-table-class))
;=> #<MITO.DAO.TABLE:DAO-TABLE-CLASS COMMON-LISP-USER::USER>

(mito:table-definition 'user)
;=> #<SXQL-STATEMENT: CREATE TABLE user (id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY, name VARCHAR(64) NOT NULL, email VARCHAR(128))>

(defclass tweet ()
  ((status :col-type :text
           :initarg :status
           :accessor tweet-status)
   (user :col-type user
         :initarg :user
         :accessor tweet-user))
  (:metaclass mito:dao-table-class))
;=> #<MITO.DAO.TABLE:DAO-TABLE-CLASS COMMON-LISP-USER::TWEET>

(mito:table-definition 'tweet)
;=> #<SXQL-STATEMENT: CREATE TABLE tweet (id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY, status TEXT NOT NULL, user_id BIGINT UNSIGNED NOT NULL, created_at TIMESTAMP, updated_at TIMESTAMP)>
```

### Connecting to DB

Mito provides a function `connect-toplevel` and `disconnect-toplevel` to establish a connection to RDBMS.

`connect-toplevel` takes the same arguments as `dbi:connect`, typically the driver-type, the database name to connect, user name and password.

```common-lisp
(mito:connect-toplevel :mysql :database-name "myapp" :username "fukamachi" :password "c0mon-1isp")
```

`connect-toplevel` sets `*connection*` the new connection and returns it.

If you want to use a connection lexically, just bind it:

```common-lisp
(let ((mito:*connection* (dbi:connect :sqlite3 :database-name #P"/tmp/myapp.db")))
  (unwind-protect (progn ...)
    ;; Ensure that the connection is closed.
    (dbi:disconnect mito:*connection*)))
```

### Class Definitions

In Mito, you can define a class which corresponds to a database table by specifying `(:metaclass mito:dao-table-class)`.

```common-lisp
(defclass user ()
  ((name :col-type (:varchar 64)
         :initarg :name
         :accessor user-name)
   (email :col-type (or (:varchar 128) :null)
          :initarg :email
          :accessor user-email))
  (:metaclass mito:dao-table-class))
```

The above defines a Common Lisp normal class except that it allows additional options.

```
(defclass {class-name} ()
  ({column-definition}*)
  (:metaclass mito:dao-table-class)
  [[class-option]])

column-definition ::= (slot-name [[column-option]])
column-option ::= {:col-type col-type} |
                  {:primary-key boolean} |
                  {:ghost boolean}
col-type ::= { keyword |
              (keyword . args) |
              (or keyword :null) |
              (or :null keyword) }
class-option ::= {:primary-key symbol*} |
                 {:unique-keys {symbol | (symbol*)}*} |
                 {:keys {symbol | (symbol*)}*} |
                 {:table-name table-name}
                 {:auto-pk boolean}
                 {:record-timestamps boolean}
```

Note the class automatically adds some slots -- a primary key named `id` if there's no primary keys, `created_at` and `updated_at` for recording timestamps. To disable these behaviors, specify `:auto-pk nil` or `:record-timestamps nil` to defclass forms.

```common-lisp
(mito.class:table-column-slots (find-class 'user))
;=> (#<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS MITO.DAO.MIXIN::ID>
;    #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS COMMON-LISP-USER::NAME>
;    #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS COMMON-LISP-USER::EMAIL>
;    #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS MITO.DAO.MIXIN::CREATED-AT>
;    #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS MITO.DAO.MIXIN::UPDATED-AT>)
```

The class inherits `mito:dao-class` implicitly.

```common-lisp
(find-class 'user)
;=> #<MITO.DAO.TABLE:DAO-TABLE-CLASS COMMON-LISP-USER::USER>

(c2mop:class-direct-superclasses *)
;=> (#<STANDARD-CLASS MITO.DAO.TABLE:DAO-CLASS>)
```

This may be useful when you define methods which can be applied for all table classes.

### Generating Table Definitions

```common-lisp
(mito:table-definition 'user)
;=> #<SXQL-STATEMENT: CREATE TABLE user (id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY, name VARCHAR(64) NOT NULL, email VARCHAR(128), created_at TIMESTAMP, updated_at TIMESTAMP)>

(sxql:yield *)
;=> "CREATE TABLE user (id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY, name VARCHAR(64) NOT NULL, email VARCHAR(128), created_at TIMESTAMP, updated_at TIMESTAMP)"
;   NIL
```

### Creating DB tables

```common-lisp
(mito:execute-sql (mito:table-definition 'user))

(mito:ensure-table-exists 'user)
```

### CRUD

```common-lisp
(defvar me
  (make-instance 'user :name "Eitaro Fukamachi" :email "e.arrows@gmail.com"))
;=> USER

(mito:insert-dao me)
;-> ;; INSERT INTO `user` (`name`, `email`, `created_at`, `updated_at`) VALUES (?, ?, ?, ?) ("Eitaro Fukamachi", "e.arrows@gmail.com", "2016-02-04T19:55:16.365543Z", "2016-02-04T19:55:16.365543Z") [0 rows] | MITO.DAO:INSERT-DAO
;=> #<USER {10053C4453}>

;; Same as above
(mito:create-dao 'user :name "Eitaro Fukamachi" :email "e.arrows@gmail.com")

;; Getting the primary key value
(mito:object-id me)
;=> 1

;; Retrieving from the DB
(mito:find-dao 'user :id 1)
;-> ;; SELECT * FROM `user` WHERE (`id` = ?) LIMIT 1 (1) [1 row] | MITO.DB:RETRIEVE-BY-SQL
;=> #<USER {10077C6073}>

;; Updating
(setf (slot-value me 'name) "nitro_idiot")
;=> "nitro_idiot"

(mito:save-dao me)
;-> ;; UPDATE `user` SET `id` = ?, `name` = ?, `email` = ?, `created_at` = ?, `updated_at` = ? WHERE (`id` = ?) (2, "nitro_idiot", "e.arrows@gmail.com", "2016-02-04T19:56:11.408927Z", "2016-02-04T19:56:19.006020Z", 2) [0 rows] | MITO.DAO:UPDATE-DAO

;; Deleting
(mito:delete-dao me)
;-> ;; DELETE FROM `user` WHERE (`id` = ?) (1) [0 rows] | MITO.DAO:DELETE-DAO
```

### Relation

### Inflation/Deflation

### Eager loading

```common-lisp
(use-package '(:mito :sxql))

(select-dao 'tweet
  (includes 'user)
  (where (:= :id 1)))
;-> ;; SELECT * FROM `tweet` WHERE (`id` = ?) (1) [1 row] | MITO.DB:RETRIEVE-BY-SQL
;-> ;; SELECT * FROM `user` WHERE (`id` IN (?)) (1) [1 row] | MITO.DB:RETRIEVE-BY-SQL
;=> (#<TWEET {1003513EC3}>)

;; No additional SQLs aren't executed.
(tweet-user (first *))
;=> #<USER {100361E813}>
```

### Migrations

## Installation

```
$ mkdir -p ~/common-lisp
$ cd ~/common-lisp
$ git clone https://github.com/fukamachi/mito
$ ros -L ~/common-lisp/mito/mito.asd install mito
```

```common-lisp
(ql:quickload :mito)
```

## See Also

* [CL-DBI](https://github.com/fukamachi/cl-dbi)
* [SxQL](https://github.com/fukamachi/sxql)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the LLGPL License.
