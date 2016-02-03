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
;=> #<SXQL-STATEMENT: CREATE TABLE tweet (id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY, status TEXT NOT NULL, user_id BIGINT UNSIGNED NOT NULL)>
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
class-option ::= {:primary-keys symbol*} |
                 {:unique-keys {symbol | (symbol*)}*} |
                 {:keys {symbol | (symbol*)}*} |
                 {:table-name table-name}
                 {:auto-pk boolean}
```

Note the class automatically adds a primary key named `id` if there's no primary keys.

```common-lisp
(c2mop:class-direct-slots (find-class 'user))
;=> (#<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS MITO.DAO.TABLE::%SYNCED>
;    #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS MITO.DAO.TABLE::ID>
;    #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS COMMON-LISP-USER::NAME>
;    #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS COMMON-LISP-USER::EMAIL>)
```

To prevent this behavior, add `:auto-pk nil` to the class options.

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
;=> #<SXQL-STATEMENT: CREATE TABLE user (id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY, name VARCHAR(64) NOT NULL, email VARCHAR(128))>

(sxql:yield *)
;=> "CREATE TABLE user (id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY, name VARCHAR(64) NOT NULL, email VARCHAR(128))"
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
;-> ;; INSERT INTO `user` (`name`, `email`) VALUES (?, ?) ("Eitaro Fukamachi", "e.arrows@gmail.com") [0 rows] | MITO.DAO:INSERT-DAO
;=> #<USER {10053C4453}>

;; Same as above
(mito:create-dao 'user :name "Eitaro Fukamachi" :email "e.arrows@gmail.com")

;; Getting the primary key value
(mito:object-id me)
;=> 1

;; Retrieving from the DB
(mito:find-dao 'user 1)
;-> ;; SELECT * FROM `user` WHERE (`id` = ?) LIMIT 1 (1) [1 row] | MITO.DB:RETRIEVE-BY-SQL
;=> #<USER {10077C6073}>

;; Updating
(setf (slot-value me 'name) "nitro_idiot")
;=> "nitro_idiot"

(mito:save-dao me)
;-> ;; UPDATE `user` SET `id` = ?, `name` = ?, `email` = ? WHERE (`id` = ?) (1, "nitro_idiot", "e.arrows@gmail.com", 2) [0 rows] | MITO.DB:EXECUTE-SQL

;; Deleting
(mito:delete-dao me)
;-> ;; DELETE FROM `user` WHERE (`id` = ?) (1) [0 rows] | MITO.DAO:DELETE-DAO
```

### Relation

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
