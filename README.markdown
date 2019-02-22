# Mito

[![Build Status](https://travis-ci.org/fukamachi/mito.svg?branch=master)](https://travis-ci.org/fukamachi/mito)
[![Coverage Status](https://coveralls.io/repos/fukamachi/mito/badge.svg?branch=master&service=github)](https://coveralls.io/github/fukamachi/mito?branch=master)
[![Quicklisp dist](http://quickdocs.org/badge/mito.svg)](http://quickdocs.org/mito/)

Mito is yet another object relational mapper and it aims to be a successor of [Integral](https://github.com/fukamachi/integral).

* Supports MySQL, PostgreSQL and SQLite3
* Adds `id` (serial/uuid primary key), `created_at` and `updated_at` by default like Ruby's ActiveRecord
* Migrations
* DB schema versioning

## Warning

This software is still ALPHA quality. The APIs will be likely to change.

Should work fine with MySQL, PostgreSQL and SQLite3 on SBCL/Clozure CL.

## Usage

```common-lisp
(mito:connect-toplevel :mysql :database-name "myapp" :username "fukamachi" :password "c0mon-1isp")
;=> #<DBD.MYSQL:<DBD-MYSQL-CONNECTION> {100691BFF3}>

(mito:deftable user ()
  ((name :col-type (:varchar 64))
   (email :col-type (or (:varchar 128) :null))))
;=> #<MITO.DAO.TABLE:DAO-TABLE-CLASS COMMON-LISP-USER::USER>

(mito:table-definition 'user)
;=> (#<SXQL-STATEMENT: CREATE TABLE user (id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY, name VARCHAR(64) NOT NULL, email VARCHAR(128))>)

(mito:deftable tweet ()
  ((status :col-type :text)
   (user :col-type user)))
;=> #<MITO.DAO.TABLE:DAO-TABLE-CLASS COMMON-LISP-USER::TWEET>

(mito:table-definition 'tweet)
;=> (#<SXQL-STATEMENT: CREATE TABLE tweet (id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY, status TEXT NOT NULL, user_id BIGINT UNSIGNED NOT NULL, created_at TIMESTAMP, updated_at TIMESTAMP)>)
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

Use `connection-database-name` to get the name of the current
connection, or of the one given as parameter.

### deftable macro

As Mito's dao table class is defined as a CLOS metaclass, you can define a table class like this:

```common-lisp
(defclass user ()
  ((name :col-type (:varchar 64)
         :accessor user-name)
   (email :col-type (or (:varchar 128) :null)
          :accessor user-email))
  (:metaclass mito:dao-table-class))
```

It's quite clear how to use since its grammar is same as `cl:defclass`. However, the definition is a little bit redundant.

`mito:deftable` is a thin macro to define a table class with less typing.

For example, the above example can be rewritten like the following definition by using `deftable`.

```common-lisp
(mito:deftable user ()
  ((name :col-type (:varchar 64))
   (email :col-type (or (:varchar 128) :null))))
```

It adds `:metaclass mito:dao-table-class` and adds the default accessors which starts with `<class-name>-` by default like `defstruct`.

You can change the accessors prefix with `:conc-name` class option:

```common-lisp
(mito:deftable user ()
  ((name :col-type (:varchar 64))
   (email :col-type (or (:varchar 128) :null)))
  (:conc-name my-))

(my-name (make-instance 'user :name "fukamachi"))
;=> "fukamachi"
```

If `:conc-name` is NIL, it never adds the default accessors.

### Class Definitions

In Mito, you can define a class which corresponds to a database table by specifying `(:metaclass mito:dao-table-class)`.

```common-lisp
(defclass user ()
  ((name :col-type (:varchar 64)
         :accessor user-name)
   (email :col-type (or (:varchar 128) :null)
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
                  {:inflate inflation-function} |
                  {:deflate deflation-function} |
                  {:references {class-name | (class-name slot-name)}} |
                  {:ghost boolean}
col-type ::= { keyword |
              (keyword . args) |
              (or keyword :null) |
              (or :null keyword) }
class-option ::= {:primary-key symbol*} |
                 {:unique-keys {symbol | (symbol*)}*} |
                 {:keys {symbol | (symbol*)}*} |
                 {:table-name table-name} |
                 {:auto-pk auto-pk-mixin-class-name} |
                 {:record-timestamps boolean} |
                 {:conc-name conc-name}
auto-pk-mixin-class-name ::= {:serial | :uuid}
conc-name ::= {null | string-designator}
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
;=> (#<SXQL-STATEMENT: CREATE TABLE user (id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY, name VARCHAR(64) NOT NULL, email VARCHAR(128), created_at TIMESTAMP, updated_at TIMESTAMP)>)

(sxql:yield *)
;=> "CREATE TABLE user (id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY, name VARCHAR(64) NOT NULL, email VARCHAR(128), created_at TIMESTAMP, updated_at TIMESTAMP)"
;   NIL
```

### Creating DB tables

```common-lisp
(mapc #'mito:execute-sql (mito:table-definition 'user))

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

(mito:retrieve-dao 'user)
;=> (#<USER {10077C6073}>)

;; Updating
(setf (slot-value me 'name) "nitro_idiot")
;=> "nitro_idiot"

(mito:save-dao me)
;-> ;; UPDATE `user` SET `id` = ?, `name` = ?, `email` = ?, `created_at` = ?, `updated_at` = ? WHERE (`id` = ?) (2, "nitro_idiot", "e.arrows@gmail.com", "2016-02-04T19:56:11.408927Z", "2016-02-04T19:56:19.006020Z", 2) [0 rows] | MITO.DAO:UPDATE-DAO

;; Deleting
(mito:delete-dao me)
;-> ;; DELETE FROM `user` WHERE (`id` = ?) (1) [0 rows] | MITO.DAO:DELETE-DAO
(mito:delete-by-values 'user :id 1)
;-> ;; DELETE FROM `user` WHERE (`id` = ?) (1) [0 rows] | MITO.DAO:DELETE-DAO

;; Counting
(mito:count-dao 'user)
;-> 1
```

Use `select-dao` to build custom queries with sxql (examples below).

### Relationship

To define a relationship, use `:references` at the slot:

```common-lisp
(mito:deftable user ()
  ((name :col-type (:varchar 64))
   (email :col-type (or (:varchar 128) :null))))

(mito:deftable tweet ()
  ((status :col-type :text)
   ;; This slot refers to USER class
   (user-id :references (user id))))

;; The :col-type of USER-ID column is retrieved from the foreign class.
(table-definition (find-class 'tweet))
;=> (#<SXQL-STATEMENT: CREATE TABLE tweet (
;        id BIGSERIAL NOT NULL PRIMARY KEY,
;        status TEXT NOT NULL,
;        user_id BIGINT NOT NULL,
;        created_at TIMESTAMP,
;        updated_at TIMESTAMP
;    )>)
```

You can also specify another foreign class at `:col-type` for defining a relationship:

```common-lisp
(mito:deftable tweet ()
  ((status :col-type :text)
   ;; This slot refers to USER class
   (user :col-type user)))

(table-definition (find-class 'tweet))
;=> (#<SXQL-STATEMENT: CREATE TABLE tweet (
;        id BIGSERIAL NOT NULL PRIMARY KEY,
;        status TEXT NOT NULL,
;        user_id BIGINT NOT NULL,
;        created_at TIMESTAMP,
;        updated_at TIMESTAMP
;    )>)

;; You can specify :USER arg, instead of :USER-ID.
(defvar *user* (mito:create-dao 'user :name "Eitaro Fukamachi"))
(mito:create-dao 'tweet :user *user*)

(mito:find-dao 'tweet :user *user*)
```

The later example allows you to create/retrieve `TWEET` by a `USER` object, not a `USER-ID`.

Mito doesn't add foreign key constraints for refering tables since I'm not sure it's still handful while using with ORMs.

### Inflation/Deflation

Inflation/Deflation is a function to convert values between Mito and RDBMS.

```common-lisp
(mito:deftable user-report ()
  ((title :col-type (:varchar 100))
   (body :col-type :text
         :initform "")
   (reported-at :col-type :timestamp
                :initform (local-time:now)
                :inflate #'local-time:universal-to-timestamp
                :deflate #'local-time:timestamp-to-universal))
  (:conc-name report-))
```

### Eager loading

One of the pains in the neck to use ORMs is "N+1 query" problem.

```common-lisp
;; BAD EXAMPLE

(use-package '(:mito :sxql))

(defvar *tweets-contain-japan*
  (select-dao 'tweet
    (where (:like :status "%Japan%"))))

;; Getting names of tweeted users.
(mapcar (lambda (tweet)
          (user-name (tweet-user tweet)))
        *tweets-contain-japan*)
```

This example sends a query to retrieve a user like "SELECT * FROM user WHERE id = ?" for each iterations.

To prevent this performance issue, add `includes` to the above query which only sends a single WHERE IN query instead of N queries:

```common-lisp
;; GOOD EXAMPLE with eager loading

(use-package '(:mito :sxql))

(defvar *tweets-contain-japan*
  (select-dao 'tweet
    (includes 'user)
    (where (:like :status "%Japan%"))))
;-> ;; SELECT * FROM `tweet` WHERE (`status` LIKE ?) ("%Japan%") [3 row] | MITO.DB:RETRIEVE-BY-SQL
;-> ;; SELECT * FROM `user` WHERE (`id` IN (?, ?, ?)) (1, 3, 12) [3 row] | MITO.DB:RETRIEVE-BY-SQL
;=> (#<TWEET {1003513EC3}> #<TWEET {1007BABEF3}> #<TWEET {1007BB9D63}>)

;; No additional SQLs will be executed.
(tweet-user (first *))
;=> #<USER {100361E813}>
```

### Migrations

```common-lisp
(ensure-table-exists 'user)
;-> ;; CREATE TABLE IF NOT EXISTS "user" (
;       "id" BIGSERIAL NOT NULL PRIMARY KEY,
;       "name" VARCHAR(64) NOT NULL,
;       "email" VARCHAR(128),
;       "created_at" TIMESTAMP,
;       "updated_at" TIMESTAMP
;   ) () [0 rows] | MITO.DAO:ENSURE-TABLE-EXISTS

;; No changes
(mito:migration-expressions 'user)
;=> NIL

(mito:deftable user ()
  ((name :col-type (:varchar 64))
   (email :col-type (:varchar 128)))
  (:unique-keys email))

(mito:migration-expressions 'user)
;=> (#<SXQL-STATEMENT: ALTER TABLE user ALTER COLUMN email TYPE character varying(128), ALTER COLUMN email SET NOT NULL>
;    #<SXQL-STATEMENT: CREATE UNIQUE INDEX unique_user_email ON user (email)>)

(mito:migrate-table 'user)
;-> ;; ALTER TABLE "user" ALTER COLUMN "email" TYPE character varying(128), ALTER COLUMN "email" SET NOT NULL () [0 rows] | MITO.MIGRATION.TABLE:MIGRATE-TABLE
;   ;; CREATE UNIQUE INDEX "unique_user_email" ON "user" ("email") () [0 rows] | MITO.MIGRATION.TABLE:MIGRATE-TABLE
;-> (#<SXQL-STATEMENT: ALTER TABLE user ALTER COLUMN email TYPE character varying(128), ALTER COLUMN email SET NOT NULL>
;    #<SXQL-STATEMENT: CREATE UNIQUE INDEX unique_user_email ON user (email)>)
```

### Schema versioning

```
$ ros install mito
$ mito
Usage: mito command [option...]

Commands:
    generate-migrations
    migrate
    migration-status

Options:
    -t, --type DRIVER-TYPE          DBI driver type (one of "mysql", "postgres" or "sqlite3")
    -d, --database DATABASE-NAME    Database name to use
    -u, --username USERNAME         Username for RDBMS
    -p, --password PASSWORD         Password for RDBMS
    -s, --system SYSTEM             ASDF system to load (several -s's allowed)
    -D, --directory DIRECTORY       Directory path to keep migration SQL files (default: "/Users/nitro_idiot/Programs/lib/mito/db/")
    --dry-run                       List SQL expressions to migrate
    -f, --force                     Create a new empty migration file even when it's unnecessary.
```

#### Example

```
mito --database postgres --username fukamachi --pasword c0mmon-l1sp
```

### Inheritance and Mixin

A subclass of DAO-CLASS is allowed to be inherited. This may be useful when you need classes which have similar columns:

```common-lisp
(mito:deftable user ()
  ((name :col-type (:varchar 64))
   (email :col-type (:varchar 128)))
  (:unique-keys email))

(mito:deftable temporary-user (user)
  ((registered-at :col-type :timestamp)))

(mito:table-definition 'temporary-user)
;=> (#<SXQL-STATEMENT: CREATE TABLE temporary_user (
;        id BIGSERIAL NOT NULL PRIMARY KEY,
;        name VARCHAR(64) NOT NULL,
;        email VARCHAR(128) NOT NULL,
;        registered_at TIMESTAMP NOT NULL,
;        created_at TIMESTAMP,
;        updated_at TIMESTAMP,
;        UNIQUE (email)
;    )>)
```

If you need a 'template' for tables which doesn't related to any database tables, you can use `DAO-TABLE-MIXIN`:

```common-lisp
(defclass has-email ()
  ((email :col-type (:varchar 128)
          :accessor object-email))
  (:metaclass mito:dao-table-mixin)
  (:unique-keys email))
;=> #<MITO.DAO.MIXIN:DAO-TABLE-MIXIN COMMON-LISP-USER::HAS-EMAIL>

(mito:deftable user (has-email)
  ((name :col-type (:varchar 64))))
;=> #<MITO.DAO.TABLE:DAO-TABLE-CLASS COMMON-LISP-USER::USER>

(mito:table-definition 'user)
;=> (#<SXQL-STATEMENT: CREATE TABLE user (
;       id BIGSERIAL NOT NULL PRIMARY KEY,
;       name VARCHAR(64) NOT NULL,
;       email VARCHAR(128) NOT NULL,
;       created_at TIMESTAMP,
;       updated_at TIMESTAMP,
;       UNIQUE (email)
;   )>)
```

* [mito-attachment](https://github.com/fukamachi/mito-attachment)
* [mito-auth](https://github.com/fukamachi/mito-auth)

### Triggers

Since `insert-dao`, `update-dao` and `delete-dao` are defined as generic functions, you can define `:before`, `:after` or `:around` methods to those.

```common-lisp
(defmethod mito:insert-dao :before ((object user))
  (format t "~&Adding ~S...~%" (user-name object)))

(mito:create-dao 'user :name "Eitaro Fukamachi" :email "e.arrows@gmail.com")
;-> Adding "Eitaro Fukamachi"...
;   ;; INSERT INTO "user" ("name", "email", "created_at", "updated_at") VALUES (?, ?, ?, ?) ("Eitaro Fukamachi", "e.arrows@gmail.com", "2016-02-16 21:13:47", "2016-02-16 21:13:47") [0 rows] | MITO.DAO:INSERT-DAO
;=> #<USER {100835FB33}>
```

## Installation

```common-lisp
(ql:quickload :mito)
```

Or, with Roswell:

```
ros install mito
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
