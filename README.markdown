# Mito

[![Build Status](https://github.com/fukamachi/mito/workflows/CI/badge.svg)](https://github.com/fukamachi/mito/actions?query=workflow%3ACI)
[![Quicklisp dist](http://quickdocs.org/badge/mito.svg)](http://quickdocs.org/mito/)

Mito is yet another object relational mapper, and it aims to be a successor of [Integral](https://github.com/fukamachi/integral).

* Supports MySQL, PostgreSQL and SQLite3
* Adds `id` (serial/uuid primary key), `created_at` and `updated_at` by default like Ruby's ActiveRecord
* Migrations
* DB schema versioning

## Warning

This software is still ALPHA quality. The APIs likely change.

This software should work fine with MySQL, PostgreSQL and SQLite3 on SBCL/Clozure CL.

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

Mito provides the functions `connect-toplevel` and `disconnect-toplevel` to establish and sever a connection to RDBMS.

`connect-toplevel` takes the same arguments as `dbi:connect`: typically the driver-type, the database name to connect, user name and password.

```common-lisp
(mito:connect-toplevel :mysql :database-name "myapp" :username "fukamachi" :password "c0mon-1isp")
```

`connect-toplevel` sets `*connection*` to a new connection and returns it.

To use a connection lexically, just bind it:

```common-lisp
(let ((mito:*connection* (dbi:connect :sqlite3 :database-name #P"/tmp/myapp.db")))
  (unwind-protect (progn ...)
    ;; Ensure that the connection is closed.
    (dbi:disconnect mito:*connection*)))
```

In most cases `dbi:connect-cached` is a better option, since it reuses a connection for multiple threads.

```common-lisp
(let ((mito:*connection* (dbi:connect-cached :sqlite3 :database-name #P"/tmp/myapp.db")))
  (unwind-protect (progn ...)
    ;; Ensure that the connection is closed.
    ))
```

Use `connection-database-name` to get the name of the current connection, or of one named via parameter.

If you are using [clack](https://github.com/fukamachi/clack) as your webserver, A middleware is provided.

```common-lisp
(clack:clackup
  (lack:builder
    (:mito '(:sqlite3 :database-name #P"/tmp/myapp.db"))
    ...
    *app*))
```

#### Connecting To `sqlite3` In Memory

To connect to a `sqlite3` in memory database without having to save a file you can do:

```common-lisp
(mito:connect-toplevel :sqlite3
                       :database-name #P":memory:")
```

### deftable macro

As Mito's dao table class is defined as a CLOS metaclass, a table class can be defined like this:

```common-lisp
(defclass user ()
  ((name :col-type (:varchar 64)
         :accessor user-name)
   (email :col-type (or (:varchar 128) :null)
          :accessor user-email))
  (:metaclass mito:dao-table-class))
```

`deftable`'s syntax is the same as that of `cl:defclass`. However, the definition is a little bit redundant.

`mito:deftable` is a thin macro, to allow definion of a table class with less typing.

For example, the above example can be rewritten, using `deftable` as follows.

```common-lisp
(mito:deftable user ()
  ((name :col-type (:varchar 64))
   (email :col-type (or (:varchar 128) :null))))
```

It adds `:metaclass mito:dao-table-class`, and adds default accessors that start with `<class-name>-` by default, like `defstruct` does.

The prefix for accessors can be changed with the `:conc-name` class option:

```common-lisp
(mito:deftable user ()
  ((name :col-type (:varchar 64))
   (email :col-type (or (:varchar 128) :null)))
  (:conc-name my-))

(my-name (make-instance 'user :name "fukamachi"))
;=> "fukamachi"
```

If `:conc-name` is NIL, default accessors will NOT be defined.

### Class Definitions

In Mito, a class corresponding to a database table is defined by specifying `(:metaclass mito:dao-table-class)`.

```common-lisp
(defclass user ()
  ((name :col-type (:varchar 64)
         :accessor user-name)
   (email :col-type (or (:varchar 128) :null)
          :accessor user-email))
  (:metaclass mito:dao-table-class))
```

The above defines a Common Lisp normal class, except that it allows additional options.

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

Note: the class automatically adds some slots -- a primary key named `id` if there is no primary key, `created_at` and `updated_at` for recording timestamps. To disable these behaviors, specify `:auto-pk nil` or `:record-timestamps nil` to defclass forms.

```common-lisp
(mito.class:table-column-slots (find-class 'user))
;=> (#<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS MITO.DAO.MIXIN::ID>
;    #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS COMMON-LISP-USER::NAME>
;    #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS COMMON-LISP-USER::EMAIL>
;    #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS MITO.DAO.MIXIN::CREATED-AT>
;    #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS MITO.DAO.MIXIN::UPDATED-AT>)
```

This class inherits `mito:dao-class` implicitly.

```common-lisp
(find-class 'user)
;=> #<MITO.DAO.TABLE:DAO-TABLE-CLASS COMMON-LISP-USER::USER>

(c2mop:class-direct-superclasses *)
;=> (#<STANDARD-CLASS MITO.DAO.TABLE:DAO-CLASS>)
```

This may be useful to define methods that can be applied for many or all table classes.

#### :col-type Options

The following are valid keywords for :col-type in the `deftable` definition above.

```common-lisp
:serial
:bigserial
:timestamptz
:integer
:bytea
:timestamp
:bigint
:unsigned
:int
:binary
:datetime
```

Besides the above keywords, there are other keywords that are valid, however they are dependent on the RDS and its version.

An example of this is that `:json` and `:jsonb` work for PostgreSQL but don't work on an old version of MySQL which doesn't support those types.

A complete list of valid `:col-type` options is dependent on the database system. Here's a link for the current Data Types for:
- [PostgreSQL Data Types](https://www.postgresql.org/docs/current/datatype.html#DATATYPE-TABLE)
- [MySQL Data Types](https://dev.mysql.com/doc/refman/8.0/en/data-types.html)
- [SQLite3 Data Types](https://www.sqlite.org/datatype3.html)

The symbols are not defined directly in the system, rather they are the symbol equivalent of the string which is the name for the data type. Therefore, for any data type name, just preprend a colon to the name `:data-type` in order to use it as a `col-type`.

##### :col-type Definitions with Qualifiers

For some data types there are qualifiers available.

When there is only **one** qualfier in the data type, it can be given like in the following example

```lisp
(name :col-type (:varchar 64))
```

However, when there is **more than one** qualifier, providing a list of qualifiers **does not currently work**.

A **workaround** that works is giving the whole data type definition, including the qualifier, as a string.

For example the following will work:

```lisp
(price :col-type "numeric(10,2)")
```

However note that the following examples will **not work**:

```lisp
(price :col-type (:numeric "10,2"))
(price :col-type (:numeric 10 2))
```

Common Lisp does not accept parenthesis and commas as valid variable names, so `:numeric(10,2)` and `:numeric10,2` are obviously invalid.

Keep this in mind in particular when using `NUMERIC`, `DECIMAL`, and spatial data types.

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

### Custom queries
Mito is at its core a rather thin wrapper around sxql and cl-dbi for converting sql results to special types and back. Most of the porcelain functions shown above are acutally implemented in just under 200 lines.

Given a plist which represents the result from the database, you can apply `make-dao-instance` To make it into a `dao-class` automatically doing inflation/deflation.

To run a custom query, use `retrieve-by-sql` which returns a list of plists.
```common-lisp
(mito:retrieve-by-sql
  (select (:user.*)
    (from :users)
    ;; Using a subquery to avoid a join and distinct
    ;; Make sure you actually test performance before doing this in production
    (where (:in :user.name
                (select (:poster)
                  (from :tweets)
                  (where (:> :tweets.likes 1000))
                  (returning :poster))))))
;=> ((:name "Shinmera" :email "shinmera@tymoon.eu" :followers 200000)
;    (:name "Fukamachi" :email "e.arrows@gmail.com" :followers 100000) ...)
```

You can use `select-by-sql` if you want to automatically convert it to a class.

```common-lisp
(mito:select-by-sql 'user
  (select (:user.*)
    (from :users)
    (where (:in :user.name
                (select (:poster)
                  (from :tweets)
                  (where (:> :tweets.likes 1000))
                  (returning :poster))))))
;=> (#<USER {1003E769E3}> #<USER {10040637A3}>)
```
The actual definition is basically `mapcar #'make-dao-instance` over the results of `retrieve-by-sql`

Finally `select-dao` provides the highest level API. This is usually what you need.
```common-lisp
(mito:select-dao 'user
  (where (:in :user.name
              (select (:poster)
                (from :tweets)
                (where (:> :tweets.likes 1000))
                (returning :poster)))))
;=> (#<USER {1003E769E3}> #<USER {10040637A3}>)
```

It also provides neat facilities such as an `includes` clause so that you don't have to write out joins by hand (examples below).

### Relationship

To define a relationship, use `:references` on the slot:

```common-lisp
(mito:deftable user ()
  ((id :col-type (:varchar 36)
       :primary-key t)
   (name :col-type (:varchar 64))
   (email :col-type (or (:varchar 128) :null))))

(mito:deftable tweet ()
  ((status :col-type :text)
   ;; This slot refers to USER class
   (user-id :references (user id))))

;; The :col-type of USER-ID column is retrieved from the foreign class.
(mito:table-definition (find-class 'tweet))
;=> (#<SXQL-STATEMENT: CREATE TABLE tweet (
;       id BIGSERIAL NOT NULL PRIMARY KEY,
;       status TEXT NOT NULL,
;       user_id VARCHAR(36) NOT NULL,
;       created_at TIMESTAMPTZ,
;       updated_at TIMESTAMPTZ
;   )>)
```

You can also specify another foreign class at `:col-type` to define a relationship:

```common-lisp
(mito:deftable tweet ()
  ((status :col-type :text)
   ;; This slot refers to USER class
   (user :col-type user)))

(mito:table-definition (find-class 'tweet))
;=> (#<SXQL-STATEMENT: CREATE TABLE tweet (
;        id BIGSERIAL NOT NULL PRIMARY KEY,
;        status TEXT NOT NULL,
;        user_id VARCHAR(36) NOT NULL,
;        created_at TIMESTAMP,
;        updated_at TIMESTAMP
;    )>)

;; You can specify :USER arg, instead of :USER-ID.
(defvar *user* (mito:create-dao 'user :name "Eitaro Fukamachi"))
(mito:create-dao 'tweet :user *user*)

(mito:find-dao 'tweet :user *user*)
```

The latter example allows you to create/retrieve `TWEET` by a `USER` object, not a `USER-ID`.

Mito doesn't add foreign key constraints for referring tables, since I'm not sure it's still handful while using with ORMs.

#### Defining One-to-Many and Many-to-Many Relationship Accessors

While one-to-one relationships automatically get accessors via `:col-type`, there's no automatic way to define accessors for retrieving collections of related objects (one-to-many or many-to-many relationships). For this, Mito provides the `define-accessor` macro.

```common-lisp
(mito:deftable user ()
  ((name :col-type (:varchar 64))))

(mito:deftable tweet ()
  ((status :col-type :text)
   (user :col-type user)))  ; one-to-one, automatic accessor

;; Define accessor for one-to-many relationship (user -> tweets)
(define-accessor user-tweets (user user)
  (mito:select-dao 'tweet
    (sxql:where (:= :user_id (mito:object-id user)))))

;; Usage
(defvar *user* (mito:create-dao 'user :name "Eitaro Fukamachi"))
(defvar *tweet1* (mito:create-dao 'tweet :status "Hello" :user *user*))
(defvar *tweet2* (mito:create-dao 'tweet :status "World" :user *user*))

(user-tweets *user*)  ; Returns list of tweets for this user
;=> (#<TWEET {100234567}> #<TWEET {100234568}>)

;; The result is cached - subsequent calls don't hit the database
(user-tweets *user*)  ; Returns cached result

;; You can also set the value directly if needed
(setf (user-tweets *user*) (list *tweet1*))
```

The `define-accessor` macro creates cached accessor functions that:
- Execute the query only on first access
- Cache results to avoid repeated database hits

This is particularly useful for:
- **One-to-many relationships**: Getting all children of a parent object
- **Many-to-many relationships**: Complex queries through junction tables
- **Reverse relationships**: Accessing the "other direction" of a relationship
- **Filtered relationships**: Adding WHERE clauses, ordering, etc.

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

One of the pains in the neck to use ORMs is the "N+1 query" problem.

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

This example sends a query to retrieve a user, like "SELECT * FROM user WHERE id = ?" for each iteration.

To prevent this performance issue, add `includes` to the above query, which sends only a single WHERE IN query instead of N queries:

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

#### Filtering with JOINs

While `includes` helps with eager loading, you may want to filter results based on related table columns using SQL JOIN clauses. Mito provides a `joins` function for this:

```common-lisp
;; Filter tweets by active users only (using INNER JOIN)
(select-dao 'tweet
  (joins 'user)
  (where (:= :user.status "active")))
;-> ;; SELECT tweet.* FROM tweet INNER JOIN user ON tweet.user_id = user.id WHERE (user.status = ?) ("active")

;; Use LEFT JOIN to include tweets without users
(select-dao 'tweet
  (joins 'user :type :left)
  (where (:is-null :user.id)))
;-> ;; SELECT tweet.* FROM tweet LEFT JOIN user ON tweet.user_id = user.id WHERE (user.id IS NULL)
```

**Important:** `joins` only adds SQL JOIN clauses for filtering—it does NOT automatically load foreign objects into ghost slots. If you need both filtering and eager loading, combine `joins` with `includes`:

```common-lisp
;; Filter by active users AND load user objects
(select-dao 'tweet
  (joins 'user)
  (includes 'user)
  (where (:= :user.status "active")))
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

SQLite3 migration creates temporary tables with pre-migration data. To delete them after migration is complete set
`mito:*migration-keep-temp-tables*` to `nil`. It has no effect on other drivers.

#### Auto migrations

If `mito:*auto-migration-mode*` is set to `t`, and you are connected to a database, Mito will run migrations after
each change to model definitions.

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

A subclass of DAO-CLASS is allowed to be inherited. This may be useful when you need classes that have similar columns:

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

If you need a 'template' for tables, not related to any specific database table, you can use `DAO-TABLE-MIXIN`:

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

Examples of inheritance can be found here:

* [mito-attachment](https://github.com/fukamachi/mito-attachment)
* [mito-auth](https://github.com/fukamachi/mito-auth)

### Triggers

Since `insert-dao`, `update-dao` and `delete-dao` are defined as generic functions, you can define `:before`, `:after` or `:around` methods on those.

```common-lisp
(defmethod mito:insert-dao :before ((object user))
  (format t "~&Adding ~S...~%" (user-name object)))

(mito:create-dao 'user :name "Eitaro Fukamachi" :email "e.arrows@gmail.com")
;-> Adding "Eitaro Fukamachi"...
;   ;; INSERT INTO "user" ("name", "email", "created_at", "updated_at") VALUES (?, ?, ?, ?) ("Eitaro Fukamachi", "e.arrows@gmail.com", "2016-02-16 21:13:47", "2016-02-16 21:13:47") [0 rows] | MITO.DAO:INSERT-DAO
;=> #<USER {100835FB33}>
```

### Iteration (Experimental)

`do-select` is a macro to iterate over results from SELECT one by one. It's the same as `cl:loop`, but it uses CURSOR for PostgreSQL, which can reduce memory usage since it won't load whole results on memory.

```common-lisp
(do-select (dao (select-dao 'user
                  (where (:< "2024-07-01" :created_at))))
  ;; Can be a more complex condition
  (when (equal (user-name dao) "Eitaro")
    (return dao)))

;; Same but without using CURSOR
(loop for dao in (select-dao 'user
                   (where (:< "2024-07-01" :created_at)))
      when (equal (user-name dao) "Eitaro")
      do (return dao))
```

The query form must be one of `select-dao`, `retrieve-dao`, or `select-by-sql`.

## Installation

```common-lisp
(ql:quickload :mito)
```

Or, with Roswell:

```
ros install mito
```

If you build a binary, reference a DB driver in your dependencies:

    :dbd-sqlite3 :dbd-mysql :dbd-postgres


## Mito Extensions and Plugins

* [mito-attachment](https://github.com/fukamachi/mito-attachment)
* [mito-auth](https://github.com/fukamachi/mito-auth)

## See Also

* [CL-DBI](https://github.com/fukamachi/cl-dbi)
* [SxQL](https://github.com/fukamachi/sxql)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 3-Clause License.
