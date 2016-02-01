# Mito

[![Build Status](https://travis-ci.org/fukamachi/mito.svg?branch=master)](https://travis-ci.org/fukamachi/mito)
[![Coverage Status](https://coveralls.io/repos/fukamachi/mito/badge.svg?branch=master&service=github)](https://coveralls.io/github/fukamachi/mito?branch=master)

Work in progress.

Mito is yet another object relational mapper and it aims to be a successor of [Integral](https://github.com/fukamachi/integral).

* Support PostgreSQL
* Better abstraction for RDBMS
* Better code structure

## Usage

```common-lisp
(defclass user ()
  ((name :col-type (:varchar 64)
         :initarg :name
         :accessor user-name)
   (email :col-type (or (:varchar 128) :null)
          :initarg :email
          :accessor user-email))
  (:metaclass dao-table-class))

(defclass tweet ()
  ((status :col-type :text
           :initarg :status
           :accessor tweet-status)
   (user :col-type user
         :initarg :user
         :accessor tweet-user))
  (:metaclass dao-table-class))
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the LLGPL License.
