# Mito

[![Build Status](https://travis-ci.org/fukamachi/mito.svg?branch=master)](https://travis-ci.org/fukamachi/mito)

Work in progress.

Mito is yet another object relational mapper and it aims to be a successor of [Integral](https://github.com/fukamachi/integral).

* Support PostgreSQL
* Better abstraction for RDBMS
* Better code structure

## Usage

```common-lisp
(defclass tweet ()
  ((id :col-type :serial
       :primary-key t
       :reader tweet-id)
   (status :col-type :text
           :accessor tweet-status)
   (user :col-type (:varchar 64)
         :accessor tweet-user))
  (:metaclass dao-table-class))
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the LLGPL License.
