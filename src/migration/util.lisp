(defpackage #:mito.migration.util
  (:use #:cl)
  (:import-from #:chipz
                #:make-crc32
                #:update-crc32
                #:produce-crc32)
  (:export #:generate-advisory-lock-id))
(in-package #:mito.migration.util)

(defun ascii-string-to-octets (value)
  (check-type value string)
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code value))

(defun crc32 (string)
  (let ((state (make-crc32))
        (octets (ascii-string-to-octets string)))
    (update-crc32 state octets 0 (length octets))
    (produce-crc32 state)))

(defvar +migrator-salt+ 2069753430)
(defun generate-advisory-lock-id (database-name)
  (* +migrator-salt+ (crc32 database-name)))
