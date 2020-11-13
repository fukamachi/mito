(defpackage #:mito.migration.sql-parse
  (:use #:cl
        #:esrap)
  (:shadow #:space)
  (:export #:parse-statements))
(in-package #:mito.migration.sql-parse)

(defrule end (~ "END"))

(defrule begin-end (and (~ "BEGIN") (+ (or quoted-symbol quoted-string begin-end case-end if-end (not end))) end)
  (:destructure (b content e)
    (format nil "~A~{~A~}~A" b content e)))

(defrule case-end (and (~ "CASE") (+ (or quoted-symbol quoted-string begin-end case-end if-end (not end))) end)
  (:destructure (b content e)
    (format nil "~A~{~A~}~A" b content e)))

(defrule if-end (and (~ "IF") (+ (or quoted-symbol quoted-string begin-end case-end if-end (not (~ "END IF")))) (~ "END IF"))
  (:destructure (b content e)
    (format nil "~A~{~A~}~A" b content e)))

(defrule quoted-symbol (and #\" (+ (or (not #\") (and #\\ #\"))) #\")
  (:destructure (d1 content d2)
    (declare (ignore d1 d2))
    (format nil "\"~{~A~}\"" content)))

(defrule quoted-string (and #\' (+ (or (not #\') (and #\\ #\'))) #\')
  (:destructure (d1 content d2)
    (declare (ignore d1 d2))
    (format nil "'~{~A~}'" content)))

(defrule space (or #\Space #\Newline #\Return #\Tab))

(defrule statement (and (* space)
                        (+ (or quoted-symbol quoted-string begin-end (not #\;))) (? #\;)
                        (* space))
  (:destructure (s1 content semicolon s2)
    (declare (ignore s1 s2))
    (format nil "~{~A~}~:[~;;~]" content semicolon)))

(defun parse-statements (content)
  (values (esrap:parse '(* statement) content)))
