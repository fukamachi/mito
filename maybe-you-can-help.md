# Hi Fukamachi-san,

I am interested in using mito (and sxql, etc.) for a small project I am working on, called ClassStarter.

ClassStarter will be an auction market for classes at my Hackerspace, **Freeside Atlanta**.

I am starting by connecting to Sqlite3, but will probably migrate it to Postgres, so Mito seems perfect.

I have been able to create tables, and add records to some tables, but am having problems with adding records that depend on other records.

For instance, one table holds records for **person**, and each **person** has a **role**.

**role** is a separate, small table to condense data and avoid spelling errors and so on.

Here is a simplified case showing the problem I am having.

I did some edits on the mito README, to try to help you out, and not just ask for help.

I hope you can help me get over this issue.

## **File: /home/gt/gitstuff/classstarter/package.lisp**
```
;;;; From ./package.lisp
(ql:quickload :mito)
(defpackage #:classstarter
(:use
   #:common-lisp
   #:mito))
(in-package :classstarter)
(mito:connect-toplevel :sqlite3 :database-name \"/home/gt/test.sqlite3\")
(mito:deftable role ()
  ((name :col-type (:varchar 64))))
(mito:deftable person ()
  ((name :col-type (:varchar 64))
   (role :col-type role)))
(mapc #'mito:execute-sql (mito:table-definition 'role))
(mapc #'mito:execute-sql (mito:table-definition 'person))
(defun create-roles (role-names)
  (mapc (lambda (x) (mito:create-dao 'role :name x)) role-names))
;(setf role-namen '(\"Instructor\" \"Student\"))
;(create-roles role-namen)
(defun create-person ()
  (mito:create-dao 'person :name \"Jud Taylor\" :role 1))
```
  
## **At REPL:**
```
; Restarting inferior lisp process
; --------------------------------------------------------
; Dedicated output stream setup (port 44285)
; Redirecting all output to this MREPL
; SLY 1.0.0-beta-3 (#<MREPL mrepl-1-1>)
CL-USER> (load \"/home/gt/gitstuff/classstarter/class-starter-test.lisp\")
To load \"mito\":
  Load 1 ASDF system:
    mito
; Loading \"mito\"
....
T
CL-USER> (in-package :classstarter)
#<PACKAGE \"CLASSSTARTER\">
CLASSSTARTER> (setf role-namen '(\"Instructor\" \"Student\"))
; in: SETF ROLE-NAMEN
;     (SETF CLASSSTARTER::ROLE-NAMEN '(\"Instructor\" \"Student\"))
; ==>
;   (SETQ CLASSSTARTER::ROLE-NAMEN '(\"Instructor\" \"Student\"))
; 
; caught WARNING:
;   undefined variable: ROLE-NAMEN
; 
; compilation unit finished
;   Undefined variable:
;     ROLE-NAMEN
;   caught 1 WARNING condition
(\"Instructor\" \"Student\")
CLASSSTARTER> (create-roles role-namen)
(\"Instructor\" \"Student\")
CLASSSTARTER> ; Worked.  I checked in DB Browser
; No values
CLASSSTARTER> (create-person)
; Debugger entered on #<SIMPLE-ERROR \"~@<When attempting to ~A, the slot ~S is missing from the ~
;           object ~S.~@:>\" {1002BF79C3}>
```
## **In debugger:**
```
When attempting to read the slot's value (slot-value), the slot
MITO.DAO.MIXIN::ID is missing from the object 1.
   [Condition of type SIMPLE-ERROR]
Restarts:
 0: [RETRY] Retry SLY mREPL evaluation request.
 1: [*ABORT] Return to SLY's top level.
 2: [ABORT] abort thread (#<THREAD \"sly-channel-1-mrepl-remote-1\" RUNNING {1002916C03}>)
Backtrace:
 0: ((:METHOD SLOT-MISSING (T T T T)) #<unused argument> 1 MITO.DAO.MIXIN::ID SLOT-VALUE NIL) [fast-method]
 1: (SLOT-VALUE 1 MITO.DAO.MIXIN::ID)
 2: (MITO.DAO::FOREIGN-VALUE #<PERSON {10029043C3}> #<MITO.DAO.COLUMN:DAO-TABLE-COLUMN-CLASS CLASSSTARTER::ROLE-ID>)
 3: (MITO.DAO::MAKE-SET-CLAUSE #<PERSON {10029043C3}>)
 4: ((:METHOD INSERT-DAO (DAO-CLASS)) #<PERSON {10029043C3}>) [fast-method]
 5: ((SB-PCL::EMF INSERT-DAO) #<unused argument> #<unused argument> #<PERSON {10029043C3}>)
 6: (SB-INT:SIMPLE-EVAL-IN-LEXENV (CREATE-PERSON) #<NULL-LEXENV>)
 7: (EVAL (CREATE-PERSON))
 8: ((LAMBDA NIL :IN SLYNK-MREPL::MREPL-EVAL-1))
 --more--
```
How should I change either a class definition or a function to create a **person**?

# Thanks!
Jud
