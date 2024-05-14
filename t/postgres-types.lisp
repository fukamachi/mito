(defpackage #:mito-test.postgres-types
  (:use #:cl
        #:rove
        #:mito.dao
        #:mito.connection
        #:mito-test.util))
(in-package #:mito-test.postgres-types)

(deftest retrieve-by-sql
  (setf *connection* (connect-to-testdb :postgres))
  (ok (equal (mito:retrieve-by-sql "select row(1,NULL);")
             '((:ROW "(1,)"))))

  (ok (equal (mito:retrieve-by-sql "select NULL;")
             '((:?COLUMN? NIL))))

  (ok (equal (mito:retrieve-by-sql "select row(NULL, 'a', NULL);")
             '((:ROW "(,a,)"))))

  (ok (equalp (mito:retrieve-by-sql "select row(ARRAY[NULL, NULL]);")
              '((:ROW "(\"{NULL,NULL}\")"))))

  (ok (equalp (mito:retrieve-by-sql "select row(ARRAY['bogus', NULL]);")
              '((:ROW "(\"{bogus,NULL}\")")))))

(deftest retrieve-by-sql-binary
  (setf *connection* (connect-to-testdb :postgres))
  (cl-postgres:with-binary-row-values
    (ok (equal (mito:retrieve-by-sql "select row(1,NULL);")
               '((:ROW (1 NIL)))))

    (ok (equal (mito:retrieve-by-sql "select NULL;")
               '((:?COLUMN? NIL))))

    (ok (equal (mito:retrieve-by-sql "select row(NULL, 'a', NULL);")
               '((:ROW (NIL "a" NIL)))))

    (ok (equalp (mito:retrieve-by-sql "select row(ARRAY[NULL, NULL]);")
                '((:ROW (#(NIL NIL))))))

    (ok (equalp (mito:retrieve-by-sql "select row(ARRAY['bogus', NULL]);")
                '((:ROW (#("bogus" NIL))))))))
