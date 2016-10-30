(in-package :cl-user)
(defpackage mito-test.postgres-types
  (:use #:cl
        #:prove
        #:mito.dao
        #:mito.connection
        #:mito-test.util))
(in-package :mito-test.postgres-types)

(plan nil)

(subtest "retrieve-by-sql"
  (setf *connection* (connect-to-testdb :postgres))
  (is (mito:retrieve-by-sql "select row(1,NULL);")
      '((:ROW "(1,)")))

  (is (mito:retrieve-by-sql "select NULL;")
      '((:?COLUMN? NIL)))

  (is (mito:retrieve-by-sql "select row(NULL, 'a', NULL);")
      '((:ROW "(,a,)")))

  (is (mito:retrieve-by-sql "select row(ARRAY[NULL, NULL]);")
      '((:ROW "(\"{NULL,NULL}\")"))
      :test #'equalp)

  (is (mito:retrieve-by-sql "select row(ARRAY['bogus', NULL]);")
      '((:ROW "(\"{bogus,NULL}\")"))
      :test #'equalp))

(subtest "retrieve-by-sql-binary"
  (setf *connection* (connect-to-testdb :postgres))
  (cl-postgres:with-binary-row-values
    (is (mito:retrieve-by-sql "select row(1,NULL);")
        '((:ROW (1 NIL))))

    (is (mito:retrieve-by-sql "select NULL;")
        '((:?COLUMN? NIL)))

    (is (mito:retrieve-by-sql "select row(NULL, 'a', NULL);")
        '((:ROW (NIL "a" NIL))))

    (is (mito:retrieve-by-sql "select row(ARRAY[NULL, NULL]);")
        '((:ROW (#(NIL NIL))))
        :test #'equalp)

    (is (mito:retrieve-by-sql "select row(ARRAY['bogus', NULL]);")
        '((:ROW (#("bogus" NIL))))
        :test #'equalp)))

(finalize)
