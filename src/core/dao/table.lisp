(in-package :cl-user)
(defpackage mito.dao.table
  (:use #:cl
        #:mito.util
        #:mito.error)
  (:import-from #:mito.connection
                #:driver-type)
  (:import-from #:mito.class
                #:table-class
                #:table-name
                #:table-column-name
                #:table-column-type
                #:table-column-slots
                #:table-primary-key
                #:create-table-sxql
                #:ghost-slot-p
                #:find-slot-by-name)
  (:import-from #:mito.dao.column
                #:dao-table-column-class
                #:dao-table-column-foreign-class
                #:dao-table-column-inflate)
  (:import-from #:mito.dao.mixin
                #:auto-pk-mixin
                #:record-timestamps-mixin)
  (:import-from #:alexandria
                #:disjoin)
  (:export #:dao-class
           #:dao-table-class

           #:dao-synced

           #:make-dao-instance
           #:table-definition

           #:depending-table-classes))
(in-package :mito.dao.table)

(defclass dao-class ()
  ((synced :type boolean
           :initform nil
           :accessor dao-synced)))

(defclass dao-table-class (table-class)
  ((auto-pk :initarg :auto-pk
            :initform '(t))
   (record-timestamps :initarg :record-timestamps
                      :initform '(t))
   (parent-column-map)))

(defmethod c2mop:direct-slot-definition-class ((class dao-table-class) &key)
  'dao-table-column-class)

(defun check-col-type (col-type)
  (etypecase col-type
    ((or null keyword) t)
    (symbol (typep (find-class col-type nil) 'dao-table-class))
    (cons
     (optima:ematch col-type
       ((or (list 'or :null (or (keyword) (cons (keyword) _)))
            (list 'or (or (keyword) (cons (keyword) _)) :null)
            (cons (keyword) _))
        t)))))

(defun initargs-enables-auto-pk (initargs)
  (first (or (getf initargs :auto-pk) '(t))))

(defun initargs-enables-record-timestamps (initargs)
  (first (or (getf initargs :record-timestamps) '(t))))

(defun initargs-contains-primary-key (initargs)
  (or (getf initargs :primary-key)
      (find-if (lambda (slot)
                 (getf slot :primary-key))
               (getf initargs :direct-slots))))

(defun make-dao-instance (class &rest initargs)
  (setf class (ensure-class class))

  (assert (and class
               (typep class 'dao-table-class)))

  (let ((obj
          (apply #'make-instance class
                 :allow-other-keys t
                 (loop for (k v) on initargs by #'cddr
                       for column = (find-if (lambda (initargs)
                                               (find k initargs :test #'eq))
                                             (table-column-slots class)
                                             :key #'c2mop:slot-definition-initargs)
                       if column
                         append (list k
                                      (dao-table-column-inflate column v))
                       else
                         append (list k v)))))
    (setf (dao-synced obj) t)
    obj))

(defun rel-column-name (name pk-name)
  (intern
   (format nil "~A-~A" name pk-name)
   (symbol-package name)))

(defun initialize-initargs (initargs)
  (let ((parent-column-map (make-hash-table :test 'eq)))
    ;; Add relational column slots (ex. user-id)
    (loop for column in (copy-seq (getf initargs :direct-slots)) ;; Prevent infinite-loop
          for (col-type . not-null) = (let ((col-type (getf column :col-type)))
                                        (optima:match col-type
                                          ((or (list 'or :null x)
                                               (list 'or x :null))
                                           (cons x t))
                                          (otherwise
                                           (cons col-type nil))))
          when (and (symbolp col-type)
                    (not (null col-type))
                    (not (keywordp col-type)))
            do (rplacd (cdr column)
                       `(:ghost t ,@(cddr column)))
               (let* ((name (getf column :name))
                      ;; FIXME: find-class returns NIL if the class is this same class
                      (rel-class (find-class col-type))
                      (pk-names (table-primary-key rel-class)))
                 (dolist (pk-name pk-names)
                   (let ((rel-column-name (rel-column-name name pk-name)))
                     (setf (gethash rel-column-name parent-column-map) name)
                     (rplacd (last (getf initargs :direct-slots))
                             `((:name ,rel-column-name
                                :initargs (,(intern (symbol-name rel-column-name) :keyword))
                                ;; Defer retrieving the relational column type until table-column-info
                                :col-type ,(getf column :col-type)
                                :references (,(class-name rel-class) ,pk-name))))))))
    (values initargs parent-column-map)))

(defun make-relational-reader-method (func-name class slot-name rel-class)
  (let ((generic-function
          (ensure-generic-function func-name :lambda-list '(object))))
    (add-method
     generic-function
     (make-instance 'standard-method
                    :lambda-list '(object)
                    :qualifiers ()
                    :specializers (list class)
                    :function
                    (lambda (object &rest ignore)
                      (declare (ignore ignore))
                      ;; I don't know why but SBCL pass a CONS of the instance instead of the instance itself.
                      (when (consp object)
                        (setf object (first object)))
                      (if (slot-boundp object slot-name)
                          (slot-value object slot-name)
                          (let ((foreign-object
                                  (apply #'make-dao-instance rel-class
                                         (first
                                          (mito.db:retrieve-by-sql
                                           (sxql:select :*
                                             (sxql:from (sxql:make-sql-symbol (table-name rel-class)))
                                             (sxql:where
                                              `(:and
                                                ,@(mapcar (lambda (pk-name)
                                                            `(:= ,(sxql:make-sql-symbol
                                                                   (table-column-name
                                                                    (find-slot-by-name rel-class pk-name)))
                                                                 ,(slot-value object (rel-column-name slot-name pk-name))))
                                                          (table-primary-key rel-class))))
                                             (sxql:limit 1)))))))
                            (setf (slot-value object slot-name) foreign-object))))))))

(defun add-relational-readers (class initargs)
  (loop for column in (copy-seq (getf initargs :direct-slots)) ;; Prevent infinite-loop
        for (col-type . not-null) = (let ((col-type (getf column :col-type)))
                                      (optima:match col-type
                                        ((or (list 'or :null x)
                                             (list 'or x :null))
                                         (cons x t))
                                        (otherwise
                                         (cons col-type nil))))
        when (and (symbolp col-type)
                  (not (null col-type))
                  (not (keywordp col-type)))
          do (let* ((name (getf column :name))
                    ;; FIXME: find-class returns NIL if the class is this same class
                    (rel-class (find-class col-type)))
               (dolist (reader (getf column :readers))
                 (make-relational-reader-method reader class name rel-class)))))

(defun expand-relational-keys (class slot-name)
  (let ((keys (slot-value class slot-name))
        (direct-slots (c2mop:class-direct-slots class)))
    (labels ((expand-key (key)
               (let ((slot (find key direct-slots
                                 :key #'c2mop:slot-definition-name
                                 :test #'eq)))
                 (if (ghost-slot-p slot)
                     (mapcar #'c2mop:slot-definition-name
                             (remove-if-not (lambda (ds)
                                              (eq (dao-table-column-foreign-class ds)
                                                  (find-class (table-column-type slot))))
                                            direct-slots))
                     (list key))))
             (expand-keys (keys)
               (loop for key in keys
                     append (expand-key key))))
      (setf (slot-value class slot-name)
            (loop for key in keys
                  if (listp key)
                    collect (expand-keys key)
                  else
                    append (expand-key key))))))

(defun depending-table-classes (class)
  (delete-duplicates
   (remove-if (disjoin (lambda (x) (eq x class))
                       #'null)
              (mapcar #'dao-table-column-foreign-class
                      (table-column-slots class)))
   :from-end t
   :test #'eq))

(defmethod initialize-instance :around ((class dao-table-class) &rest initargs
                                        &key direct-superclasses &allow-other-keys)
  (multiple-value-bind (initargs parent-column-map)
      (initialize-initargs initargs)

    (when (and (initargs-enables-record-timestamps initargs)
               (not (contains-class-or-subclasses 'record-timestamps-mixin direct-superclasses)))
      (setf (getf initargs :direct-superclasses)
            (append (getf initargs :direct-superclasses)
                    (list (find-class 'record-timestamps-mixin)))))

    (unless (contains-class-or-subclasses 'dao-class direct-superclasses)
      (push (find-class 'dao-class) (getf initargs :direct-superclasses)))

    (when (and (initargs-enables-auto-pk initargs)
               (not (initargs-contains-primary-key initargs))
               (not (contains-class-or-subclasses 'auto-pk-mixin direct-superclasses)))
      (push (find-class 'auto-pk-mixin) (getf initargs :direct-superclasses)))

    (let ((class (apply #'call-next-method class initargs)))
      (add-relational-readers class initargs)
      (expand-relational-keys class 'mito.class.table::primary-key)
      (expand-relational-keys class 'mito.class.table::unique-keys)
      (expand-relational-keys class 'mito.class.table::keys)
      (setf (slot-value class 'parent-column-map) parent-column-map)
      class)))

(defmethod reinitialize-instance :around ((class dao-table-class) &rest initargs
                                                                    &key direct-superclasses &allow-other-keys)
  (multiple-value-bind (initargs parent-column-map)
      (initialize-initargs initargs)
    (when (and (initargs-enables-record-timestamps initargs)
               (not (contains-class-or-subclasses 'record-timestamps-mixin direct-superclasses)))
      (setf (getf initargs :direct-superclasses)
            (append (getf initargs :direct-superclasses)
                    (list (find-class 'record-timestamps-mixin)))))

    (when (and (initargs-enables-auto-pk initargs)
               (not (initargs-contains-primary-key initargs))
               (not (contains-class-or-subclasses 'auto-pk-mixin direct-superclasses)))
      (push (find-class 'auto-pk-mixin) (getf initargs :direct-superclasses)))

    (let ((class (apply #'call-next-method class initargs)))
      (add-relational-readers class initargs)
      (expand-relational-keys class 'mito.class.table::primary-key)
      (expand-relational-keys class 'mito.class.table::unique-keys)
      (expand-relational-keys class 'mito.class.table::keys)
      (setf (slot-value class 'parent-column-map) parent-column-map)
      class)))

(defmethod c2mop:ensure-class-using-class :around ((class dao-table-class) name &rest keys
                                                   &key direct-superclasses &allow-other-keys)
  (unless (contains-class-or-subclasses 'dao-class direct-superclasses)
    (setf (getf keys :direct-superclasses)
          (cons (find-class 'dao-class) direct-superclasses)))
  (apply #'call-next-method class name keys))

(defun table-definition (class &key if-not-exists)
  (setf class (ensure-class class))
  (check-type class table-class)
  (create-table-sxql class (driver-type)
                     :if-not-exists if-not-exists))

(defun find-parent-column (table slot)
  (let* ((name (c2mop:slot-definition-name slot))
         (fifo-queue-of-classes (list table))
         (last fifo-queue-of-classes))
    ;; runs a breadth-first search
    (labels ((enqueue-last (thing)
               (setf (cdr last) (list thing)
                     last (cdr last)))
             (rec ()
               (let ((class (first fifo-queue-of-classes)))
                 (when class
                   (or (gethash name (slot-value class 'parent-column-map))
                       (progn
                         (map nil #'enqueue-last (c2mop:class-direct-superclasses class))
                         (pop fifo-queue-of-classes)
                         (rec)))))))
      (rec))))

(defun find-child-columns (table slot)
  (let (results)
    (maphash (lambda (child parent)
               (when (eq parent (c2mop:slot-definition-name slot))
                 (push child results)))
             (slot-value table 'parent-column-map))
    results))
