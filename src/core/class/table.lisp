(in-package :cl-user)
(defpackage mito.class.table
  (:use #:cl
        #:mito.util)
  (:import-from #:mito.class.column
                #:parse-col-type
                #:table-column-class
                #:table-column-references
                #:column-standard-effective-slot-definitions
                #:table-column-type
                #:%table-column-type
                #:table-column-name
                #:primary-key-p
                #:ghost-slot-p)
  (:import-from #:alexandria
                #:ensure-list)
  (:export #:table-class
           #:table-name
           #:table-column-slots
           #:table-direct-column-slots
           #:table-primary-key
           #:table-serial-key
           #:table-indices-info
           #:database-column-slots
           #:find-slot-by-name

           #:find-parent-column
           #:find-child-columns))
(in-package :mito.class.table)

(defclass table-class (standard-class)
  ((primary-key :initarg :primary-key
                :initform nil)
   (unique-keys :initarg :unique-keys
                :initform nil)
   (keys :initarg :keys
         :initform nil)
   (table-name :initarg :table-name
               :initform nil)

   (parent-column-map)))

(defun rel-column-name (name pk-name)
  (intern
   (format nil "~:@(~A-~A~)" name pk-name)))

(defun add-referencing-slots (initargs)
  (let ((parent-column-map (make-hash-table :test 'eq))
        (class-name (getf initargs :name)))
    (setf (getf initargs :direct-slots)
          (loop for column in (getf initargs :direct-slots)
                for (col-type not-null) = (multiple-value-list (parse-col-type (getf column :col-type)))

                if (typep col-type '(and symbol (not null) (not keyword)))
                  append
                  (let* ((column-name (getf column :name))
                         ;; FIXME: find-class raises an error if the class is not defined yet.
                         (pk-names (if (eq col-type class-name)
                                       (or (getf initargs :primary-key)
                                           (getf (find-if (lambda (column-def)
                                                            (getf column-def :primary-key))
                                                          (getf initargs :direct-slots))
                                                 :name)
                                           (loop for superclass in (getf initargs :direct-superclasses)
                                                 for pk-names = (table-primary-key superclass)
                                                 until pk-names
                                                 finally (return pk-names)))
                                       (table-primary-key (find-class col-type)))))
                    (unless pk-names
                      (error "Primary keys can not be determined for ~A."
                             col-type))
                    (rplacd (cdr column)
                            `(:ghost t ,@(cddr column)))

                    (cons column
                          (mapcar (lambda (pk-name)
                                    (let ((rel-column-name (rel-column-name column-name pk-name)))
                                      (setf (gethash rel-column-name parent-column-map) column-name)
                                      `(:name ,rel-column-name
                                        :initargs (,(intern (symbol-name rel-column-name) :keyword))
                                        :col-type ,(if not-null
                                                       col-type
                                                       `(or ,col-type :null))
                                        :primary-key ,(getf column :primary-key)
                                        :references (,col-type ,pk-name))))
                                  pk-names)))
                  else collect column))
    (values initargs parent-column-map)))

(defun expand-relational-keys (class slot-name)
  (let ((keys (slot-value class slot-name))
        (table-slots (table-column-slots class)))
    (labels ((expand-key (key)
               (let* ((key-name (if (stringp key)
                                    key
                                    (unlispify (symbol-name-literally key))))
                      (slot (find key-name table-slots
                                  :key #'table-column-name
                                  :test #'string=)))
                 (unless slot
                   (error "Unknown column ~S is found in ~S ~S." key (class-name class) slot-name))
                 (if (ghost-slot-p slot)
                     (find-child-columns class slot)
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

(defmethod initialize-instance :around ((class table-class) &rest initargs)
  (multiple-value-bind (initargs parent-column-map)
      (add-referencing-slots initargs)
    (let ((class (apply #'call-next-method class initargs)))
      (setf (slot-value class 'parent-column-map) parent-column-map)
      (expand-relational-keys class 'primary-key)
      (expand-relational-keys class 'unique-keys)
      (expand-relational-keys class 'keys)
      class)))

(defmethod reinitialize-instance :around ((class table-class) &rest initargs)
  (multiple-value-bind (initargs parent-column-map)
      (add-referencing-slots initargs)
    (unless (getf initargs :primary-key)
      (setf (getf initargs :primary-key) nil))
    (unless (getf initargs :unique-keys)
      (setf (getf initargs :unique-keys) nil))
    (unless (getf initargs :keys)
      (setf (getf initargs :keys) nil))
    (unless (getf initargs :table-name)
      (setf (getf initargs :table-name) nil))
    (let ((class (apply #'call-next-method class initargs)))
      (setf (slot-value class 'parent-column-map) parent-column-map)
      (expand-relational-keys class 'primary-key)
      (expand-relational-keys class 'unique-keys)
      (expand-relational-keys class 'keys)
      class)))

(defmethod c2mop:direct-slot-definition-class ((class table-class) &key &allow-other-keys)
  'table-column-class)

(defmethod c2mop:effective-slot-definition-class ((class table-class) &rest initargs)
  (declare (ignorable initargs))
  (find-class 'column-standard-effective-slot-definitions))

(defmethod c2mop:validate-superclass ((class table-class) (super standard-class))
  t)

(defmethod c2mop:compute-effective-slot-definition
    :around ((class table-class) name direct-slot-definitions)
  (declare (ignorable name))
  (let* ((result (call-next-method))
         (have-col-type-slot (remove-if-not
                              (lambda (x) (slot-exists-p x 'mito.class.column:col-type))
                              direct-slot-definitions))
         (found-col-types (remove-if-not
                           (lambda (x) (slot-boundp x 'mito.class.column:col-type))
                           have-col-type-slot)))
    ;;(break)
    (when result
      ;; set here all the relevant slots. See column-standard-effective-slot-definitions
      (setf (ghost-slot-p result)
            (some #'ghost-slot-p direct-slot-definitions))
      (when found-col-types
        (setf (%table-column-type result)
              (some #'%table-column-type found-col-types)))
      (setf (table-column-references result)
            (some #'table-column-references direct-slot-definitions))
      (setf (primary-key-p result)
            (some #'primary-key-p direct-slot-definitions))
      result)))

(defgeneric table-name (class)
  (:method ((class table-class))
    (if (slot-value class 'table-name)
        (string (car (slot-value class 'table-name)))
        (let ((class-name (lispify (symbol-name-literally (class-name class)))))
          (unlispify
           (if (and (char= (aref class-name 0) #\<)
                    (char= (aref class-name (1- (length class-name))) #\>))
               (subseq class-name 1 (1- (length class-name)))
               class-name))))))

(defgeneric table-primary-key (class)
  (:method ((class table-class))
    (or (slot-value class 'primary-key)
        (let ((primary-slot (find-if
                             #'primary-key-p
                             (database-column-slots class))))
          (if primary-slot
              (list (c2mop:slot-definition-name primary-slot))
              nil)))))

(defgeneric table-serial-key (class)
  (:method ((class table-class))
    (let* ((primary-key (table-primary-key class))
           (slot (find-if
                  (lambda (slot)
                    (and
                     ;; AUTO INCREMENT slot
                     (member (table-column-type slot) '(:serial :bigserial)
                                 :test #'eq)
                     (member (c2mop:slot-definition-name slot)
                             primary-key :test #'eq)))
                  (database-column-slots class))))
      (if slot
          (c2mop:slot-definition-name slot)
          nil))))

(defun table-direct-column-slots (class)
  (remove-if-not (lambda (slot)
                   (typep slot 'table-column-class))
                 (c2mop:class-direct-slots class)))

(defun map-all-superclasses (fn class &key (key #'identity))
  (labels ((main (class &optional main-objects)
             (let ((ret (funcall fn class)))
               (loop for superclass in (c2mop:class-direct-superclasses class)
                     if (eq (class-of superclass) (find-class 'standard-class))
                       append (if (eq superclass (find-class 'standard-object))
                                  (append ret main-objects)
                                  ret)
                     else
                       append (main superclass
                                    (append ret main-objects))))))
    (delete-duplicates
     (main class)
     :test #'eq
     :key key
     :from-end t)))

(defun table-column-slots (class)
  (map-all-superclasses #'table-direct-column-slots
                        class
                        :key #'c2mop:slot-definition-name))

(defun find-slot-by-name (class slot-name &key (test #'eq))
  (find slot-name
        (table-column-slots (if (typep class 'symbol)
                                (find-class class)
                                class))
        :test test
        :key #'c2mop:slot-definition-name))

(defgeneric database-column-slots (class)
  (:method ((class table-class))
    (remove-if #'ghost-slot-p
               (table-column-slots class))))

(defgeneric table-indices-info (class driver-type)
  (:method (class driver-type)
    (let ((table-name (table-name class)))
      (labels ((ensure-string (data)
                 (etypecase data
                   (symbol (symbol-name-literally data))
                   (string data)))
               (unlispify-keys (keys)
                 (if (listp keys)
                     (mapcar #'unlispify (mapcar #'ensure-string keys))
                     (unlispify (ensure-string keys)))))
        (append
         (when (slot-value class 'primary-key)
           (let ((primary-keys (slot-value class 'primary-key)))
             (list
              (list (format nil "~A_pkey" table-name)
                    :unique-key t
                    :primary-key t
                    :columns (unlispify-keys primary-keys)))))
         ;; See also :primary-key column
         (let ((primary-key-slot (find-if #'primary-key-p (database-column-slots class))))
           (when primary-key-slot
             (list
              (list (format nil "~A_pkey" table-name)
                    :unique-key t
                    :primary-key t
                    :columns (unlispify-keys (list (table-column-name primary-key-slot)))))))

         (let ((unique-keys (map-all-superclasses (lambda (class)
                                                    (slot-value class 'unique-keys))
                                                  class)))
           (when unique-keys
             (mapcar (lambda (key)
                       ;; FIXME: it'll raise an error if the index name is too long
                       (list (format nil "unique_~A_~{~A~^_~}"
                                     table-name
                                     (unlispify-keys (ensure-list key)))
                             :unique-key t
                             :primary-key nil
                             :columns (ensure-list (unlispify-keys key))))
                     unique-keys)))
         (let ((keys (map-all-superclasses (lambda (class)
                                             (slot-value class 'keys))
                                           class)))
           (when keys
             (mapcar (lambda (key)
                       ;; FIXME: it'll raise an error if the index name is too long
                       (list (format nil "key_~A_~{~A~^_~}"
                                     table-name
                                     (unlispify-keys (ensure-list key)))
                             :unique-key nil
                             :primary-key nil
                             :columns (ensure-list (unlispify-keys key))))
                     keys))))))))

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
                   (or (and (slot-exists-p class 'parent-column-map)
                            (gethash name (slot-value class 'parent-column-map)))
                       (progn
                         (map nil #'enqueue-last (c2mop:class-direct-superclasses class))
                         (pop fifo-queue-of-classes)
                         (rec)))))))
      (rec))))

(defun find-child-columns (table slot)
  (let (results)
    (map-all-superclasses
      (lambda (class)
        (when (slot-exists-p class 'parent-column-map)
          (maphash (lambda (child parent)
                     (when (eq parent (c2mop:slot-definition-name slot))
                       (push child results)))
                   (slot-value class 'parent-column-map))))
      table)
    results))
