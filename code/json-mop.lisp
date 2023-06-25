(in-package :json-mop)

(defmethod to-lisp-value ((value hash-table) (json-type (eql :any)))
  "When the JSON type is :ANY, Pass the hash-table VALUE unchanged"
  value)

(defmethod to-lisp-value ((value list) (json-type cons))
  "When the JSON type is :ANY, Pass the hash-table VALUE unchanged"
  (funcall (function to-lisp-value) (coerce value 'vector) json-type))


(defclass json-serializable-slot (closer-mop:standard-direct-slot-definition)
  ((json-key :initarg :json-key
             :initform nil
             :reader json-key-name)
   (json-type :initarg :json-type
              :initform :any
              :reader json-type)
   (required :initarg :required
             :initform nil
             :reader required)))

(defun initialize-slots-from-json (input lisp-object class-obj &optional (key-count 0))
  "Initializes all slots from LISP-OBJECT from INPUT.
All slots, direct or inherited, that exist in class CLASS-OBJ are considered.
Includes checking of required slots."
  (loop for superclass in (closer-mop:class-direct-superclasses class-obj)
        unless (eql superclass (find-class (quote json-serializable)))
          do (setf (values lisp-object key-count)
                   (initialize-slots-from-json input lisp-object superclass key-count)))
  (loop for slot in (closer-mop:class-direct-slots class-obj)
        do (let* ((json-key-name
                   (json-key-name slot))
                 (value
                   (gethash json-key-name input :null)))
             (when (and (eq value :null)
                        (eq (required slot) t))
               (warn (str:concat "The key \"" json-key-name "\" is not present in " (format nil "~A" input)
                                   ", but required in "(format nil "~A" class-obj)".")))
             (when json-key-name
               (handler-case
                   (progn
                     (setf (slot-value lisp-object
                                       (closer-mop:slot-definition-name slot))
                           (to-lisp-value value (json-type slot)))
                     (incf key-count))
                 (null-value (condition)
                   (declare (ignore condition)) nil)))))
  (values lisp-object key-count))

(defmethod json-to-clos ((input hash-table) class &rest initargs)
  (multiple-value-bind (lisp-object key-count)
      (initialize-slots-from-json input
                                  (apply (function make-instance) class initargs)
                                  (find-class class))
    (when (zerop key-count)
      (warn 'no-values-parsed
            :hash-table input
            :class-name class))
    (values lisp-object key-count)))

(defmethod to-lisp-value ((value hash-table) (json-type cons))
  (destructuring-bind (type value-type)
      json-type
    (ecase type
      (:hash-table
       (let ((hash-table
               (make-hash-table :size (hash-table-size value)
                                :test (function equal))))
         (maphash (function (lambda (key value)
                    (setf (gethash key
                                   hash-table)
                          (to-lisp-value value
                                         value-type))))
                  value)
         hash-table)))))
