(in-package :openapi-generator)

(defun to-file (pathname s)
  "Adapted from cl-str.
Write string `s' to file `pathname'. If the file does not exist, create it (use `:if-does-not-exist'), if it already exists, replace its content (`:if-exists').

Returns the string written to file."
  (with-open-file (f pathname :direction :output :if-exists :supersede :if-does-not-exist :create :external-format :utf-8)
    (write-sequence s f)))

(defun from-file (pathname)
  "Adapted from cl-str.
Read the file and return its content as a string.

It simply uses uiop:read-file-string. There is also uiop:read-file-lines."
  (funcall (function uiop:read-file-string) pathname :external-format :utf-8))

(defun slot-option-p (item)
  (member item (list :reader :write :accessor :allocation
                     :initarg :initform :type :documentation
                     :json-type :json-key :required)))

(deftype slot-option ()
  '(and keyword (satisfies slot-option-p)))

(defun json-mop-type-p (item)
  (member item (list :any :string :integer :number :hash-table
                          :vector :list :bool)))

(deftype json-mop-type ()
  (quote (and keyword (satisfies json-mop-type-p))))

(deftype non-keyword-symbol ()
  (quote (and symbol (not keyword))))

(defun json-mop-composite-type-p (item)
  (and (member (first item) (list :hash-table :vector :list))
       (typep (second item) '(or json-mop-type
                              non-keyword-symbol))))

(deftype json-mop-composite-type ()
  (quote
   (and cons (satisfies json-mop-composite-type-p))))

(defmacro define-json-class (name direct-superclasses direct-slots &rest options)
  (flet ((expand-slot (slot-specifier)
           (etypecase slot-specifier
             (string
              (list (intern (string-upcase (str:param-case slot-specifier)))
                    :json-key slot-specifier))
             (cons
              (let ((first
                      (first slot-specifier))
                    (second
                      (second slot-specifier)))
                (etypecase first
                  (symbol
                   (etypecase second
                     (slot-option
                      slot-specifier)
                     (string
                      (let ((third
                              (third slot-specifier)))
                        (etypecase third
                          (slot-option
                           (list* first :json-key second (cddr slot-specifier)))
                          ((or json-mop-type non-keyword-symbol json-mop-composite-type)
                           (list* first :json-key second :json-type third
                                  (cdddr slot-specifier))))))))
                  (string
                   (let ((slot-name
                           (intern (string-upcase (str:param-case first)))))
                     (if (= (length slot-specifier) 1)
                         (list slot-name :json-key first)
                         (etypecase second
                           (slot-option
                            (list* slot-name :json-key first
                                   (cdr slot-specifier)))
                           ((or non-keyword-symbol json-mop-type json-mop-composite-type)
                            (list* slot-name :json-key first :json-type second
                                   (cddr slot-specifier)))))))))))))
    (list* 'defclass name direct-superclasses
           (mapcar #'expand-slot direct-slots)
           (list :metaclass 'json-mop:json-serializable-class)
           options)))

(defgeneric concat-strings (list)
  (:documentation "Concatenates strings together and breaks when other element comes")
  (:method ((list list))
    (let ((result-list
            nil))
      (mapc (function (lambda (item)
              (push (typecase item
                      (string
                       (let ((first-list-item
                               (first result-list)))
                         (typecase first-list-item
                           (string 
                            (pop result-list)
                            (concat item first-list-item))
                           (otherwise
                            item))))
                      (otherwise
                       item))
                    result-list)))
            (reverse list))
      result-list)))

(defun remove-empty-values (alist)
  "Remove empty values from alist (used at run time)"
  (remove-if (function (lambda (list)
               (unless (cdr list)
                 t)))
             alist))

(defun one-item (name)
  "Intern the item in the package active during execution of this function"
  `(cons ,name
         ,(intern (upcase (param-case name)))))

(defun gen-alist (names)
  "Generate association list with the symbol lispified."
  (when names
    (mapcar (function one-item) names)))

(defgeneric list-symbols (list)
  (:documentation "Filter non-symols out of list")
  (:method ((list list))
    (remove-if (function (lambda (item)
                 (cl:not (symbolp item))))
               list)))

(defun get-data-file (name &key (type "json"))
  "Get data file"
  (system-relative-pathname "openapi-generator"
                            (concat "data/" name "." type)))

(defun uri-p (uri)
  "Uri predicate."
  (let ((scheme
          (uri-scheme (uri uri))))
    (when (or (string-equal "https" scheme)
              (string-equal "http" scheme))
      t)))

(defmethod hash-copy-recursive ((hash hash-table))
  "Inspired by cl-hash-util:hash-copy, but performs a recursive (deep) hash-table copy
  which replaces all internal hash-tables with copies.
  This is needed to avoid looping when working with circular json-pointers."
  (let ((new-hash (make-hash-table :test (function equal) :size (hash-table-count hash))))
    (loop for k being the hash-keys of hash
          for v being the hash-values of hash do
            (setf (gethash k new-hash)
                  (if (typep v (quote hash-table))
                      (funcall (function hash-copy-recursive) v)
                      v)))
    new-hash))

(defgeneric intern-param (s)
  (:documentation "Convert string or list of strings to param-cased symbol(s).")
  (:method ((s string))
    (intern (upcase (param-case s))))
  (:method ((s null))
    nil)
  (:method ((s vector))
    (mapcar (function intern-param) (coerce s (quote list))))
  (:method ((s list))
    (mapcar (function intern-param) s)))

(deftype json-string ()
  (quote (and string (satisfies valid-json-p))))

(defun json-true-p (s)
  "Predicate for valid json true"
  (string-equal "true" s))

(deftype json-true ()
  (quote (and string (satisfies json-true-p))))

(defun json-false-p (s)
  "Predicate for valid json false"
  (string-equal "false" s))

(deftype json-false ()
  (quote (and string (satisfies json-false-p))))

(defun json-null-p (s)
  "Predicate for valid json null"
  (string-equal "null" s))

(deftype json-null ()
  (quote (and string (satisfies json-null-p))))

(defun json-number-p (s)
  "Predicate for valid json number (string)"
  (when (ignore-errors (parse-float s))
    t))

(deftype json-number ()
  (quote (and string (satisfies json-number-p))))

(defun integer-string-p (s)
  "Predicate for valid json number (string)"
  (integerp (ignore-errors (parse-integer s))))

(deftype integer-string ()
  (quote (and string (satisfies integer-string-p))))

(defun json-array-p (s)
  "Predicate for valid json array (string)"
  (vectorp (ignore-errors (parse s))))

(deftype json-array ()
  (quote (and string (satisfies json-array-p))))

(defun json-object-p (s)
  "Predicate for valid json array (string)"
  (hash-table-p (ignore-errors (parse s))))

(deftype json-object ()
  (quote (and string (satisfies json-object-p))))

(defpackage dummy-printing-package )
