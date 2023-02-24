(in-package :openapi-generator)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-slot (json-key json-type rest)
    (let ((slot-symbol
            (intern (upcase (param-case json-key)))))
      (if rest
          `(,slot-symbol
            :json-type ,json-type
            :json-key ,json-key
            :accessor ,slot-symbol
            ,@rest)
          `(,slot-symbol
            :json-type ,json-type
            :json-key ,json-key
            :accessor ,slot-symbol)))))

(defmacro json-class (name direct-superclasses slots)
  `(defclass ,name ,direct-superclasses
     (,@(mapcar (function (lambda (slot)
                  (expand-slot (first slot)
                               (second slot)
                               (cddr slot))))
                slots))
     (:metaclass json-mop:json-serializable-class)))

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

#|
(defun remove-empty-values (alist)
  "Remove empty values from alist (used at run time)"
  (let ((new-alist nil))
    (mapc (function (lambda (list)
	    (unless (proper-list-p list)
	      (push list new-alist))))
	  alist)
    new-alist))
|#
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
