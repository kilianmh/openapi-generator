(cl:in-package #:openapi-generator)

(defgeneric resolve-reference (api reference-string)
  (:documentation "Resolve relative references.")
  (:method ((api openapi) (reference-string string))
    (let ((reference-list
            (cdr (split "/" reference-string))))
      (gethash (third reference-list)
               (slot-value (slot-value api (intern (upcase (first reference-list))
                                                   "OPENAPI-GENERATOR"))
                           (intern (upcase (second reference-list))
                                   "OPENAPI-GENERATOR"))))))

(defgeneric get-primary-uri (api)
  (:documentation "Return first server uri")
  (:method ((api openapi))
    (let ((servers (servers api)))
      (typecase servers
        (string
         servers)
        (vector
         (let ((first-server-object
                 (car (coerce servers (quote list)))))
           (typecase first-server-object
             (string first-server-object)
             (hash-table
              (let* ((first-url
                      (gethash "url" first-server-object))
                     (curly-brace-location
                       (cl:search "{" first-url)))
                (if curly-brace-location
                    (let* ((end
                             (cl:search "}" first-url))
                           (variable-name
                             (substring (1+ curly-brace-location)
                                        end first-url)))
                      (replace-first (concat "{" variable-name "}")
                                     (gethash "default"
                                              (gethash variable-name
                                                       (gethash "variables" first-server-object)))
                                     first-url))
                    first-url))))))))))

(defgeneric collect-parameters (api path operation)
  (:documentation "Collect all parameters belong to an api a path and operation.")
  (:method ((api openapi) (path path) (operation symbol))
    (let* ((path-parameters
             (slot-value-safe path (quote parameters)))
           (operation-parameters
             (slot-value-safe (slot-value path operation) (quote parameters)))
           (parameters-with-duplicates
             (remove nil (append (when path-parameters
                                   path-parameters)
                                 (when operation-parameters
                                   operation-parameters))))
           (parameters-with-references
             (mapcar (function (lambda (parameter)
                       (let ((parameter-ref
                               (slot-value-safe parameter (quote ref))))
                         (cond ((slot-boundp parameter (quote name))
                                parameter)
                               (parameter-ref
                                (resolve-reference api parameter-ref))))))
                     parameters-with-duplicates))
           (parameters-without-duplicates
             (remove nil
                     (let ((names-list
                             ()))
                       (mapcar (function (lambda (parameter)
                                 (let ((name
                                         (slot-value parameter (quote name))))
                                   (unless (member name names-list :test (function string-equal))
                                     (push name names-list)
                                     parameter))))
                               parameters-with-references)))))
      parameters-without-duplicates)))

(defgeneric get-parameter-type (type parameters)
  (:documentation "Get list of parameters with specified type: Can be either query, path, head or cookie.")
  (:method ((type string) (parameters list))
    (remove nil
            (mapcar (function (lambda (parameter)
                      (when (string-equal (slot-value parameter (quote in))
                                          type)
                        parameter)))
                    parameters))))

(defgeneric get-required-parameter (parameters &key required)
  (:documentation "Get required or optional parameters for given api path operation")
  (:method ((parameters list) &key required)
    (remove nil
            (mapcar (function (lambda (parameter)
                      (when (cond ((eql required t)
                                   (eql (quote true) (slot-value-safe parameter (quote required))))
                                  ((null required)
                                   (null (eql (quote true)
                                              (slot-value-safe parameter (quote required))))))
                        parameter)))
                    parameters))))

(defgeneric get-uri-type (uri)
  (:documentation "")
  (:method ((uri string))
    (let* ((uri-path (uri-path (uri uri)))
           (uri-path-length (length uri-path))
           (file-type (substring (- uri-path-length 4)
                                 uri-path-length
                                 uri-path)))
      (string-case file-type
        ("yaml"
         "yaml")
        ("json"
         "json")
        (otherwise
         (warn "~A is the wrong file type. Acceptable values are yaml or json." file-type))))))

(defgeneric function-name (path operation-type &key param-case)
  (:documentation "Generate unique symbole name for given operation-type and path")
  (:method ((path string) (operation-type symbol) &key param-case)
    (concat (symbol-name operation-type)
	    (upcase (if param-case
                        (concat "-" (param-case path))
                        path)))))

(defgeneric object-name-symbols (objects)
  (:documentation "Outputs list with the name-symbols of the parameter objects in the input list.")
  (:method ((objects list))
    (let ((object-names (mapcar (function name) objects)))
      (mapcar (function (lambda (object-name)
                  (intern (upcase (param-case object-name)))))
              object-names))))

(defgeneric get-lambda-list (required-parameters optional-parameters operation-object)
  (:documentation "Create the lambda list to be included in the generated function.")
  (:method ((required-parameters list) (optional-parameters list) (operation-object operation))
    (flet ((default-parameter-p (item)
             (case-using (function string-equal) item
               (("OUTPUT" "SERVER" "CONTENT" "CONTENT-TYPE" "AUTHORIZATION" "HEADERS" "COOKIE" "QUERY")
                t)
               (otherwise nil))))
      (remove nil
              (append
               (remove-if (function default-parameter-p) (object-name-symbols required-parameters))
               (list (quote &key)
                     (list (intern "QUERY") (intern "*QUERY*"))
                     (list (intern "HEADERS") (intern "*HEADERS*"))
                     (list (intern "COOKIE") (intern "*COOKIE*"))
                     (list (intern "AUTHORIZATION") (intern "*AUTHORIZATION*"))
                     (list (intern "SERVER") (intern "*SERVER*"))
                     (list (intern "OUTPUT") (intern "*OUTPUT*")))
               (let ((content-types
                       (handler-case (hash-keys (content (request-body operation-object)))
                         (unbound-slot ()
                           nil))))
                 (cond ((and content-types
                             (null (length= 1 content-types)))
                        (list (intern "CONTENT") (intern "CONTENT-TYPE")))
                       ((or content-types (member "CONTENT"
                                                  (object-name-symbols optional-parameters)
                                                  :test (function string-equal)))
                        (list (intern "CONTENT")))
                       (t
                        nil)))
               (remove-if (function default-parameter-p) (object-name-symbols optional-parameters)))))))

(defgeneric get-description (operation-object)
  (:documentation "Extract documentation slots summary and description from operation object")
  (:method ((operation-object operation))
    (let ((operation-id
            (slot-value-safe operation-object (quote operation-id)))
          (summary
            (slot-value-safe operation-object (quote summary)))
          (description
            (slot-value-safe operation-object (quote description))))
      (trim (concat
             (when (param-case operation-id)
               (concat "Operation-id: " (param-case operation-id) (string #\Newline)))
             (when summary
               (concat "Summary: " summary (string #\Newline)))
             (when description
               (concat "Description: " description)))))))

(defgeneric schema-or-ref (api parameter)
  (:documentation "return the type from schema or referenced schema")
  (:method ((api openapi) (parameter parameter))
    (let* ((schema
             (slot-value-safe parameter (quote schema)))
           (schema-type
             (slot-value-safe schema (quote type)))
           (schema-one-of
             (slot-value-safe schema (quote one-of)))
           (schema-ref
             (slot-value-safe schema (quote ref))))
      (cond (schema-type
             (typecase schema-type
               (string (intern (upcase schema-type)))
               (vector (append (list (quote cl:or))
                               (mapcar (function (lambda (item)
                                         (intern (upcase item))))
                                       (coerce schema-type 'list))))))
            (schema-one-of
             (cons (quote cl:or)
                   (mapcar (function (lambda (items)
                             (intern (upcase (type items)))))
                           (coerce schema-one-of (quote list)))))
            (schema-ref
             (let ((type
                     (type (resolve-reference api schema-ref))))
               (typecase type
                 (string (intern (upcase type)))
                 (vector (append (list (quote cl:or))
                                 (mapcar (function (lambda (item)
                                           (intern (upcase item))))
                                         (coerce type 'list)))))))))))

(defgeneric assure-required (api required-parameters)
  (:documentation "Generate code for run-time type checking of required arguments")
  (:method ((api openapi) (required-parameter parameter))
    `(assuref ,(intern (upcase (param-case (name required-parameter))))
              ,(schema-or-ref api required-parameter)))
  (:method ((api openapi) (required-parameters list))
    (mapcar (function (lambda (parameter)
              (funcall (function assure-required) api parameter)))
            required-parameters)))

(defgeneric assure-optional (api optional-parameter)
  (:documentation "Generate code for run-time type checking of optional arguments.
This only happens, if arguments supplied.")
  (:method ((api openapi) (optional-parameter parameter))
    (let ((type
            (schema-or-ref api optional-parameter))
          (parameter-symbol
            (intern (upcase (param-case (name optional-parameter))))))
      (when (and type
                 (cl:not (string-equal "content-type" parameter-symbol)))
        `(when ,parameter-symbol
           (assuref
            ,parameter-symbol
            ,type)))))
  (:method ((api openapi) (optional-parameter list))
    (mapcar (function (lambda (parameter)
              (funcall (function assure-optional) api parameter)))
            optional-parameter)))

(defgeneric path-list (uri-path path)
  (:documentation "Convert path string into a list of strings and symbols")
  (:method ((uri-path string) (path string))
    (remove-if (function emptyp)
               (cons (string-right-trim "/" uri-path)
                     (mapcar
                      (function (lambda (sequence)
                        (if (starts-with-p "{" sequence)
                            (intern (upcase
                                     (param-case
                                      (substring 1
                                                 (1- (length sequence))
                                                 sequence))))
                            sequence)))
                      (split " "
                             (replace-using (list "/" " / "
                                                  "." " . ")
                                            path))))))
  (:method ((uri-path null) (path string))
    (path-list "" path)))

(defgeneric path-list-stringified (api path-list parameter-list)
  (:documentation "Get a list where symbols that will have a value of type strings are untouched, while
symbols will have numbers values are converted into strings at run time.")
  (:method ((api openapi) (path-list list) (parameter-list list))
    (flet ((get-parameter-by-name (name parameters)
             (remove nil
                     (mapc (function (lambda (parameter)
                             (when (string-equal (param-case (slot-value parameter (quote name)))
                                                 name)
                               (return-from get-parameter-by-name parameter))))
                           parameters))))
      (remove-if (function (lambda (item)
                   (when (typep item (quote string))
                     (emptyp item))))
                 (mapcar (function (lambda (item)
                           (typecase item
                             (symbol (case (schema-or-ref api
                                                          (get-parameter-by-name (symbol-name item)
                                                                                 parameter-list))
                                       ((number integer)
                                        `(write-to-string ,item))
                                       (otherwise
                                        item)))
                             (string item))))
                         path-list)))))

(defgeneric get-path (api path parameters primary-uri)
  (:documentation "generate path list")
  (:method ((api openapi) (path string) (parameters list) (primary-uri string))
    (let ((path-list
            (concat-strings
             (path-list-stringified api
                                    (path-list (uri-path (uri primary-uri))
                                               path)
                                    parameters))))
      (case (length path-list)
        (1 (car path-list))
        (otherwise `(concat ,@path-list))))))

(defgeneric get-query (parameters)
  (:documentation "Generate query (if there are parameters)")
  (:method ((parameters list))
    (let ((generated-list
            (gen-alist (mapcar (function name)
                               (get-parameter-type "query" parameters)))))
      (case (length generated-list)
        (0 `(remove-empty-values
             (when ,(intern "QUERY") ,(intern "QUERY"))))
        (otherwise `(remove-empty-values
                     (append (list ,@generated-list)
                             (when ,(intern "QUERY") ,(intern "QUERY")))))))))

(defgeneric get-headers (parameters operation)
  (:documentation "Generate code for run-time header checking. Headers are only send, if they are supplied.")
  (:method ((parameters list) (operation operation))
    (let ((content-type-list
            (let ((content-types
                    (handler-case (hash-keys (content (request-body operation)))
                      (unbound-slot ()
                        nil))))
              (case (length content-types)
                (0 nil)
                (1 `(cons "Content-Type" ,(car content-types)))
                (otherwise `(cons "Content-Type"
                                  (cond (,(intern "CONTENT-TYPE")
                                         (if (find ,(intern "CONTENT-TYPE")
                                                   (quote ,content-types)
                                                   :test (function string-equal))
                                             ,(intern "CONTENT-TYPE")
                                             (cl:warn "The body type ~A is not mentioned. Valid content-types are: ~A"
                                                      ,(intern "CONTENT-TYPE") ,(unwords content-types))))
                                        (t
                                         ,(car content-types))))))))
          (generated-alist
            (gen-alist
             (remove "content-type"
                     (mapcar (function name) (get-parameter-type "header" parameters))
                     :test (function string-equal))))
          (standard-headers
            `((cl:cons "Authorization" ,(intern "AUTHORIZATION"))
                   (cl:cons "cookie" ,(intern "COOKIE")))))
      (when content-type-list
        (push content-type-list standard-headers))
      (when generated-alist
        (setf standard-headers (append generated-alist standard-headers)))
      `(remove-empty-values
        (append (list ,@standard-headers)
                (when ,(intern "HEADERS") ,(intern "HEADERS")))))))

(defgeneric generate-function (api path operation-type)
  (:documentation "Generate functions for all types of http request")
  (:method ((api openapi) (path string) (operation-type symbol))
    (let* ((path-object      (gethash path (paths api)))
           (operation-object (slot-value path-object operation-type))
           (all-parameters   (collect-parameters api path-object operation-type))
           (required-params  (get-required-parameter all-parameters :required t))
           (optional-params  (get-required-parameter all-parameters :required nil))
           (lambda-list      (get-lambda-list required-params optional-params operation-object))
	   (description      (get-description operation-object))
           (primary-uri      (get-primary-uri api))
           (uri-path         (get-path api path all-parameters primary-uri))
           (uri-query        (get-query all-parameters)))
      `(defun ,(intern (function-name path operation-type :param-case nil)) ,lambda-list
	 ,description
         ,@(assure-required api required-params)
         ,@(assure-optional api optional-params)
         (let* ((,(intern "SERVER-URI")
                  (uri (or ,(intern "SERVER")
                           ,primary-uri)))
                (,(intern "RESPONSE")
                  (request
                   (render-uri
	            (make-uri :scheme (uri-scheme ,(intern "SERVER-URI"))
                              :host (uri-host ,(intern "SERVER-URI"))
                              :path ,uri-path
                              :query ,uri-query))
	           ,@(if (member (intern "CONTENT") lambda-list)
                         `(:content ,(intern "CONTENT"))
                         (values))
	           :method (quote ,(intern (symbol-name operation-type)))
	           :headers ,(get-headers all-parameters operation-object))))
           (case ,(intern "OUTPUT")
             (:json ,(intern "RESPONSE"))
             (:hash-table (parse ,(intern "RESPONSE")))))))))
