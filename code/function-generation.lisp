(cl:in-package #:openapi-generator)

(defgeneric get-primary-uri (api)
  (:documentation "Return first server uri")
  (:method ((api openapi))
    (let ((servers (slot-value-safe api 'servers)))
      (etypecase servers
        (null
         "")
        (string
         servers)
        (vector
         (if (= 0 (length servers))
             ""
             (let ((first-server-object
                     (car (coerce servers (quote list)))))
               (str:trim-right
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
                         first-url))))
                :char-bag "/"))))))))

(defgeneric collect-parameters (path operation)
  (:documentation "Collect all parameters belong to an api a path and operation.")
  (:method ((path path) (operation symbol))
    (let* ((path-parameters
             (slot-value-safe path (quote parameters)))
           (operation-parameters
             (slot-value-safe (slot-value path operation) (quote parameters)))
           (parameters-with-duplicates
             (remove nil (append (when path-parameters
                                   path-parameters)
                                 (when operation-parameters
                                   operation-parameters))))
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
                               parameters-with-duplicates)))))
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

(defmethod get-required-parameter ((parameters list))
  "Collect required parameter from list of parameters"
  (let ((required-parameter
          nil))
    (mapc (function (lambda (parameter)
            (when (let ((required
                          (slot-value-safe parameter (quote required))))
                    (or (eql (quote true) required)
                        (eql (quote t) required)))
              (setf required-parameter (append required-parameter (list parameter))))))
          parameters)
    required-parameter))

(defmethod get-optional-parameter ((parameters list))
  "Collect optional parameter from list of parameters"
  (let ((optional-parameter
          nil))
    (mapc (function (lambda (parameter)
            (unless (let ((required (slot-value-safe parameter (quote required))))
                      (or (eql (quote true) required)
                          (eql (quote t) required)))
              (setf optional-parameter (append optional-parameter (list parameter))))))
          parameters)
    optional-parameter))


(defgeneric get-uri-type (uri)
  (:documentation "")
  (:method ((uri string))
    (let* ((uri-path (uri-path (uri uri)))
           (uri-path-length (length uri-path))
           (file-type (substring (- uri-path-length 4)
                                 uri-path-length
                                 uri-path)))
      (case-using (function string-equal) file-type
        (("yml" "yaml")
         "yaml")
        ("json"
         "json")
        (otherwise
         (warn "~A is the wrong file type. Acceptable values are yaml, yml, or json." file-type))))))

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

(defgeneric get-lambda-list (required-parameters optional-parameters operation-object
                             json-body-schema)
  (:documentation "Create the lambda list to be included in the generated function.")
  (:method ((required-parameters list) (optional-parameters list) (operation-object operation)
            json-body-schema)
    (flet ((default-parameter-p (item)
             (when (typep item (quote string-designator))
               (case-using (function string-equal) item
                 (("PARSE" "SERVER" "AUTHORIZATION" "BEARER" "HEADERS" "COOKIE" "QUERY")
                  t)
                 (otherwise nil)))))
      (let ((title-symbol (slot-value-safe json-body-schema (quote title)))
            (content-types
              (handler-case (hash-keys (content (request-body operation-object)))
                (unbound-slot ()
                  nil))))
        (remove-duplicates
         (remove-if
          (function default-parameter-p)
          (remove nil
                  (append
                   (object-name-symbols required-parameters)
                   (list (quote &key)
                         (list (intern "QUERY") (intern "*QUERY*"))
                         (list (intern "HEADERS") (intern "*HEADERS*"))
                         (list (intern "COOKIE") (intern "*COOKIE*"))
                         (list (intern "AUTHORIZATION") (intern "*AUTHORIZATION*"))
                         (list (intern "BEARER") (intern "*BEARER*"))
                         (list (intern "SERVER") (intern "*SERVER*"))
                         (list (intern "PARSE") (intern "*PARSE*")))
                   (when title-symbol (list (intern-param title-symbol)))
                   (when json-body-schema
                     (let ((properties (slot-value-safe json-body-schema (quote properties))))
                       (when properties
                         (intern-param (hash-keys properties)))))
                   (cond ((and content-types
                               (null (length= 1 content-types)))
                          (list (intern "CONTENT") (intern "CONTENT-TYPE")))
                         ((or content-types (member "CONTENT"
                                                    (object-name-symbols optional-parameters)
                                                    :test (function string-equal)))
                          (list (intern "CONTENT")))
                         (t
                          nil))
                   (object-name-symbols optional-parameters)))))))))

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

(defgeneric parameter-schema-type (parameter)
  (:documentation "Return the parameter type from schema")
  (:method ((parameter parameter))
    (let* ((schema
             (slot-value-safe parameter (quote schema)))
           (schema-type
             (slot-value-safe schema (quote type)))
           (schema-one-of
             (slot-value-safe schema (quote one-of))))
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
                             (intern (upcase (slot-value items (quote type))))))
                           (coerce schema-one-of (quote list)))))))))

(defgeneric assure-required (required-parameters)
  (:documentation "Generate code for run-time type checking of required arguments")
  (:method ((required-parameter parameter))
    (let ((*print-case* :downcase)
          (*package* (find-package 'dummy-printing-package))
          (types
            (parameter-schema-type required-parameter)))
      (cl:read-from-string (cl:format nil "~{~W~% ~}" (cl:list `(assuref ,(cl:intern (upcase (param-case (name required-parameter))))
                                                                         ,(if (consp types)
                                                                              (mapcar (lambda (type)
                                                                                        (intern (symbol-name type)
                                                                                                :common-lisp))
                                                                                      types)
                                                                              (intern (symbol-name types)
                                                                                      :common-lisp))))))))
  (:method ((required-parameters list))
    (mapcar (function (lambda (parameter)
              (funcall (function assure-required) parameter)))
            required-parameters)))

(defgeneric assure-optional (optional-parameter)
  (:documentation "Generate code for run-time type checking of optional arguments.
This only happens, if arguments supplied.")
  (:method ((optional-parameter parameter))
    (let ((types
            (parameter-schema-type optional-parameter))
          (parameter-symbol
            (intern (upcase (param-case (name optional-parameter)))))
          (*print-case* :downcase)
          (*package* (find-package 'dummy-printing-package)))
      (cl:when (cl:and types
                       (cl:not (cl:string-equal "content-type" parameter-symbol)))
        (cl:read-from-string
         (cl:format nil "~{~W~% ~}"
                    (cl:list
                     `(cl:when ,parameter-symbol
                        (serapeum:assuref
                         ,parameter-symbol
                         ,(if (consp types)
                              (mapcar (lambda (type)
                                        (intern (symbol-name type)
                                                :cl))
                                      types)
                              (intern (symbol-name types)
                                      :cl))))))))))
  (:method ((optional-parameter list))
    (mapcar (function (lambda (parameter)
              (funcall (function assure-optional) parameter)))
            optional-parameter)))

(defgeneric path-list (path)
  (:documentation "Convert path string into a list of strings and symbols")
  (:method ((path string))
    (remove-if (function emptyp)
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

(defgeneric path-list-stringified (path-list parameter-list)
  (:documentation "Get a list where symbols that will have a value of type strings are untouched, while
symbols will have numbers values are converted into strings at run time.")
  (:method ((path-list list) (parameter-list list))
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
                             (symbol
                              (let ((parameter-schema-type
                                      (parameter-schema-type
                                       (get-parameter-by-name (symbol-name item)
                                                              parameter-list))))
                                (if (consp parameter-schema-type)
                                    (list 'format nil "~A" item)
                                    (case-using (function string-equal)
                                        parameter-schema-type
                                      ((number integer)
                                       (list 'write-to-string item))
                                      (otherwise
                                       item)))))
                             (string item))))
                         path-list)))))

(defgeneric get-path (path parameters)
  (:documentation "generate path list")
  (:method ((path string) (parameters list))
    (let ((path-list
            (concat-strings
             (path-list-stringified (path-list path)
                                    parameters))))
      (case (length path-list)
        (1 (car path-list))
        (otherwise `(concat ,@path-list))))))

(defgeneric get-query (parameters)
  (:documentation "Generate query (if there are parameters)")
  (:method ((parameters list))
    (let ((generated-list
            (mapcar (lambda (parameter)
                      (let ((interned (intern (upcase (param-case (name parameter))))))
                        `(cons ,(name parameter)
                               ,(if (eq 'boolean (parameter-schema-type parameter))
                                    `(if ,interned
                                         "true"
                                         "false")
                                    interned))))
                    (get-parameter-type "query" parameters))))
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
            `((when ,(intern "AUTHORIZATION")
                (cl:cons "Authorization" ,(intern "AUTHORIZATION")))
              (cl:cons "cookie" ,(intern "COOKIE")))))
      (when content-type-list
        (push content-type-list standard-headers))
      (when generated-alist
        (setf standard-headers (append generated-alist standard-headers)))
      `(remove-empty-values
        (append (list ,@standard-headers)
                (when ,(intern "HEADERS") ,(intern "HEADERS")))))))

(defmethod json-body-schema ((operation operation))
  "Return application/json media type or nil."
  (let ((request-body (slot-value-safe operation (quote request-body))))
    (when request-body
      (gethash "application/json"
               (content request-body)))))

(defgeneric type-conversion (json-type)
  (:documentation "Convert json-type string or list of strings to lisp types.")
  (:method ((json-type string))
    (case-using (function string-equal) json-type
      (null (list (quote or) (quote json-null) (quote null)))
      (number (list (quote or) (quote json-number) (quote number)))
      (boolean (list (quote or) (quote json-true) (quote json-false) (quote null) (quote t)))
      (string (quote string))
      (array (list (quote or) (quote json-array) (quote list)))
      (object (list (quote or) (quote json-object) (quote hash-table)))))
  (:method ((json-type vector))
    (let ((type-list
            (mapcar (function type-conversion)
                    (coerce json-type (quote list)))))
      (if type-list
          (cons (quote or)
                type-list)
          t)))
  (:method ((json-type null))
    t))

(defmethod json-content ((schema schema) &key (check-type t))
  "Generate the code to validate request-body or generate it according to the spec."
  (let ((intern-content
          (intern "CONTENT"))
        (title
          (intern-param (slot-value-safe schema (quote title))))
        (properties
          (slot-value-safe schema (quote properties)))
        (required-properties
          (slot-value-safe schema (quote required)))
        (type-list
          (type-conversion (slot-value-safe schema (quote type)))))
    (declare (symbol title) (type (or hash-table null) properties)
             (type (or cons null) required-properties))
    (labels ((required (name schema)
               (declare (schema schema) (string name))
               `((com.inuoe.jzon:write-key* ,name)
                 (com.inuoe.jzon:write-value*
                  ,(if check-type
                       `(progn (assuref ,(intern-param name)
                                        ,(type-conversion (slot-value-safe schema (quote type))))
                               (or (ignore-errors (parse ,(intern-param name)))
                                   ,(intern-param name)))
                       (intern-param name))
                  )))
             (optional (name schema)
               (declare (schema schema) (string name))
               (let ((name-symbol (intern-param name)))
                 `((when ,name-symbol
                     (com.inuoe.jzon:write-key* ,name)
                     (com.inuoe.jzon:write-value*
                      ,(if check-type
                           `(assuref ,name-symbol
                                     ,(type-conversion (slot-value-safe schema (quote type))))
                           name-symbol))))))
             (optional-or-required (property)
               (declare (string property))
               (if (member property required-properties :test (function string-equal))
                   (funcall (function required) property (gethash property properties))
                   (funcall (function optional) property (gethash property properties)))))
      (remove nil `(cond (,intern-content
                          ,(if check-type
                               (if (and (atom type-list)
                                        (string-equal type-list (quote string)))
                                   `(assuref ,intern-content string)
                                   `(if (not (stringp (assuref ,intern-content ,type-list)))
                                        (stringify ,intern-content)
                                        ,intern-content))
                               `(stringify ,intern-content)))
                         ,(when title
                            `(,title
                              ,(if check-type
                                   (if (and (atom type-list)
                                            (string-equal type-list (quote string)))
                                       `(assuref ,title string)
                                       `(if (not (stringp (assuref ,title ,type-list)))
                                            (stringify ,title)
                                            ,title))
                                   `(stringify ,title))))
                         ,(when properties
                            (let ((property-names (delete "content"
                                                          (hash-keys properties)
                                                          :test (function string=))))
                              (declare (type (or cons null) property-names))
                              (when (> (list-length property-names) 0)
                                `((or ,@(intern-param
                                         property-names))
                                  (let ((,(intern "S") (make-string-output-stream)))
                                    (declare (stream ,(intern "S")))
                                    (com.inuoe.jzon:with-writer* (:stream ,(intern "S") :pretty t)
                                      (com.inuoe.jzon:with-object*
                                        ,@(mapcan (function optional-or-required)
                                                  property-names)))
                                    (let ((output
                                            (get-output-stream-string ,(intern "S"))))
                                      (declare (string output))
                                      (unless (string= "{}" output)
                                        output))))))))))))

(defmethod get-response-type ((operation operation))
  "Get response type. Return value can be either :json or nil"
  (maphash (lambda (key value)
             (declare (ignore key))
             (let ((content (slot-value-safe value (quote content))))
               (declare (type (or null hash-table) content))
               (when content
                 (let ((hash-keys
                         (hash-keys content)))
                   (declare (list hash-keys))
                   (mapc (lambda (item)
                           (when (str:containsp "application/json" item)
                             (return-from get-response-type :json)))
                         hash-keys)))))
           (slot-value operation (quote responses))))

(defgeneric generate-function (api path operation-type &key check-type)
  (:documentation "Generate functions for all types of http request")
  (:method ((api openapi) (path string) (operation-type symbol) &key (check-type t))
    (let* ((path-object      (gethash path (paths api)))
           (operation-object (slot-value path-object operation-type))
           (all-parameters   (collect-parameters path-object operation-type))
           (required-params  (get-required-parameter all-parameters))
           (optional-params  (get-optional-parameter all-parameters))
           (json-body        (json-body-schema operation-object))
           (json-body-schema (slot-value-safe json-body (quote schema)))
           (lambda-list      (get-lambda-list required-params optional-params operation-object
                                              json-body-schema))
           (response-type    (get-response-type operation-object))
           (description      (get-description operation-object))
           (primary-uri      (get-primary-uri api))
           (uri-path         (get-path path all-parameters))
           (uri-query        (get-query all-parameters))
           (intern-response  (intern "RESPONSE"))
           (intern-content   (intern "CONTENT"))
           (intern-server-uri (intern "SERVER-URI")))
      `(defun ,(intern (function-name path operation-type :param-case nil)) ,lambda-list
         ,description
         ,@(when check-type
             (append (assure-required required-params)
                     (assure-optional optional-params)))
         (let* ((,intern-server-uri
                  (uri (or ,(intern "SERVER")
                           ,primary-uri)))
                (,intern-response
                  (request
                   (render-uri
                    (make-uri :scheme (uri-scheme ,intern-server-uri)
                              :host (uri-host ,intern-server-uri)
                              :port (uri-port ,intern-server-uri)
                              :path (concat (uri-path ,intern-server-uri) ,uri-path)
                              :query ,uri-query))
                   ,@(if (member intern-content lambda-list)
                         `(:content ,(if json-body
                                         (json-content json-body-schema :check-type check-type)
                                         intern-content))
                         (values))
                   :method (quote ,(intern (symbol-name operation-type)))
                   :bearer-auth ,(intern "BEARER")
                   :headers ,(get-headers all-parameters operation-object))))
           ,(if response-type
                (case response-type
                  (:json `(if ,(intern "PARSE")
                              (parse ,intern-response)
                              ,intern-response))
                  (otherwise `(case ,(intern "PARSE")
                                (:json (parse ,intern-response))
                                (otherwise ,intern-response))))
                `(case ,(intern "PARSE")
                   (:json (parse ,intern-response))
                   (otherwise ,intern-response))))))))

"delete/admin/users/{username}/keys/{id}"
