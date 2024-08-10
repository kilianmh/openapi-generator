(cl:in-package #:openapi-generator)
(declaim (optimize (speed 0) (space 0) (debug 3)))

(defun openapi-version-p (openapi-version)
  (member openapi-version
          (list "2.0" "3.0.0" "3.0.1" "3.0.2" "3.0.3" "3.1.0")
          :test (function string=)))
(deftype openapi-version ()
  '(satisfies openapi-version-p))

(define-json-class openapi nil
  (("openapi" :string
              :initarg :openapi
              :initform "3.1.0"
              :accessor openapi
              :required t
              :type openapi-version
              :documentation "This string MUST be the version number of the OpenAPI Specification that the OpenAPI document uses. The openapi field SHOULD be used by tooling to interpret the OpenAPI document. This is not related to the API info.version string.")
   ("info" info
           :initarg :info
           :type info
           :accessor info
           :required t
           :documentation "Provides metadata about the API. The metadata MAY be used by tooling as required.")
   ("servers" :any
              :initarg :servers
              :accessor servers
              :documentation "An array of Server Objects, which provide connectivity information to a target server. If the servers property is not provided, or is an empty array, the default value would be a Server Object with a url value of /.")
   ("security" :any
               :initarg :security
               :accessor security
               :documentation "A declaration of which security mechanisms can be used across the API. The list of values includes alternative security requirement objects that can be used. Only one of the security requirement objects need to be satisfied to authorize a request. Individual operations can override this definition. To make security optional, an empty security requirement ({}) can be included in the array.")
   ("paths" (:hash-table path)
            :initarg :paths
            :accessor paths
            :documentation "The available paths and operations for the API.
Holds the relative paths to the individual endpoints and their operations. The path is appended to the URL from the Server Object in order to construct the full URL. The Paths MAY be empty, due to Access Control List (ACL) constraints.")
   ("tags" (:list tag)
           :initarg :tags
           :accessor tags
           :documentation "A list of tags used by the document with additional metadata. The order of the tags can be used to reflect on their order by the parsing tools. Not all tags that are used by the Operation Object must be declared. The tags that are not declared MAY be organized randomly or based on the tools’ logic. Each tag name in the list MUST be unique.")
   ("externalDocs" external-documentation
                   :initarg :externaldocs
                   :accessor external-documentation
                   :documentation "Additional external documentation.")
   ("components" components
                 :initarg :components
                 :accessor components
                 :documentation "An element to hold various schemas for the document.")
   ("schema" schema
             :initarg :schema
             :accessor schema))
  (:documentation "This is the root object of the OpenAPI document."))

(define-json-class openapi-3.0 (openapi) nil)

(define-json-class openapi-3.1 (openapi)
  ((json-schema-dialect "jsonSchemaDialect"
                        :string
                        :initarg :json-schema-dialect
                        :accessor json-schema-dialect
                        :documentation "The default value for the $schema keyword within Schema Objects contained within this OAS document. This MUST be in the form of a URI.")
   ("webhooks" (:hash-table path)
               :initarg :webhooks
               :accessor webhooks
               :documentation "The incoming webhooks that MAY be received as part of this API and that the API consumer MAY choose to implement. Closely related to the callbacks feature, this section describes requests initiated other than by an API call, for example by an out of band registration. The key name is a unique string to refer to each webhook, while the (optionally referenced) Path Item Object describes a request that may be initiated by the API provider and the expected responses. An example is available.")))

(defmethod print-object ((api openapi) stream)
  (print-unreadable-object (api stream :type t :identity t)
    (cl:format stream "~A, version: ~A"
               (title (info api))
               (version (info api)))))

(define-json-class info nil
  (("title" :string
            :initarg :title
            :accessor title
            :documentation "The title of the API."
            :required t)
   ("summary" :string
              :initarg :summary
              :accessor summary
              :documentation "A short summary of the API.")
   ("description" :string
                  :initarg :description
                  :accessor description
                  :documentation "A description of the API. CommonMark syntax MAY be used for rich text representation.")
   (terms-of-service "termsOfService" :string
                     :initarg :terms-of-service
                     :accessor terms-of-service
                     :documentation "A URL to the Terms of Service for the API. This MUST be in the form of a URL.")
   ("contact" contact
              :initarg :contact
              :accessor contact
              :documentation "The contact information for the exposed API.")
   ("license" :any
              :initarg :license
              :accessor license
              :documentation "The license information for the exposed API.")
   ("version" :string
              :initarg :version
              :accessor version
              :required t
              :documentation "The version of the OpenAPI document (which is distinct from the OpenAPI Specification version or the API implementation version)."))
  (:documentation "The object provides metadata about the API. The metadata MAY be used by the clients if needed, and MAY be presented in editing or documentation generation tools for convenience."))

(defmethod print-object ((object info) stream)
  (print-unreadable-object (object stream)
    (cl:format stream
               (str:concat
                (if-let (title
                         (title object))
                  (concat "(title " title ")")
                  "title should be supplied")
                (when-let (version
                           (slot-value-safe object (quote version)))
                  (concat " (version " version ")"))
                (when-let (summary
                           (slot-value-safe object (quote summary)))
                  (concat " (summary " summary ")"))
                (when-let (description
                           (slot-value-safe object (quote description)))
                  (concat " (description " description ")"))))))

(define-json-class contact nil
  (("name" :string
           :initarg :name
           :accessor name
           :documentation "The identifying name of the contact person/organization.")
   ("url" :string
          :initarg :url
          :accessor url
          :documentation "The URL pointing to the contact information. This MUST be in the form of a URL.")
   ("email" :string
            :initarg :email
            :accessor email
            :documentation "The email address of the contact person/organization. This MUST be in the form of an email address."))
  (:documentation "Contact information for the exposed API."))

(define-json-class license nil
  (("name" :string
           :initarg :name
           :accessor name
           :required t
           :documentation "The license name used for the API.")
   ("identifier" :string
                 :initarg :identifier
                 :accessor identifier
                 :documentation "An SPDX license expression for the API. The identifier field is mutually exclusive of the url field.")
   ("url" :string
          :initarg :url
          :accessor url
          :documentation "A URL to the license used for the API. This MUST be in the form of a URL. The url field is mutually exclusive of the identifier field."))
  (:documentation "License information for the exposed API."))

(defmethod print-object ((object license) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cl:format stream "~A"
               (title (info object)))))

(define-json-class server nil
  (("url" :string
          :initarg :url
          :accessor url
          :required t
          :documentation "A URL to the target host. This URL supports Server Variables and MAY be relative, to indicate that the host location is relative to the location where the OpenAPI document is being served. Variable substitutions will be made when a variable is named in {brackets}.")
   ("description" :string
                  :initarg :description
                  :accessor description
                  :documentation "An optional string describing the host designated by the URL. CommonMark syntax MAY be used for rich text representation.")
   ("variables" (:hash-table server-variable)
                :initarg :variables
                :accessor variables
                :documentation "A map between a variable name and its value. The value is used for substitution in the server’s URL template."))
  (:documentation "An object representing a Server."))

(defmethod print-object ((object server) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cl:format stream "url: ~A" (url object))))

(define-json-class server-variable nil
  (("enum" (:list :string)
           :initarg :server-variable
           :accessor enum
           :documentation "An enumeration of string values to be used if the substitution options are from a limited set. The array MUST NOT be empty.")
   ("default" :string
              :initarg :default
              :required t
              :accessor default
              :documentation "The default value to use for substitution, which SHALL be sent if an alternate value is not supplied. Note this behavior is different than the Schema Object’s treatment of default values, because in those cases parameter values are optional. If the enum is defined, the value MUST exist in the enum’s values.")
   ("description" :string
                  :initarg :description
                  :accessor description
                  :documentation "An optional description for the server variable. CommonMark syntax MAY be used for rich text representation."))
  (:documentation "An object representing a Server Variable for server URL template substitution."))

(defmethod print-object ((object server-variable) stream)
  (print-unreadable-object (object stream :type t)
    (cl:format stream "default: ~A" (default object))))

(define-json-class components nil
  (("schemas" (:hash-table schema)
              :initarg :schemas
              :accessor schemas
              :documentation "An object to hold reusable Schema Objects.")
   ("responses" (:hash-table response)
                :initarg :responses
                :accessor responses
                :documentation "An object to hold reusable Response Objects.")
   ("parameters" (:hash-table parameter)
                 :initarg :parameters
                 :accessor parameters
                 :documentation "An object to hold reusable Parameter Objects.")
   ("examples" (:hash-table example)
               :initarg :examples
               :accessor examples
               :documentation "An object to hold reusable Example Objects.")
   (request-bodies "requestBodies"
                   (:hash-table request-body)
                   :initarg :request-bodies
                   :accessor request-bodies
                   :documentation "An object to hold reusable Request Body Objects.")
   ("headers" (:hash-table header)
              :initarg :headers
              :accessor headers
              :documentation "An object to hold reusable Header Objects.")
   (security-schemes "securitySchemes"
                     (:hash-table security-scheme)
                     :initarg :security-schemes

                     :accessor security-schemes
                     :documentation "An object to hold reusable Security Scheme Objects.")
   ("links" (:hash-table link)
            :initarg :links
            :accessor links
            :documentation "An object to hold reusable Link Objects.")
   ("callbacks" (:hash-table :any)
                :initarg :callbacks
                :accessor callbacks
                :documentation "An object to hold reusable Callback Objects.")
   (path-items "pathItems"
               (:hash-table :any)
               :initarg :path-items
               :accessor path-items
               :documentation "An object to hold reusable Path Item Object.")
   (reference "$ref" :string
              :initarg :reference
              :accessor reference))
  (:documentation "Holds a set of reusable objects for different aspects of the OAS. All objects defined within the components object will have no effect on the API unless they are explicitly referenced from properties outside the components object."))

(define-json-class path nil
  ((reference "$ref" :string
              :initarg :reference
              :accessor reference
              :documentation "Allows for a referenced definition of this path item. The referenced structure MUST be in the form of a Path Item Object. In case a Path Item Object field appears both in the defined object and the referenced object, the behavior is undefined. See the rules for resolving Relative References.")
   ("summary" :string
              :initarg :summary
              :accessor summary
              :documentation "An optional, string summary, intended to apply to all operations in this path.")
   ("description" :string
                  :initarg :description
                  :accessor description
                  :documentation "An optional, string description, intended to apply to all operations in this path. CommonMark syntax MAY be used for rich text representation.")
   ("get" operation
          :initarg :get
          :accessor path-get
          :documentation "A definition of a GET operation on this path.")
   ("head" operation
           :initarg :head
           :accessor head
           :documentation "A definition of a HEAD operation on this path.")
   ("options" operation
              :initarg :options
              :accessor options
              :documentation "A definition of a OPTIONS operation on this path.")
   ("trace" operation
            :initarg :trace
            :accessor path-trace
            :documentation "A definition of a TRACE operation on this path.")
   ("delete" operation
             :initarg :delete
             :accessor path-delete
             :documentation "A definition of a DELETE operation on this path.")
   ("put" operation
          :initarg :put
          :accessor put
          :documentation "A definition of a PUT operation on this path.")
   ("post" operation
           :initarg :post
           :accessor post
           :documentation "A definition of a POST operation on this path.")
   ("patch" operation
            :initarg :patch
            :accessor patch
            :documentation "A definition of a PATCH operation on this path.")
   ("parameters" (:list parameter)
                 :initarg :parameters
                 :accessor parameters
                 :documentation "A list of parameters that are applicable for all the operations described under this path. These parameters can be overridden at the operation level, but cannot be removed there. The list MUST NOT include duplicated parameters. A unique parameter is defined by a combination of a name and location. The list can use the Reference Object to link to parameters that are defined at the OpenAPI Object’s components/parameters.")
   ("servers" (:list server)
              :initarg :servers
              :accessor servers
              :documentation "An alternative server array to service all operations in this path."))
  (:documentation "Describes the operations available on a single path. A Path Item MAY be empty, due to ACL constraints. The path itself is still exposed to the documentation viewer but they will not know which operations and parameters are available."))

(defmethod print-object ((object path) stream)
  (print-unreadable-object (object stream)
    (cl:format stream "~A"
               (remove nil
                       (mapcar (function (lambda (operation-type)
                                 (when (slot-boundp object operation-type)
                                   operation-type)))
                               (moptilities:direct-slot-names 'path))))))

(define-json-class operation nil
  (("tags" (:list :string)
           :initarg :tags
           :accessor tags
           :documentation "A list of tags for API documentation control. Tags can be used for logical grouping of operations by resources or any other qualifier.")
   ("summary" :string
              :initarg :summary
              :accessor summary
              :documentation "A short summary of what the operation does.")
   ("description" :string
                  :initarg :description
                  :accessor description
                  :documentation "A verbose explanation of the operation behavior. CommonMark syntax MAY be used for rich text representation.")
   (external-documentation "externalDocs"
                           external-documentation
                           :initarg :external-documentation
                           :accessor external-documentation
                           :documentation "Additional external documentation for this operation.")
   (operation-id "operationId"
                 :string
                 :initarg :operation-id
                 :accessor operation-id
                 :documentation "Unique string used to identify the operation. The id MUST be unique among all operations described in the API. The operationId value is case-sensitive. Tools and libraries MAY use the operationId to uniquely identify an operation, therefore, it is RECOMMENDED to follow common programming naming conventions.")
   ("parameters"  (:list parameter)
                  :initarg :parameters
                  :accessor parameters
                  :documentation "A list of parameters that are applicable for this operation. If a parameter is already defined at the Path Item, the new definition will override it but can never remove it. The list MUST NOT include duplicated parameters. A unique parameter is defined by a combination of a name and location. The list can use the Reference Object to link to parameters that are defined at the OpenAPI Object’s components/parameters.")
   (request-body "requestBody"
                 request-body
                 :initarg :request-body
                 :accessor request-body
                 :documentation "The request body applicable for this operation. The requestBody is fully supported in HTTP methods where the HTTP 1.1 specification [RFC7231] has explicitly defined semantics for request bodies. In other cases where the HTTP spec is vague (such as GET, HEAD and DELETE), requestBody is permitted but does not have well-defined semantics and SHOULD be avoided if possible.")
   ("responses" (:hash-table response)
                :initarg :responses
                :accessor responses
                :documentation "The list of possible responses as they are returned from executing this operation.

A container for the expected responses of an operation. The container maps a HTTP response code to the expected response.

The documentation is not necessarily expected to cover all possible HTTP response codes because they may not be known in advance. However, documentation is expected to cover a successful operation response and any known errors.

The default MAY be used as a default response object for all HTTP codes that are not covered individually by the Responses Object.

The Responses Object MUST contain at least one response code, and if only one response code is provided it SHOULD be the response for a successful operation call.")
   ("callbacks" (:hash-table :any)
                :initarg :callbacks
                :accessor callbacks
                :documentation "A map of possible out-of band callbacks related to the parent operation. The key is a unique identifier for the Callback Object. Each value in the map is a Callback Object that describes a request that may be initiated by the API provider and the expected responses.")
   ("deprecated" :bool
                 :initarg :deprecated
                 :accessor deprecated
                 :documentation "Declares this operation to be deprecated. Consumers SHOULD refrain from usage of the declared operation. Default value is false.")
   ("security" :any
               :initarg :security
               :accessor security
               :documentation "A declaration of which security mechanisms can be used for this operation. The list of values includes alternative security requirement objects that can be used. Only one of the security requirement objects need to be satisfied to authorize a request. To make security optional, an empty security requirement ({}) can be included in the array. This definition overrides any declared top-level security. To remove a top-level security declaration, an empty array can be used.")
   ("servers" (:list server)
              :initarg :servers
              :accessor servers
              :documentation "An alternative server array to service this operation. If an alternative server object is specified at the Path Item Object or Root level, it will be overridden by this value."))
  (:documentation "Describes a single API operation on a path."))

(defmethod print-object ((object operation) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((operation-id
            (slot-value-safe object (quote operation-id))))
      (if operation-id
          (cl:format stream "operation-id: ~A" operation-id)
          (cl:format stream "")))))

(define-json-class external-documentation nil
  (("description" :string
                  :initarg :description
                  :accessor description
                  :documentation "A description of the target documentation. CommonMark syntax MAY be used for rich text representation.")
   ("url" :string
          :initarg :url
          :accessor url
          :required t
          :documentation "The URL for the target documentation. This MUST be in the form of a URL."))
  (:documentation "Allows referencing an external resource for extended documentation."))

(defmethod print-object ((object external-documentation) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cl:format stream "url: ~A" (url object))))

(define-json-class parameter ()
  (("name" :string
           :initarg :name
           :required t
           :accessor name
           :documentation "The name of the parameter. Parameter names are case sensitive.

    If in is \"path\", the name field MUST correspond to a template expression occurring within the path field in the Paths Object. See Path Templating for further information.
    If in is \"header\" and the name field is \"Accept\", \"Content-Type\" or \"Authorization\", the parameter definition SHALL be ignored.
    For all other cases, the name corresponds to the parameter name used by the in property.")
   ("in" :string
         :initarg :in
         :required t
         :accessor in
         :documentation "The location of the parameter. Possible values are \"query\", \"header\", \"path\" or \"cookie\".")
   ("description" :string
                  :initarg :description
                  :accessor description
                  :documentation "A brief description of the parameter. This could contain examples of use. CommonMark syntax MAY be used for rich text representation.")
   ("required" :bool
               :initarg :required
               :accessor required
               :documentation "Determines whether this parameter is mandatory. If the parameter location is \"path\", this property is REQUIRED and its value MUST be true. Otherwise, the property MAY be included and its default value is false.")
   ("deprecated" :bool
                 :initarg :deprecated
                 :accessor deprecated
                 :documentation "Specifies that a parameter is deprecated and SHOULD be transitioned out of usage. Default value is false.")
   (allow-empty-value "allowEmptyValue"
                      :bool
                      :initarg :allow-empty-value
                      :accessor allow-empty-value
                      :documentation "Sets the ability to pass empty-valued parameters. This is valid only for query parameters and allows sending a parameter with an empty value. Default value is false. If style is used, and if behavior is n/a (cannot be serialized), the value of allowEmptyValue SHALL be ignored. Use of this property is NOT RECOMMENDED, as it is likely to be removed in a later revision.")
   ("style" :string
            :initarg :style
            :accessor style
            :documentation "Describes how the parameter value will be serialized depending on the type of the parameter value. Default values (based on value of in): for query - form; for path - simple; for header - simple; for cookie - form.")
   ("explode" :bool
              :initarg :explode
              :accessor explode
              :documentation "When this is true, parameter values of type array or object generate separate parameters for each value of the array or key-value pair of the map. For other types of parameters this property has no effect. When style is form, the default value is true. For all other styles, the default value is false.")
   (allow-reserved "allowReserved"
                   :bool
                   :initarg :allow-reserved
                   :accessor allow-reserved
                   :documentation "Determines whether the parameter value SHOULD allow reserved characters, as defined by [RFC3986] :/?#[]@!$&'()*+,;= to be included without percent-encoding. This property only applies to parameters with an in value of query. The default value is false.")
   ("schema" schema
             :initarg :schema
             :accessor schema
             :documentation "The schema defining the type used for the parameter.") ;; only if in is body
   ("example" :any
              :initarg :example
              :accessor example
              :documentation "Example of the parameter’s potential value. The example SHOULD match the specified schema and encoding properties if present. The example field is mutually exclusive of the examples field. Furthermore, if referencing a schema that contains an example, the example value SHALL override the example provided by the schema. To represent examples of media types that cannot naturally be represented in JSON or YAML, a string value can contain the example with escaping where necessary.")
   ("examples" (:hash-table example)
               :initarg :examples
               :accessor examples
               :documentation "Examples of the parameter’s potential value. Each example SHOULD contain a value in the correct format as specified in the parameter encoding. The examples field is mutually exclusive of the example field. Furthermore, if referencing a schema that contains an example, the examples value SHALL override the example provided by the schema.")
   ("content" (:hash-table media-type)
              :initarg :content
              :accessor content
              :documentation "A map containing the representations for the parameter. The key is the media type and the value describes it. The map MUST only contain one entry.")
   (reference "$ref"
              :string
              :initarg :reference
              :accessor reference))
  (:documentation "Describes a single operation parameter.
A unique parameter is defined by a combination of a name and location.

Parameter Locations
There are four possible parameter locations specified by the in field:

    path - Used together with Path Templating, where the parameter value is actually part of the operation’s URL. This does not include the host or base path of the API. For example, in /items/{itemId}, the path parameter is itemId.
    query - Parameters that are appended to the URL. For example, in /items?id=###, the query parameter is id.
    header - Custom headers that are expected as part of the request. Note that [RFC7230] states header names are case insensitive.
    cookie - Used to pass a specific cookie value to the API.

The rules for serialization of the parameter are specified in one of two ways. For simpler scenarios, a schema and style can describe the structure and syntax of the parameter.

For more complex scenarios, the content property can define the media type and schema of the parameter. A parameter MUST contain either a schema property, or a content property, but not both. When example or examples are provided in conjunction with the schema object, the example MUST follow the prescribed serialization strategy for the parameter."))

(defmethod print-object ((object parameter) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((ref
            (slot-value-safe object (quote ref))))
      (if ref
          (cl:format stream "ref: ~A" ref)
          (cl:format stream "name: ~A, in ~A" (name object) (in object))))))

(define-json-class request-body nil
  ((reference "$ref"
              :string
              :initarg :reference
              :accessor reference)
   ("description" :string
                  :initarg :description
                  :documentation "A brief description of the request body. This could contain examples of use. CommonMark syntax MAY be used for rich text representation.")
   ("required" :bool
               :initarg :required
               :documentation "Determines if the request body is required in the request. Defaults to false.")
   ("content"  (:hash-table media-type)
               :initarg :content
               :accessor content
               :required t
               :documentation "The content of the request body. The key is a media type or media type range and the value describes it. For requests that match multiple keys, only the most specific key is applicable. e.g. text/plain overrides text/*"))
  (:documentation "Describes a single request body."))

(defmethod print-object ((object request-body) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((content-type
            (car (hash-keys (slot-value-safe object (quote content))))))
      (if content-type
          (cl:format stream "content-type: ~A" content-type)
          (cl:format stream "")))))

(define-json-class media-type nil
  (("schema" schema
             :initarg :schema
             :accessor schema
             :documentation "The schema defining the content of the request, response, or parameter.")
   ("example" :any
              :initarg :example
              :accessor example
              :documentation "Example of the media type. The example object SHOULD be in the correct format as specified by the media type. The example field is mutually exclusive of the examples field. Furthermore, if referencing a schema which contains an example, the example value SHALL override the example provided by the schema.")
   ("examples" (:hash-table example)
               :initarg :examples
               :accessor examples
               :documentation "Examples of the media type. Each example object SHOULD match the media type and specified schema if present. The examples field is mutually exclusive of the example field. Furthermore, if referencing a schema which contains an example, the examples value SHALL override the example provided by the schema.")
   ("encoding" (:hash-table encoding)
               :initarg :encoding
               :accessor encoding
               :documentation "A map between a property name and its encoding information. The key, being the property name, MUST exist in the schema as a property. The encoding object SHALL only apply to requestBody objects when the media type is multipart or application/x-www-form-urlencoded."))
  (:documentation "Each Media Type Object provides schema and examples for the media type identified by its key."))

(define-json-class encoding nil
  ((content-type "contentType"
                 :string
                 :initarg :content-type
                 :accessor content-type
                 :documentation "The Content-Type for encoding a specific property. Default value depends on the property type: for object - application/json; for array – the default is defined based on the inner type; for all other cases the default is application/octet-stream. The value can be a specific media type (e.g. application/json), a wildcard media type (e.g. image/*), or a comma-separated list of the two types.")
   ("headers" (:hash-table header)
              :initarg :headers
              :accessor headers
              :documentation "A map allowing additional information to be provided as headers, for example Content-Disposition. Content-Type is described separately and SHALL be ignored in this section. This property SHALL be ignored if the request body media type is not a multipart.")
   ("style" :string
            :initarg :style
            :accessor style
            :documentation "Describes how a specific property value will be serialized depending on its type. See Parameter Object for details on the style property. The behavior follows the same values as query parameters, including default values. This property SHALL be ignored if the request body media type is not application/x-www-form-urlencoded or multipart/form-data. If a value is explicitly defined, then the value of contentType (implicit or explicit) SHALL be ignored.")
   ("explode" :bool
              :initarg :explode
              :accessor explode
              :documentation "When this is true, property values of type array or object generate separate parameters for each value of the array, or key-value-pair of the map. For other types of properties this property has no effect. When style is form, the default value is true. For all other styles, the default value is false. This property SHALL be ignored if the request body media type is not application/x-www-form-urlencoded or multipart/form-data. If a value is explicitly defined, then the value of contentType (implicit or explicit) SHALL be ignored.")
   (allow-reserved "allowReserved" :bool
                   :accessor allow-reserved
                   :documentation "Determines whether the parameter value SHOULD allow reserved characters, as defined by [RFC3986] :/?#[]@!$&'()*+,;= to be included without percent-encoding. The default value is false. This property SHALL be ignored if the request body media type is not application/x-www-form-urlencoded or multipart/form-data. If a value is explicitly defined, then the value of contentType (implicit or explicit) SHALL be ignored."))
  (:documentation "A single encoding definition applied to a single schema property."))

(define-json-class response nil
  (("description" :string
                  :initarg :description
                  :accessor description
                  :required t
                  :documentation "A description of the response. CommonMark syntax MAY be used for rich text representation.")
   ("headers" (:hash-table header)
              :initarg :headers
              :accessor headers
              :documentation "Maps a header name to its definition. [RFC7230] states header names are case insensitive. If a response header is defined with the name \"Content-Type\", it SHALL be ignored.")
   ("content" (:hash-table media-type)
              :initarg :content
              :accessor content
              :documentation "Maps a header name to its definition. [RFC7230] states header names are case insensitive. If a response header is defined with the name \"Content-Type\", it SHALL be ignored.")
   ("links" (:hash-table link)
            :initarg :links
            :accessor links
            :documentation "A map of operations links that can be followed from the response. The key of the map is a short name for the link, following the naming constraints of the names for Component Objects.")
   (reference "$ref"
              :string
              :initarg :reference
              :accessor reference))
  (:documentation "Describes a single response from an API Operation, including design-time, static links to operations based on the response."))

(define-json-class example nil
  (("summary" :string
              :initarg :summary
              :accessor summary
              :documentation "Short description for the example.")
   ("description" :string
                  :initarg :description
                  :accessor description
                  :documentation "Long description for the example. CommonMark syntax MAY be used for rich text representation.")
   ("value" :any
            :initarg :value
            :accessor value
            :documentation "Embedded literal example. The value field and externalValue field are mutually exclusive. To represent examples of media types that cannot naturally represented in JSON or YAML, use a string value to contain the example, escaping where necessary.")
   (external-value "externalValue"
                   :string
                   :initarg :external-value
                   :accessor external-value
                   :documentation "A URI that points to the literal example. This provides the capability to reference examples that cannot easily be included in JSON or YAML documents. The value field and externalValue field are mutually exclusive. See the rules for resolving Relative References.")
   (reference "$ref"
              :string
              :initarg :reference
              :accessor reference)))

(define-json-class link nil
  ((operation-ref "operationRef"
                  :string
                  :initarg :operation-ref
                  :accessor operation-ref
                  :documentation "A relative or absolute URI reference to an OAS operation. This field is mutually exclusive of the operationId field, and MUST point to an Operation Object. Relative operationRef values MAY be used to locate an existing Operation Object in the OpenAPI definition. See the rules for resolving Relative References.")
   (operation-id "operationId"
                 :string
                 :initarg :operation-id
                 :accessor operation-id
                 :documentation "The name of an existing, resolvable OAS operation, as defined with a unique operationId. This field is mutually exclusive of the operationRef field.")
   ("parameters" (:hash-table :any)
                 :initarg :parameters
                 :accessor parameters
                 :documentation "A map representing parameters to pass to an operation as specified with operationId or identified via operationRef. The key is the parameter name to be used, whereas the value can be a constant or an expression to be evaluated and passed to the linked operation. The parameter name can be qualified using the parameter location [{in}.]{name} for operations that use the same parameter name in different locations (e.g. path.id).")
   (request-body "requestBody"
                 :any
                 :initarg :request-body
                 :accessor request-body
                 :documentation "A literal value or {expression} to use as a request body when calling the target operation.")
   ("description" :string
                  :initarg :description
                  :accessor description
                  :documentation "A description of the link. CommonMark syntax MAY be used for rich text representation.")
   ("server" server
             :initarg :server
             :accessor server
             :documentation "  server object to be used by the target operation.")
   (reference "$ref"
              :string
              :initarg :reference
              :accessor reference))
  (:documentation "The Link object represents a possible design-time link for a response. The presence of a link does not guarantee the caller’s ability to successfully invoke it, rather it provides a known relationship and traversal mechanism between responses and other operations.

Unlike dynamic links (i.e. links provided in the response payload), the OAS linking mechanism does not require link information in the runtime response.

For computing links, and providing instructions to execute them, a runtime expression is used for accessing values in an operation and using them as parameters while invoking the linked operation.

A linked operation MUST be identified using either an operationRef or operationId. In the case of an operationId, it MUST be unique and resolved in the scope of the OAS document. Because of the potential for name clashes, the operationRef syntax is preferred for OpenAPI documents with external references."))

(define-json-class header nil
  (("description" :string
                  :initarg :description
                  :accessor header
                  :documentation "A brief description of the header. This could contain examples of use. CommonMark syntax MAY be used for rich text representation.")
   ("required" :bool
               :initarg :required
               :accessor required
               :documentation "Determines whether this header is mandatory.")
   ("deprecated" :bool
                 :initarg :deprecated
                 :accessor deprecated
                 :documentation "Specifies that a parameter is deprecated and SHOULD be transitioned out of usage. Default value is false.")
   (allow-empty-value "allowEmptyValue"
                      :bool
                      :initarg :allow-empty-value
                      :accessor alllow-empty-value
                      :documentation "Sets the ability to pass empty-valued parameters. Default value is false. If style is used, and if behavior is n/a (cannot be serialized), the value of allowEmptyValue SHALL be ignored. Use of this property is NOT RECOMMENDED, as it is likely to be removed in a later revision.")
   ("schema" schema
             :initarg :schema
             :accessor schema
             :documentation "The schema defining the type used for the parameter.")
   ("content" (:hash-table media-type)
              :initarg :content
              :accessor content
              :documentation "A map containing the representations for the parameter. The key is the media type and the value describes it. The map MUST only contain one entry.")
   ("style" :string
            :initarg :style
            :accessor style
            :documentation "Describes how the parameter value will be serialized depending on the type of the parameter value. Default value: simple.")
   ("explode" :bool
              :initarg :explode
              :accessor explode
              :documentation "When this is true, parameter values of type array or object generate separate parameters for each value of the array or key-value pair of the map. For other types of parameters this property has no effect. When style is form, the default value is true. For all other styles, the default value is false.")
   (allow-reserved "allowReserved"
                   :bool
                   :initarg :allow-reserved
                   :accessor allow-reserved
                   :documentation "Determines whether the parameter value SHOULD allow reserved characters, as defined by [RFC3986] :/?#[]@!$&'()*+,;= to be included without percent-encoding. This property only applies to parameters with an in value of query. The default value is false.")
   ("example" :any
              :initarg :example
              :accessor example
              :documentation "Example of the parameter’s potential value. The example SHOULD match the specified schema and encoding properties if present. The example field is mutually exclusive of the examples field. Furthermore, if referencing a schema that contains an example, the example value SHALL override the example provided by the schema. To represent examples of media types that cannot naturally be represented in JSON or YAML, a string value can contain the example with escaping where necessary.")
   ("examples" (:hash-table example)
               :initarg :examples
               :accessor examples
               :documentation "Examples of the parameter’s potential value. Each example SHOULD contain a value in the correct format as specified in the parameter encoding. The examples field is mutually exclusive of the example field. Furthermore, if referencing a schema that contains an example, the examples value SHALL override the example provided by the schema.")
   (reference "$ref"
              :string
              :initarg :reference
              :accessor reference))
  (:documentation "The Header Object follows the structure of the Parameter Object with the following changes:
  1. name MUST NOT be specified, it is given in the corresponding headers map.
  2. in MUST NOT be specified, it is implicitly in header.
  3. All traits that are affected by the location MUST be applicable to a location of header (for example, style)."))

(define-json-class tag nil
  (("name" :string
           :initarg :name
           :accessor name
           :required t
           :documentation "The name of the tag.")
   ("description" :string
                  :initarg :description
                  :accessor description
                  :documentation "A description for the tag. CommonMark syntax MAY be used for rich text representation.")
   (external-documentation "externalDocs"
                           external-documentation
                           :initarg :external-documentation
                           :accessor external-documentation
                           :documentation "Additional external documentation for this tag."))
  (:documentation "Adds metadata to a single tag that is used by the Operation Object. It is not mandatory to have a Tag Object per tag defined in the Operation Object instances."))

(defmethod print-object ((object tag) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cl:format stream "~A" (name object))))

(define-json-class reference nil
  ((reference "$ref"
              :string
              :initarg :reference
              :accessor reference
              :required t
              :documentation "The reference identifier. This MUST be in the form of a URI.")
   ("summary" :string
              :initarg :summary
              :accessor summary
              :documentation "A short summary which by default SHOULD override that of the referenced component. If the referenced object-type does not allow a summary field, then this field has no effect.")
   ("description" :string
                  :initarg :description
                  :accessor description
                  :documentation "A description which by default SHOULD override that of the referenced component. CommonMark syntax MAY be used for rich text representation. If the referenced object-type does not allow a description field, then this field has no effect."))
  (:documentation "A simple object to allow referencing other components in the OpenAPI document, internally and externally.

The $ref string value contains a URI [RFC3986], which identifies the location of the value being referenced.

See the rules for resolving Relative References.

This object cannot be extended with additional properties and any properties added SHALL be ignored.

Note that this restriction on additional properties is a difference between Reference Objects and Schema Objects that contain a $ref keyword."))

(defmethod print-object ((object reference) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cl:format stream "~A" (slot-value object (quote ref)))))

(define-json-class schema nil
  ((schema "$schema" :any
           :initarg :schema
           :accessor schema
           :documentation "Specifying Schema Dialects

It is important for tooling to be able to determine which dialect or meta-schema any given resource wishes to be processed with: JSON Schema Core, JSON Schema Validation, OpenAPI Schema dialect, or some custom meta-schema.

The $schema keyword MAY be present in any root Schema Object, and if present MUST be used to determine which dialect should be used when processing the schema. This allows use of Schema Objects which comply with other drafts of JSON Schema than the default Draft 2020-12 support. Tooling MUST support the OAS dialect schema id, and MAY support additional values of $schema.

To allow use of a different default $schema value for all Schema Objects contained within an OAS document, a jsonSchemaDialect value may be set within the OpenAPI Object. If this default is not set, then the OAS dialect schema id MUST be used for these Schema Objects. The value of $schema within a Schema Object always overrides any default.

When a Schema Object is referenced from an external resource which is not an OAS document (e.g. a bare JSON Schema resource), then the value of the $schema keyword for schemas within that resource MUST follow JSON Schema rules")
   (vocabulary "$vocabulary"
               (:hash-table :any)
               :initarg :vocabulary
               :accessor vocabulary
               :documentation "The \"$vocabulary\" keyword is used in meta-schemas to identify the vocabularies available for use in schemas described by that meta-schema. It is also used to indicate whether each vocabulary is required or optional, in the sense that an implementation MUST understand the required vocabularies in order to successfully process the schema. Together, this information forms a dialect. Any vocabulary that is understood by the implementation MUST be processed in a manner consistent with the semantic definitions contained within the vocabulary. The value of this keyword MUST be an object. The property names in the object MUST be URIs (containing a scheme) and this URI MUST be normalized. Each URI that appears as a property name identifies a specific set of keywords and their semantics. The URI MAY be a URL, but the nature of the retrievable resource is currently undefined, and reserved for future use. Vocabulary authors MAY use the URL of the vocabulary specification, in a human-readable media type such as text/html or text/plain, as the vocabulary URI. Vocabulary documents may be added in forthcoming drafts. For now, identifying the keyword set is deemed sufficient as that, along with meta-schema validation, is how the current \"vocabularies\" work today. Any future vocabulary document format will be specified as a JSON document, so using text/html or other non-JSON formats in the meantime will not produce any future ambiguity. The values of the object properties MUST be booleans. If the value is true, then implementations that do not recognize the vocabulary MUST refuse to process any schemas that declare this meta-schema with \"$schema\". If the value is false, implementations that do not recognize the vocabulary SHOULD proceed with processing such schemas. The value has no impact if the implementation understands the vocabulary. Per 6.5, unrecognized keywords SHOULD be treated as annotations. This remains the case for keywords defined by unrecognized vocabularies. It is not currently possible to distinguish between unrecognized keywords that are defined in vocabularies from those that are not part of any vocabulary. The \"$vocabulary\" keyword SHOULD be used in the root schema of any schema document intended for use as a meta-schema. It MUST NOT appear in subschemas. The \"$vocabulary\" keyword MUST be ignored in schema documents that are not being processed as a meta-schema. This allows validating a meta-schema M against its own meta-schema M' without requiring the validator to understand the vocabularies declared by M.")

   ;; Base URI
   (id "$id"
       :string
       :initarg :id
       :accessor id
       :documentation "The \"$id\" keyword identifies a schema resource with its canonical [RFC6596] URI. Note that this URI is an identifier and not necessarily a network locator. In the case of a network-addressable URL, a schema need not be downloadable from its canonical URI. If present, the value for this keyword MUST be a string, and MUST represent a valid URI-reference [RFC3986]. This URI-reference SHOULD be normalized, and MUST resolve to an absolute-URI [RFC3986] (without a fragment), or to a URI with an empty fragment. The empty fragment form is NOT RECOMMENDED and is retained only for backwards compatibility, and because the application/schema+json media type defines that a URI with an empty fragment identifies the same resource as the same URI with the fragment removed. However, since this equivalence is not part of the RFC 3986 normalization process [RFC3986], implementers and schema authors cannot rely on generic URI libraries understanding it. Therefore, \"$id\" MUST NOT contain a non-empty fragment, and SHOULD NOT contain an empty fragment. The absolute-URI form MUST be considered the canonical URI, regardless of the presence or absence of an empty fragment. An empty fragment is currently allowed because older meta-schemas have an empty fragment in their $id (or previously, id). A future draft may outright forbid even empty fragments in \"$id\". The absolute-URI also serves as the base URI for relative URI-references in keywords within the schema resource, in accordance with RFC 3986 section 5.1.1 [RFC3986] regarding base URIs embedded in content. The presence of \"$id\" in a subschema indicates that the subschema constitutes a distinct schema resource within a single schema document. Furthermore, in accordance with RFC 3986 section 5.1.2 [RFC3986] regarding encapsulating entities, if an \"$id\" in a subschema is a relative URI-reference, the base URI for resolving that reference is the URI of the parent schema resource. If no parent schema object explicitly identifies itself as a resource with \"$id\", the base URI is that of the entire document")
   ;; location-independent
   (anchor "$anchor"
           :any
           :initarg :anchor
           :accessor anchor
           :documentation "Using JSON Pointer fragments requires knowledge of the structure of the schema. When writing schema documents with the intention to provide re-usable schemas, it may be preferable to use a plain name fragment that is not tied to any particular structural location. This allows a subschema to be relocated without requiring JSON Pointer references to be updated. The \"$anchor\" and \"$dynamicAnchor\" keywords are used to specify such fragments. They are identifier keywords that can only be used to create plain name fragments, rather than absolute URIs as seen with \"$id\". The base URI to which the resulting fragment is appended is the canonical URI of the schema resource containing the \"$anchor\" or \"$dynamicAnchor\" in question. As discussed in the previous section, this is either the nearest \"$id\" in the same or parent schema object, or the base URI for the document as determined according to RFC 3986.
If present, the value of this keyword MUST be a string and MUST start with a letter ([A-Za-z]) or underscore (\"_\"), followed by any number of letters, digits ([0-9]), hyphens (\"-\"), underscores (\"_\"), and periods (\".\"). This matches the US-ASCII part of XML's NCName production [xml-names]. Note that the anchor string does not include the \"#\" character, as it is not a URI-reference. An \"$anchor\": \"foo\" becomes the fragment \"#foo\" when used in a URI. See below for full examples. The effect of specifying the same fragment name multiple times within the same resource, using any combination of \"$anchor\" and/or \"$dynamicAnchor\", is undefined. Implementations MAY raise an error if such usage is detected.")
   (dynamic-anchor "$dynamicAnchor"
                   :any
                   :initarg :dynamic-anchor
                   :accessor dynamic-anchor
                   :documentation "Separately from the usual usage of URIs, \"$dynamicAnchor\" indicates that the fragment is an extension point when used with the \"$dynamicRef\" keyword. This low-level, advanced feature makes it easier to extend recursive schemas such as the meta-schemas, without imposing any particular semantics on that extension. See the section on \"$dynamicRef\" (Section 8.2.3.2) for details.
In most cases, the normal fragment behavior both suffices and is more intuitive. Therefore it is RECOMMENDED that \"$anchor\" be used to create plain name fragments unless there is a clear need for \"$dynamicAnchor\".")
   ;; Schema References
   (reference "$ref"
              :string
              :initarg :reference
              :accessor referenece
              :documentation "Direct References with \"$ref\"

The \"$ref\" keyword is an applicator that is used to reference a statically identified schema. Its results are the results of the referenced schema. Note that this definition of how the results are determined means that other keywords can appear alongside of \"$ref\" in the same schema object.
The value of the \"$ref\" keyword MUST be a string which is a URI-Reference. Resolved against the current URI base, it produces the URI of the schema to apply. This resolution is safe to perform on schema load, as the process of evaluating an instance cannot change how the reference resolves.")
   (dynamic-reference "$dynamicRef"
                      :string
                      :initarg :dynamic-reference
                      :accessor dynamic-reference
                      :documentation "The $dynamicRef keyword is an applicator that allows for deferring the full resolution until runtime, at which point it is resolved each time it is encountered while evaluating an instance. Together with \"$dynamicAnchor\", \"$dynamicRef\" implements a cooperative extension mechanism that is primarily useful with recursive schemas (schemas that reference themselves). Both the extension point and the runtime-determined extension target are defined with \"$dynamicAnchor\", and only exhibit runtime dynamic behavior when referenced with \"$dynamicRef\". The value of the \"$dynamicRef\" property MUST be a string which is a URI-Reference. Resolved against the current URI base, it produces the URI used as the starting point for runtime resolution. This initial resolution is safe to perform on schema load. If the initially resolved starting point URI includes a fragment that was created by the \"$dynamicAnchor\" keyword, the initial URI MUST be replaced by the URI (including the fragment) for the outermost schema resource in the dynamic scope (Section 7.1) that defines an identically named fragment with \"$dynamicAnchor\".
Otherwise, its behavior is identical to \"$ref\", and no runtime resolution is needed.")
   ;; Schema Re-Use
   (definitions "$defs"
                (:hash-table schema)
                :initarg :definitions
                :accessor definitions
                :documentation "Schema Re-Use With \"$defs\"

The \"$defs\" keyword reserves a location for schema authors to inline re-usable JSON Schemas into a more general schema. The keyword does not directly affect the validation result.
This keyword's value MUST be an object. Each member value of this object MUST be a valid JSON Schema. As an example, here is a schema describing an array of positive integers, where the positive integer constraint is a subschema in \"$defs\":

{
    \"type\": \"array\",
    \"items\": { \"$ref\": \"#/$defs/positiveInteger\" },
    \"$defs\": {
        \"positiveInteger\": {
            \"type\": \"integer\",
            \"exclusiveMinimum\": 0
        }
    }
}")
   (comment "$comment"
            :string
            :initarg :comment
            :accessor comment
            :documentation "Comments With \"$comment\"

This keyword reserves a location for comments from schema authors to readers or maintainers of the schema.
The value of this keyword MUST be a string. Implementations MUST NOT present this string to end users. Tools for editing schemas SHOULD support displaying and editing this keyword. The value of this keyword MAY be used in debug or error output which is intended for developers making use of schemas. Schema vocabularies SHOULD allow \"$comment\" within any object containing vocabulary keywords. Implementations MAY assume \"$comment\" is allowed unless the vocabulary specifically forbids it. Vocabularies MUST NOT specify any effect of \"$comment\" beyond what is described in this specification. Tools that translate other media types or programming languages to and from application/schema+json MAY choose to convert that media type or programming language's native comments to or from \"$comment\" values. The behavior of such translation when both native comments and \"$comment\" properties are present is implementation-dependent. Implementations MAY strip \"$comment\" values at any point during processing. In particular, this allows for shortening schemas when the size of deployed schemas is a concern. Implementations MUST NOT take any other action based on the presence, absence, or contents of \"$comment\" properties. In particular, the value of \"$comment\" MUST NOT be collected as an annotation result.")
   ("discriminator" discriminator
                    :initarg :discriminator
                    :accessor discriminator
                    :documentation "Adds support for polymorphism. The discriminator is an object name that is used to differentiate between other schemas which may satisfy the payload description. See Composition and Inheritance for more details.")
   ("xml" :any
          :initarg :xml
          :accessor xml
          :documentation "This MAY be used only on properties schemas. It has no effect on root schemas. Adds additional metadata to describe the XML representation of this property.

XML Modeling
The xml property allows extra definitions when translating the JSON definition to XML. The XML Object contains additional information about the available options.")
   (external-documentation "externalDocs"
                           external-documentation
                           :initarg :exernal-documentation
                           :accessor external-documentation
                           :documentation "Additional external documentation for this schema.")
   ;; A Vocabulary for Structural Validation
   ;; Validation Keywords for Any Instance Type
   ("type" :any
           :initarg :type
           :accessor schema-type
           :documentation "The value of this keyword MUST be either a string or an array. If it is an array, elements of the array MUST be strings and MUST be unique.
String values MUST be one of the six primitive types (\"null\", \"boolean\", \"object\", \"array\", number, or string\"), or \"integer\" which matches any number with a zero fractional part.
If the value of type is a string, then an instance validates successfully if its type matches the type represented by the value of the string. If the value of type is an array, then an instance validates successfully if its type matches any of the types indicated by the strings in the array.")
   ("enum" :list
           :initarg :enum
           :accessor enum
           :documentation "The value of this keyword MUST be an array. This array SHOULD have at least one element. Elements in the array SHOULD be unique.
An instance validates successfully against this keyword if its value is equal to one of the elements in this keyword's array value.
Elements in the array might be of any type, including null.")
   ("const" :any
            :initarg :const
            :accessor const
            :documentation "The value of this keyword MAY be of any type, including null.
Use of this keyword is functionally equivalent to an \"enum\" (Section 6.1.2) with a single value.
An instance validates successfully against this keyword if its value is equal to the value of the keyword.")
   ;; Validation Keywords for Numeric Instances (number and integer)
   (multiple-of "multipleOf"
                :number
                :initarg :multiple-of
                :accessor multiple-of
                :documentation "The value of \"multipleOf\" MUST be a number, strictly greater than 0.
A numeric instance is valid only if division by this keyword's value results in an integer.")
   ("maximum" :number
              :initarg :maximum
              :accessor maximum
              :documentation "The value of \"maximum\" MUST be a number, representing an inclusive upper limit for a numeric instance.
If the instance is a number, then this keyword validates only if the instance is less than or exactly equal to \"maximum\".")
   (exclusive-maximum "exclusiveMaximum"
                      :any  ;; number in oas 3.1
                      :initarg :exclusive-maximum
                      :accessor exclusive-maximum
                      :documentation "The value of \"exclusiveMaximum\" MUST be a number, representing an exclusive upper limit for a numeric instance. If the instance is a number, then the instance is valid only if it has a value strictly less than (not equal to) \"exclusiveMaximum\".")
   ("minimum" :number
              :initarg :minimum
              :accessor minimum
              :documentation "The value of \"minimum\" MUST be a number, representing an inclusive lower limit for a numeric instance. If the instance is a number, then this keyword validates only if the instance is greater than or exactly equal to \"minimum\".")
   (exclusive-minimum "exclusiveMinimum"
                      :any ;; number in oas 3.1
                      :initarg :exclusive-minimum
                      :accessor exclusive-minimum
                      :documentation "The value of \"exclusiveMinimum\" MUST be a number, representing an exclusive lower limit for a numeric instance. If the instance is a number, then the instance is valid only if it has a value strictly greater than (not equal to) \"exclusiveMinimum\".")
   ;; Validation Keywords for Strings
   (maximum-length "maxLength"
                   :number
                   :initarg :maximum-length
                   :accessor maximum-length
                   :documentation "The value of this keyword MUST be a non-negative integer. A string instance is valid against this keyword if its length is less than, or equal to, the value of this keyword. The length of a string instance is defined as the number of its characters as defined by RFC 8259 [RFC8259].
")
   (minimum-length "minLength"
                   :number
                   :initarg :minimum-length
                   :accessor minimum-length
                   :documentation "The value of this keyword MUST be a non-negative integer. A string instance is valid against this keyword if its length is greater than, or equal to, the value of this keyword. The length of a string instance is defined as the number of its characters as defined by RFC 8259 [RFC8259]. Omitting this keyword has the same behavior as a value of 0.")
   ("pattern" :string
              :initarg :pattern
              :accessor pattern
              :documentation "The value of this keyword MUST be a string.
This string SHOULD be a valid regular expression, according to the ECMA-262 regular expression dialect. A string instance is considered valid if the regular expression matches the instance successfully. Recall: regular expressions are not implicitly anchored.")
   ;; Validation Keywords for Arrays
   (maximum-items "maxItems"
                  :number
                  :initarg :maximum-items
                  :accessor maximum-items
                  :documentation "The value of this keyword MUST be a non-negative integer. An array instance is valid against \"maxItems\" if its size is less than, or equal to, the value of this keyword.")
   (minimum-items "minItems"
                  :number
                  :initarg :minimum-items
                  :accessor minimum-items
                  :documentation "the value of this keyword must be a non-negative integer. an array instance is valid against \"minitems\" if its size is greater than, or equal to, the value of this keyword. omitting this keyword has the same behavior as a value of 0.")
   (unique-items "uniqueitems"
                 :bool
                 :initarg :unique-items
                 :accessor unique-items
                 :documentation "The value of this keyword MUST be a boolean. If this keyword has boolean value false, the instance validates successfully. If it has boolean value true, the instance validates successfully if all of its elements are unique. Omitting this keyword has the same behavior as a value of false.
")
   (maximum-contains "maxContains"
                     :number
                     :initarg :maximum-contains
                     :accessor maximum-contains
                     :documentation "The value of this keyword MUST be a non-negative integer.
If \"contains\" is not present within the same schema object, then this keyword has no effect. An instance array is valid against \"maxContains\" in two ways, depending on the form of the annotation result of an adjacent \"contains\" [json-schema] keyword. The first way is if the annotation result is an array and the length of that array is less than or equal to the \"maxContains\" value. The second way is if the annotation result is a boolean \"true\" and the instance array length is less than or equal to the \"maxContains\" value.")
   (minimum-contains "minContains"
                     :number
                     :initarg :minimum-contains
                     :accessor minimum-contains
                     :documentation "The value of this keyword MUST be a non-negative integer.
If \"contains\" is not present within the same schema object, then this keyword has no effect. An instance array is valid against \"minContains\" in two ways, depending on the form of the annotation result of an adjacent \"contains\" [json-schema] keyword. The first way is if the annotation result is an array and the length of that array is greater than or equal to the \"minContains\" value. The second way is if the annotation result is a boolean \"true\" and the instance array length is greater than or equal to the \"minContains\" value. A value of 0 is allowed, but is only useful for setting a range of occurrences from 0 to the value of \"maxContains\". A value of 0 causes \"minContains\" and \"contains\" to always pass validation (but validation can still fail against a \"maxContains\" keyword). Omitting this keyword has the same behavior as a value of 1.")
   ;; Validation Keywords for Objects (hash-table)
   (maximum-properties "maxProperties" :number
                       :accessor maximum-properties
                       :documentation "The value of this keyword MUST be a non-negative integer.
An object instance is valid against \"maxProperties\" if its number of properties is less than, or equal to, the value of this keyword.")
   (minimum-properties "minProperties"
                       :number
                       :initarg :minimum-properties
                       :accessor minimum-properties
                       :documentation "The value of this keyword MUST be a non-negative integer.
An object instance is valid against \"minProperties\" if its number of properties is greater than, or equal to, the value of this keyword. Omitting this keyword has the same behavior as a value of 0.")
   ("required" (:list :string)
               :initarg :required
               :accessor required
               :documentation "The value of this keyword MUST be an array. Elements of this array, if any, MUST be strings, and MUST be unique.
An object instance is valid against this keyword if every item in the array is the name of a property in the instance. Omitting this keyword has the same behavior as an empty array.")
   (dependend-required "dependendRequired"
                       (:hash-table :list)
                       :initarg :dependendend-required
                       :accessor dependend-required
                       :documentation "The value of this keyword MUST be an object. Properties in this object, if any, MUST be arrays. Elements in each array, if any, MUST be strings, and MUST be unique.
This keyword specifies properties that are required if a specific other property is present. Their requirement is dependent on the presence of the other property. Validation succeeds if, for each name that appears in both the instance and as a name within this keyword's value, every item in the corresponding array is also the name of a property in the instance. Omitting this keyword has the same behavior as an empty object.")
   ;; Contents of String-Encoded Data
   (content-encoding "contentEncoding"
                     :string
                     :initarg :content-encoding
                     :accessor content-encoding
                     :documentation "If the instance value is a string, this property defines that the string SHOULD be interpreted as encoded binary data and decoded using the encoding named by this property.
Possible values indicating base 16, 32, and 64 encodings with several variations are listed in RFC 4648 [RFC4648]. Additionally, sections 6.7 and 6.8 of RFC 2045 [RFC2045] provide encodings used in MIME. This keyword is derived from MIME's Content-Transfer-Encoding header, which was designed to map binary data into ASCII characters. It is not related to HTTP's Content-Encoding header, which is used to encode (e.g. compress or encrypt) the content of HTTP request and responses. As \"base64\" is defined in both RFCs, the definition from RFC 4648 SHOULD be assumed unless the string is specifically intended for use in a MIME context. Note that all of these encodings result in strings consisting only of 7-bit ASCII characters. Therefore, this keyword has no meaning for strings containing characters outside of that range. If this keyword is absent, but \"contentMediaType\" is present, this indicates that the encoding is the identity encoding, meaning that no transformation was needed in order to represent the content in a UTF-8 string. The value of this property MUST be a string.")
   (content-media-type "contentMediaType"
                       :string
                       :initarg :content-media-type
                       :accessor content-media-type
                       :documentation "If the instance is a string, this property indicates the media type of the contents of the string. If \"contentEncoding\" is present, this property describes the decoded string. The value of this property MUST be a string, which MUST be a media type, as defined by RFC 2046 [RFC2046].")
   (content-schema "contentSchema"
                   :string
                   :initarg :content-schema
                   :accessor content-schema
                   :documentation "If the instance is a string, and if \"contentMediaType\" is present, this property contains a schema which describes the structure of the string. This keyword MAY be used with any media type that can be mapped into JSON Schema's data model. The value of this property MUST be a valid JSON schema. It SHOULD be ignored if \"contentMediaType\" is not present.")

   ;; Basic Meta-Data Annotations
   ("title" :string
            :initarg :title
            :accessor title
            :documentation "Can be used to decorate a user interface with information about the data produced by this user interface. A title will preferably be short.")
   ("description" :string
                  :initarg :description
                  :accessor description
                  :documentation "Can be used to decorate a user interface with information about the data produced by this user interface. A description will provide explanation about the purpose of the instance described by this schema.
")
   ("default" :any
              :initarg :default
              :accessor default
              :documentation "There are no restrictions placed on the value of this keyword. When multiple occurrences of this keyword are applicable to a single sub-instance, implementations SHOULD remove duplicates.
This keyword can be used to supply a default JSON value associated with a particular schema. It is RECOMMENDED that a default value be valid against the associated schema.")
   ("deprecated" :bool
                 :initarg :deprecated
                 :accessor deprecated
                 :documentation "The value of this keyword MUST be a boolean.
When multiple occurrences of this keyword are applicable to a single sub-instance, applications SHOULD consider the instance location to be deprecated if any occurrence specifies a true value. If \"deprecated\" has a value of boolean true, it indicates that applications SHOULD refrain from usage of the declared property. It MAY mean the property is going to be removed in the future. A root schema containing \"deprecated\" with a value of true indicates that the entire resource being described MAY be removed in the future. The \"deprecated\" keyword applies to each instance location to which the schema object containing the keyword successfully applies. This can result in scenarios where every array item or object property is deprecated even though the containing array or object is not. Omitting this keyword has the same behavior as a value of false.")
   ;; "readOnly" and "writeOnly"
   (read-only "readOnly"
              :bool
              :initarg :read-only
              :accessor read-only
              :documentation "The value of these keywords MUST be a boolean. When multiple occurrences of these keywords are applicable to a single sub-instance, the resulting behavior SHOULD be as for a true value if any occurrence specifies a true value, and SHOULD be as for a false value otherwise.
If \"readOnly\" has a value of boolean true, it indicates that the value of the instance is managed exclusively by the owning authority, and attempts by an application to modify the value of this property are expected to be ignored or rejected by that owning authority.
An instance document that is marked as \"readOnly\" for the entire document MAY be ignored if sent to the owning authority, or MAY result in an error, at the authority's discretion.
For example, \"readOnly\" would be used to mark a database-generated serial number as read-only, while \"writeOnly\" would be used to mark a password input field. These keywords can be used to assist in user interface instance generation. In particular, an application MAY choose to use a widget that hides input values as they are typed for write-only fields.
Omitting these keywords has the same behavior as values of false")
   (write-only "writeOnly"
               :bool
               :initarg :write-only
               :accessor write-only
               :documentation "The value of these keywords MUST be a boolean.
When multiple occurrences of these keywords are applicable to a single sub-instance, the resulting behavior SHOULD be as for a true value if any occurrence specifies a true value, and SHOULD be as for a false value otherwise.
If \"writeOnly\" has a value of boolean true, it indicates that the value is never present when the instance is retrieved from the owning authority. It can be present when sent to the owning authority to update or create the document (or the resource it represents), but it will not be included in any updated or newly created version of the instance. An instance document that is marked as \"writeOnly\" for the entire document MAY be returned as a blank document of some sort, or MAY produce an error upon retrieval, or have the retrieval request ignored, at the authority's discretion. For example, \"readOnly\" would be used to mark a database-generated serial number as read-only, while \"writeOnly\" would be used to mark a password input field. These keywords can be used to assist in user interface instance generation. In particular, an application MAY choose to use a widget that hides input values as they are typed for write-only fields.
Omitting these keywords has the same behavior as values of false.")
   ("examples" :list
               :initarg :examples
               :accessor examples
               :documentation "The value of this keyword MUST be an array. There are no restrictions placed on the values within the array. When multiple occurrences of this keyword are applicable to a single sub-instance, implementations MUST provide a flat array of all values rather than an array of arrays. This keyword can be used to provide sample JSON values associated with a particular schema, for the purpose of illustrating usage. It is RECOMMENDED that these values be valid against the associated schema. Implementations MAY use the value(s) of \"default\", if present, as an additional example. If \"examples\" is absent, \"default\" MAY still be used in this manner.
")

   ;; Applying Subschemas in Place
             ;;; These keywords apply subschemas to the same location in the instance as the parent schema is being applied. They allow combining or modifying the subschema results in various ways.
             ;;; Subschemas of these keywords evaluate the instance completely independently such that the results of one such subschema MUST NOT impact the results of sibling subschemas. Therefore subschemas may be applied in any order.
   ;; Applying Subschemas With Logic
             ;;; These keywords correspond to logical operators for combining or modifying the boolean assertion results of the subschemas. They have no direct impact on annotation collection, although they enable the same annotation keyword to be applied to an instance location with different values. Annotation keywords define their own rules for combining such values.
   (all-of "allOf"
           (:list schema)
           :initarg :all-of
           :accessor all-of
           :documentation "This keyword's value MUST be a non-empty array. Each item of the array MUST be a valid JSON Schema.
An instance validates successfully against this keyword if it validates successfully against all schemas defined by this keyword's value.")
   (any-of "anyOf"
           (:list schema)
           :initarg :any-of
           :accessor any-of
           :documentation "This keyword's value MUST be a non-empty array. Each item of the array MUST be a valid JSON Schema.
An instance validates successfully against this keyword if it validates successfully against at least one schema defined by this keyword's value. Note that when annotations are being collected, all subschemas MUST be examined so that annotations are collected from each subschema that validates successfully.")
   (one-of "oneOf"
           (:list schema)
           :initarg :one-of
           :accessor one-of
           :documentation "This keyword's value MUST be a non-empty array. Each item of the array MUST be a valid JSON Schema.
An instance validates successfully against this keyword if it validates successfully against exactly one schema defined by this keyword's value.")

   ("not" schema
          :initarg :not
          :accessor schema-not
          :documentation "This keyword's value MUST be a valid JSON Schema.
An instance is valid against this keyword if it fails to validate successfully against the schema defined by this keyword.")
   ;; Keywords for Applying Subschemas Conditionally
             ;;; Three of these keywords work together to implement conditional application of a subschema based on the outcome of another subschema. The fourth is a shortcut for a specific conditional case.
             ;;; "if", "then", and "else" MUST NOT interact with each other across subschema boundaries. In other words, an "if" in one branch of an "allOf" MUST NOT have an impact on a "then" or "else" in another branch.
             ;;; There is no default behavior for "if", "then", or "else" when they are not present. In particular, they MUST NOT be treated as if present with an empty schema, and when "if" is not present, both "then" and "else" MUST be entirely ignored.
   ("if" schema
         :initarg :if
         :accessor schema-if
         :documentation "This keyword's value MUST be a valid JSON Schema.
This validation outcome of this keyword's subschema has no direct effect on the overall validation result. Rather, it controls which of the \"then\" or \"else\" keywords are evaluated.
Instances that successfully validate against this keyword's subschema MUST also be valid against the subschema value of the \"then\" keyword, if present.
Instances that fail to validate against this keyword's subschema MUST also be valid against the subschema value of the \"else\" keyword, if present.
If annotations (Section 7.7) are being collected, they are collected from this keyword's subschema in the usual way, including when the keyword is present without either \"then\" or \"else\".")
   ("then" schema
           :initarg :then
           :accessor then
           :documentation "This keyword's value MUST be a valid JSON Schema.
When \"if\" is present, and the instance successfully validates against its subschema, then validation succeeds against this keyword if the instance also successfully validates against this keyword's subschema.
This keyword has no effect when \"if\" is absent, or when the instance fails to validate against its subschema. Implementations MUST NOT evaluate the instance against this keyword, for either validation or annotation collection purposes, in such cases.")
   ("else" schema
           :initarg :else
           :accessor else
           :documentation "This keyword's value MUST be a valid JSON Schema.
When \"if\" is present, and the instance fails to validate against its subschema, then validation succeeds against this keyword if the instance successfully validates against this keyword's subschema.
This keyword has no effect when \"if\" is absent, or when the instance successfully validates against its subschema. Implementations MUST NOT evaluate the instance against this keyword, for either validation or annotation collection purposes, in such cases.")
   (dependent-schemas "dependentSchemas"
                      (:hash-table schema)
                      :initarg :dependent-schema
                      :accessor dependent-schemas
                      :documentation "This keyword specifies subschemas that are evaluated if the instance is an object and contains a certain property.
This keyword's value MUST be an object. Each value in the object MUST be a valid JSON Schema.
If the object key is a property in the instance, the entire instance must validate against the subschema. Its use is dependent on the presence of the property.
Omitting this keyword has the same behavior as an empty object.
")

   ;; Keywords for Applying Subschemas to Child Instances
             ;;;; Each of these keywords defines a rule for applying its subschema(s) to child instances, specifically object properties and array items, and combining their results.
             ;;; Keywords for Applying Subschemas to Arrays
   ;; Subschemas to Arrays
   (prefix-items "prefixItems"
                 (:list schema)
                 :initarg :prefix-items
                 :accessor prefix-items
                 :documentation "The value of \"prefixItems\" MUST be a non-empty array of valid JSON Schemas.
Validation succeeds if each element of the instance validates against the schema at the same position, if any. This keyword does not constrain the length of the array. If the array is longer than this keyword's value, this keyword validates only the prefix of matching length.
This keyword produces an annotation value which is the largest index to which this keyword applied a subschema. The value MAY be a boolean true if a subschema was applied to every index of the instance, such as is produced by the \"items\" keyword. This annotation affects the behavior of \"items\" and \"unevaluatedItems\".
Omitting this keyword has the same assertion behavior as an empty array.")
   ("items" schema
            :initarg :items
            :accessor items
            :documentation "The value of \"items\" MUST be a valid JSON Schema.
This keyword applies its subschema to all instance elements at indexes greater than the length of the \"prefixItems\" array in the same schema object, as reported by the annotation result of that \"prefixItems\" keyword. If no such annotation result exists, \"items\" applies its subschema to all instance array elements. Note that the behavior of \"items\" without \"prefixItems\" is identical to that of the schema form of \"items\" in prior drafts. When \"prefixItems\" is present, the behavior of \"items\" is identical to the former \"additionalItems\" keyword.
If the \"items\" subschema is applied to any positions within the instance array, it produces an annotation result of boolean true, indicating that all remaining array elements have been evaluated against this keyword's subschema. This annotation affects the behavior of \"unevaluatedItems\" in the Unevaluated vocabulary.
Omitting this keyword has the same assertion behavior as an empty schema.
Implementations MAY choose to implement or optimize this keyword in another way that produces the same effect, such as by directly checking for the presence and size of a \"prefixItems\" array. Implementations that do not support annotation collection MUST do so.")
   ("contains" schema
               :initarg :contains
               :accessor contains
               :documentation "The value of this keyword MUST be a valid JSON Schema.
An array instance is valid against \"contains\" if at least one of its elements is valid against the given schema, except when \"minContains\" is present and has a value of 0, in which case an array instance MUST be considered valid against the \"contains\" keyword, even if none of its elements is valid against the given schema.
This keyword produces an annotation value which is an array of the indexes to which this keyword validates successfully when applying its subschema, in ascending order. The value MAY be a boolean \"true\" if the subschema validates successfully when applied to every index of the instance. The annotation MUST be present if the instance array to which this keyword's schema applies is empty.
This annotation affects the behavior of \"unevaluatedItems\" in the Unevaluated vocabulary, and MAY also be used to implement the \"minContains\" and \"maxContains\" keywords in the Validation vocabulary.
The subschema MUST be applied to every array element even after the first match has been found, in order to collect annotations for use by other keywords. This is to ensure that all possible annotations are collected.")

             ;;; Keywords for Applying Subschemas to Objects
   ("properties" (:hash-table schema)
                 :initarg :properties
                 :accessor properties
                 :documentation "The value of \"properties\" MUST be an object.
Each value of this object MUST be a valid JSON Schema.
Validation succeeds if, for each name that appears in both the instance and as a name within this keyword's value, the child instance for that name successfully validates against the corresponding schema.
The annotation result of this keyword is the set of instance property names matched by this keyword. This annotation affects the behavior of \"additionalProperties\" (in this vocabulary) and \"unevaluatedProperties\" in the Unevaluated vocabulary.
Omitting this keyword has the same assertion behavior as an empty object.")
   (pattern-properties "patternProperties"
                       (:hash-table schema)
                       :initarg :pattern-properties
                       :accessor pattern-properties
                       :documentation "The value of \"patternProperties\" MUST be an object.
Each property name of this object SHOULD be a valid regular expression, according to the ECMA-262 regular expression dialect. Each property value of this object MUST be a valid JSON Schema.
Validation succeeds if, for each instance name that matches any regular expressions that appear as a property name in this keyword's value, the child instance for that name successfully validates against each schema that corresponds to a matching regular expression.
The annotation result of this keyword is the set of instance property names matched by this keyword. This annotation affects the behavior of \"additionalProperties\" (in this vocabulary) and \"unevaluatedProperties\" (in the Unevaluated vocabulary).
Omitting this keyword has the same assertion behavior as an empty object.")
   (additional-properties "additionalProperties"
                          :any
                          :initarg :additional-properties
                          :accessor additional-properties
                          :documentation "The value of \"additionalProperties\" MUST be a valid JSON Schema.
The behavior of this keyword depends on the presence and annotation results of \"properties\" and \"patternProperties\" within the same schema object. Validation with \"additionalProperties\" applies only to the child values of instance names that do not appear in the annotation results of either \"properties\" or \"patternProperties\".
For all such properties, validation succeeds if the child instance validates against the \"additionalProperties\" schema.
The annotation result of this keyword is the set of instance property names validated by this keyword's subschema. This annotation affects the behavior of \"unevaluatedProperties\" in the Unevaluated vocabulary.
Omitting this keyword has the same assertion behavior as an empty schema.
Implementations MAY choose to implement or optimize this keyword in another way that produces the same effect, such as by directly checking the names in \"properties\" and the patterns in \"patternProperties\" against the instance property set. Implementations that do not support annotation collection MUST do so. In defining this option, it seems there is the potential for ambiguity in the output format. The ambiguity does not affect validation results, but it does affect the resulting output format. The ambiguity allows for multiple valid output results depending on whether annotations are used or a solution that \"produces the same effect\" as draft-07. It is understood that annotations from failing schemas are dropped.")
   (property-names "propertyNames"
                   :any
                   :initarg :property-names
                   :accessor property-names
                   :documentation "The value of \"propertyNames\" MUST be a valid JSON Schema.
If the instance is an object, this keyword validates if every property name in the instance validates against the provided schema. Note the property name that the schema is testing will always be a string.
Omitting this keyword has the same behavior as an empty schema.")
                                        ; A Vocabulary for Unevaluated Locations
   ;; Keyword Independence
   (unevaluated-items "unevaluatedItems"
                      :any
                      :initarg :unevaluated-items
                      :accessor unevaluated-items
                      :documentation "The value of \"unevaluatedItems\" MUST be a valid JSON Schema.
The behavior of this keyword depends on the annotation results of adjacent keywords that apply to the instance location being validated. Specifically, the annotations from \"prefixItems\", \"items\", and \"contains\", which can come from those keywords when they are adjacent to the \"unevaluatedItems\" keyword. Those three annotations, as well as \"unevaluatedItems\", can also result from any and all adjacent in-place applicator (Section 10.2) keywords. This includes but is not limited to the in-place applicators defined in this document.
If no relevant annotations are present, the \"unevaluatedItems\" subschema MUST be applied to all locations in the array. If a boolean true value is present from any of the relevant annotations, \"unevaluatedItems\" MUST be ignored. Otherwise, the subschema MUST be applied to any index greater than the largest annotation value for \"prefixItems\", which does not appear in any annotation value for \"contains\".
This means that \"prefixItems\", \"items\", \"contains\", and all in-place applicators MUST be evaluated before this keyword can be evaluated. Authors of extension keywords MUST NOT define an in-place applicator that would need to be evaluated after this keyword.
If the \"unevaluatedItems\" subschema is applied to any positions within the instance array, it produces an annotation result of boolean true, analogous to the behavior of \"items\". This annotation affects the behavior of \"unevaluatedItems\" in parent schemas.
Omitting this keyword has the same assertion behavior as an empty schema.")
   (unevaluated-properties "unevaluatedProperties"
                           :any
                           :initarg :unevaluated-properties
                           :accessor unevaluated-properties
                           :documentation "The value of \"unevaluatedProperties\" MUST be a valid JSON Schema.
The behavior of this keyword depends on the annotation results of adjacent keywords that apply to the instance location being validated. Specifically, the annotations from \"properties\", \"patternProperties\", and \"additionalProperties\", which can come from those keywords when they are adjacent to the \"unevaluatedProperties\" keyword. Those three annotations, as well as \"unevaluatedProperties\", can also result from any and all adjacent in-place applicator (Section 10.2) keywords. This includes but is not limited to the in-place applicators defined in this document.
Validation with \"unevaluatedProperties\" applies only to the child values of instance names that do not appear in the \"properties\", \"patternProperties\", \"additionalProperties\", or \"unevaluatedProperties\" annotation results that apply to the instance location being validated.
For all such properties, validation succeeds if the child instance validates against the \"unevaluatedProperties\" schema.
This means that \"properties\", \"patternProperties\", \"additionalProperties\", and all in-place applicators MUST be evaluated before this keyword can be evaluated. Authors of extension keywords MUST NOT define an in-place applicator that would need to be evaluated after this keyword.
The annotation result of this keyword is the set of instance property names validated by this keyword's subschema. This annotation affects the behavior of \"unevaluatedProperties\" in parent schemas.
Omitting this keyword has the same assertion behavior as an empty schema.")
   (keyword-location "keywordLocation"
                     :string
                     :initarg :keyword-location
                     :accessor keyword-location
                     :documentation "The relative location of the validating keyword that follows the validation path. The value MUST be expressed as a JSON Pointer, and it MUST include any by-reference applicators such as \"$ref\" or \"$dynamicRef\".

/properties/width/$ref/minimum

Note that this pointer may not be resolvable by the normal JSON Pointer process due to the inclusion of these by-reference applicator keywords.")
   (absolute-keyword-location "absoluteKeywordLocation"
                              :string
                              :initarg :absolute-keyword-location
                              :accessor absolute-keyword-location
                              :documentation "The absolute, dereferenced location of the validating keyword. The value MUST be expressed as a full URI using the canonical URI of the relevant schema resource with a JSON Pointer fragment, and it MUST NOT include by-reference applicators such as \"$ref\" or \"$dynamicRef\" as non-terminal path components. It MAY end in such keywords if the error or annotation is for that keyword, such as an unresolvable reference. Note that \"absolute\" here is in the sense of \"absolute filesystem path\" (meaning the complete location) rather than the \"absolute-URI\" terminology from RFC 3986 (meaning with scheme but without fragment). Keyword absolute locations will have a fragment in order to identify the keyword.

https://example.com/schemas/common#/$defs/count/minimum

This information MAY be omitted only if either the dynamic scope did not pass over a reference or if the schema does not declare an absolute URI as its \"$id\".")
   (instance-location "instanceLocation"
                      :string
                      :initarg :instance-location
                      :accessor instance-location
                      :documentation "The location of the JSON value within the instance being validated. The value MUST be expressed as a JSON Pointer.")
   ("valid" :bool
            :initarg :valid
            :accessor valid
            :documentation "A boolean value indicating the overall validation success or failure")
   ("errors" :any
             :initarg :errors
             :accessor errors
             :documentation "The collection of errors or annotations produced by a failed validation")
   ("annotations" :any
                  :initarg :annotations
                  :accessor annotations
                  :documentation "The collection of errors or annotations produced by a successful validation")
   ("nullable" :bool
               :initarg :nullable
               :accessor nullable
               :documentation "Marks a slot that can be null."))
  (:documentation "The Schema Object allows the definition of input and output data types. These types can be objects, but also primitives and arrays. This object is a superset of the JSON Schema Specification Draft 2020-12.

For more information about the properties, see JSON Schema Core and JSON Schema Validation.

Unless stated otherwise, the property definitions follow those of JSON Schema and do not add any additional semantics. Where JSON Schema indicates that behavior is defined by the application (e.g. for annotations), OAS also defers the definition of semantics to the application consuming the OpenAPI document.

Properties

The OpenAPI Schema Object dialect is defined as requiring the OAS base vocabulary, in addition to the vocabularies as specified in the JSON Schema draft 2020-12 general purpose meta-schema.

The OpenAPI Schema Object dialect for this version of the specification is identified by the URI https://spec.openapis.org/oas/3.1/dialect/base (the “OAS dialect schema id”).

The following properties are taken from the JSON Schema specification but their definitions have been extended by the OAS:

    - description - CommonMark syntax MAY be used for rich text representation.
    - format - See Data Type Formats for further details. While relying on JSON Schema’s defined formats, the OAS offers a few additional predefined formats.

In addition to the JSON Schema properties comprising the OAS dialect, the Schema Object supports keywords from any other vocabularies, or entirely arbitrary properties.

Composition and Inheritance (Polymorphism)

The OpenAPI Specification allows combining and extending model definitions using the allOf property of JSON Schema, in effect offering model composition. allOf takes an array of object definitions that are validated independently but together compose a single object.

While composition offers model extensibility, it does not imply a hierarchy between the models. To support polymorphism, the OpenAPI Specification adds the discriminator field. When used, the discriminator will be the name of the property that decides which schema definition validates the structure of the model. As such, the discriminator field MUST be a required field. There are two ways to define the value of a discriminator for an inheriting instance.

    - Use the schema name.
    - Override the schema name by overriding the property with a new value. If a new value exists, this takes precedence over the schema name. As such, inline schema definitions, which do not have a given id, cannot be used in polymorphism."))

;; Discriminator Object (only allowed if oneOf, anyOf, allOf)
(define-json-class discriminator nil
  ((property-name "propertyName" :string
                  :required t
                  :accessor property-name
                  :documentation "The name of the property in the payload that will hold the discriminator value.")
   ("mapping" (:hash-table :string)
              :accessor mapping
              :documentation "An object to hold mappings between payload values and schema names or references."))
  (:documentation "When request bodies or response payloads may be one of a number of different schemas, a discriminator object can be used to aid in serialization, deserialization, and validation. The discriminator is a specific object in a schema which is used to inform the consumer of the document of an alternative schema based on the value associated with it.

When using the discriminator, inline schemas will not be considered.

This object MAY be extended with Specification Extensions.

The discriminator object is legal only when using one of the composite keywords oneOf, anyOf, allOf."))

(defmethod print-object ((object discriminator) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cl:format stream "~A" (property-name object))))

(define-json-class xml nil
  (("name" :string
           :accessor name
           :documentation "Replaces the name of the element/attribute used for the described schema property. When defined within items, it will affect the name of the individual XML elements within the list. When defined alongside type being array (outside the items), it will affect the wrapping element and only if wrapped is true. If wrapped is false, it will be ignored.")
   ("namespace" :string
                :accessor namespace
                :documentation "The URI of the namespace definition. This MUST be in the form of an absolute URI.")
   ("prefix" :string
             :accessor prefix
             :documentation "The prefix to be used for the name.")
   ("attribute" :any
                :accessor attribute
                :documentation "Declares whether the property definition translates to an attribute instead of an element. Default value is false.")
   ("wrapped" :bool
              :accessor wrapped
              :documentation "MAY be used only for an array definition. Signifies whether the array is wrapped (for example, <books><book/><book/></books>) or unwrapped (<book/><book/>). Default value is false. The definition takes effect only when defined alongside type being array (outside the items)."))
  (:documentation "A metadata object that allows for more fine-tuned XML model definitions.

When using arrays, XML element names are not inferred (for singular/plural forms) and the name property SHOULD be used to add that information. See examples for expected behavior."))

(define-json-class security-scheme nil
  (("type" :string
           :initarg :type
           :accessor security-scheme-type
           :required t
           :documentation "The type of the security scheme. Valid values are \"apiKey\", \"http\", \"mutualTLS\", \"oauth2\", \"openIdConnect\".")
   ("description" :string
                  :initarg :description
                  :accessor description
                  :documentation "A description for security scheme. CommonMark syntax MAY be used for rich text representation.")
   ("name" :string
           :initarg :name
           :accessor name
           :documentation "The name of the header, query or cookie parameter to be used.")
   ("in" :string
         :initarg :in
         :accessor in
         :documentation "The location of the API key. Valid values are \"query\", \"header\" or \"cookie\".")
   ("scheme" :string
             :initarg :scheme
             :accessor scheme
             :documentation "The name of the HTTP Authorization scheme to be used in the Authorization header as defined in [RFC7235]. The values used SHOULD be registered in the IANA Authentication Scheme registry.")
   (bearer-format "bearerFormat"
                  :string
                  :initarg :bearer-format
                  :accessor bearer-format
                  :documentation "A hint to the client to identify how the bearer token is formatted. Bearer tokens are usually generated by an authorization server, so this information is primarily for documentation purposes.")
   ("flows" o-auth-flow
            :initarg :flows
            :accessor flows
            :documentation "An object containing configuration information for the flow types supported.")
   (open-id-connect-url "openIdConnectUrl"
                        :string
                        :initarg :open-id-connect-url
                        :accessor open-id-connect-url
                        :documentation "OpenId Connect URL to discover OAuth2 configuration values. This MUST be in the form of a URL. The OpenID Connect standard requires the use of TLS.")
   (reference "$ref"
              :string
              :initarg :reference
              :accessor reference))
  (:documentation "Defines a security scheme that can be used by the operations.

Supported schemes are HTTP authentication, an API key (either as a header, a cookie parameter or as a query parameter), mutual TLS (use of a client certificate), OAuth2’s common flows (implicit, password, client credentials and authorization code) as defined in [RFC6749], and OpenID Connect Discovery. Please note that as of 2020, the implicit flow is about to be deprecated by OAuth 2.0 Security Best Current Practice. Recommended for most use case is Authorization Code Grant flow with PKCE."))

(defmethod print-object ((object security-scheme) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cl:format stream "type: ~A, name: ~A, in: ~A, scheme: ~A"
               (slot-value object 'type) (name object) (in object) (scheme object))))

(define-json-class o-auth-flows nil
  (("implicit" o-auth-flow
               :accessor implicit
               :documentation "Configuration for the OAuth Implicit flow")
   ("password" o-auth-flow
               :accessor password
               :documentation "Configuration for the OAuth Resource Owner Password flow")
   (client-credentials "clientCredentials" o-auth-flow
                       :accessor client-credentials
                       :documentation "Configuration for the OAuth Client Credentials flow. Previously called application in OpenAPI 2.0.")
   (authorization-code "authorizationCode" o-auth-flow
                       :accessor authorization-code
                       :documentation "Configuration for the OAuth Authorization Code flow. Previously called accessCode in OpenAPI 2.0."))
  (:documentation "Allows configuration of the supported OAuth Flows."))

(define-json-class o-auth-flow nil
  ((authorization-url "authorizationUrl" :string
                      :required t
                      :accessor authorization-url
                      :documentation "The authorization URL to be used for this flow. This MUST be in the form of a URL. The OAuth2 standard requires the use of TLS.")
   (token-url "tokenUrl" :string
              :required t
              :accessor token-url
              :documentation "The token URL to be used for this flow. This MUST be in the form of a URL. The OAuth2 standard requires the use of TLS.")
   (refresh-url "refreshUrl" :string
                :accessor refresh-url
                :documentation "The URL to be used for obtaining refresh tokens. This MUST be in the form of a URL. The OAuth2 standard requires the use of TLS.")
   ("scopes" (:hash-table :string)
             :accessor scopes
             :required t
             :documentation "The available scopes for the OAuth2 security scheme. A map between the scope name and a short description for it. The map MAY be empty."))
  (:documentation "Configuration details for a supported OAuth Flow"))

(defmethod print-object ((object o-auth-flow) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cl:format stream "authorizationUrl: ~A, tokenUrl: ~A"
               (authorization-url object) (token-url object))))

(define-json-class path-item-or-reference nil
  (("title" :string
            :accessor title)))
