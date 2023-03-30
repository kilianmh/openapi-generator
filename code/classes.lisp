(cl:in-package #:openapi-generator)

(json-class openapi nil
            (("openapi" :string
                        :documentation "REQUIRED. This string MUST be the version number of the OpenAPI Specification that the OpenAPI document uses. The openapi field SHOULD be used by tooling to interpret the OpenAPI document. This is not related to the API info.version string.")
             ("info" info
                     :documentation "REQUIRED. Provides metadata about the API. The metadata MAY be used by tooling as required.")
             ("servers" :any
                        :documentation "An array of Server Objects, which provide connectivity information to a target server. If the servers property is not provided, or is an empty array, the default value would be a Server Object with a url value of /.")
             ("security" :any :documentation "A declaration of which security mechanisms can be used across the API. The list of values includes alternative security requirement objects that can be used. Only one of the security requirement objects need to be satisfied to authorize a request. Individual operations can override this definition. To make security optional, an empty security requirement ({}) can be included in the array.")
             ("paths" (:hash-table path)
                      :documentation "The available paths and operations for the API.
Holds the relative paths to the individual endpoints and their operations. The path is appended to the URL from the Server Object in order to construct the full URL. The Paths MAY be empty, due to Access Control List (ACL) constraints.")
             ("tags" (:list tag)
                     :documentation "A list of tags used by the document with additional metadata. The order of the tags can be used to reflect on their order by the parsing tools. Not all tags that are used by the Operation Object must be declared. The tags that are not declared MAY be organized randomly or based on the tools’ logic. Each tag name in the list MUST be unique.")
             ("externalDocs" external-documentation
                             :documentation "Additional external documentation.")
             ("components" components
                           :documentation "An element to hold various schemas for the document.")
             ("schema" schema))
            (:documentation "This is the root object of the OpenAPI document."))

(json-class openapi-3.0 (openapi) nil)

(json-class openapi-3.1 (openapi)
            (("jsonSchemaDialect" :string
                                  :documentation "The default value for the $schema keyword within Schema Objects contained within this OAS document. This MUST be in the form of a URI.")
             ("webhooks" (:hash-table path)
                         :documentation "The incoming webhooks that MAY be received as part of this API and that the API consumer MAY choose to implement. Closely related to the callbacks feature, this section describes requests initiated other than by an API call, for example by an out of band registration. The key name is a unique string to refer to each webhook, while the (optionally referenced) Path Item Object describes a request that may be initiated by the API provider and the expected responses. An example is available.")))

(defmethod print-object ((api openapi) stream)
  (print-unreadable-object (api stream :type t :identity t)
    (cl:format stream "~A, version: ~A"
               (title (info api))
               (version (info api)))))

(json-class info nil
            (("title" :string
                      :documentation "REQUIRED. The title of the API.")
             ("summary" :string
                        :documentation "A short summary of the API.")
             ("description" :string
                            :documentation "A description of the API. CommonMark syntax MAY be used for rich text representation.")
             ("termsOfService" :string
                               :documentation "A URL to the Terms of Service for the API. This MUST be in the form of a URL.")
             ("contact" contact
                        :documentation "The contact information for the exposed API.")
             ("license" :any
                        :documentation "The license information for the exposed API.")
             ("version" :string
                        :documentation "REQUIRED. The version of the OpenAPI document (which is distinct from the OpenAPI Specification version or the API implementation version)."))
            (:documentation "The object provides metadata about the API. The metadata MAY be used by the clients if needed, and MAY be presented in editing or documentation generation tools for convenience."))

(defmethod print-object ((object info) stream)
  (print-unreadable-object (object stream)
    (cl:format stream "~A ~A ~A"
               (title object)
               (version object)
               (let ((summary
                       (slot-value-safe object (quote summary)))
                     (description
                       (slot-value-safe object (quote description))))
                 (cond (summary
                        summary)
                       (description
                        description)
                       (t
                        ""))))))

(json-class contact nil
            (("name" :string
                     :documentation "The identifying name of the contact person/organization.")
             ("url" :string
                    :documentation "The URL pointing to the contact information. This MUST be in the form of a URL.")
             ("email" :string
                      :documentation "The email address of the contact person/organization. This MUST be in the form of an email address."))
            (:documentation "Contact information for the exposed API."))

(json-class license nil
            (("name" :string
                     :documentation "REQUIRED. The license name used for the API.")
             ("identifier" :string
                           :documentation "An SPDX license expression for the API. The identifier field is mutually exclusive of the url field.")
             ("url" :string
                    :documentation "A URL to the license used for the API. This MUST be in the form of a URL. The url field is mutually exclusive of the identifier field."))
            (:documentation "License information for the exposed API."))

(defmethod print-object ((object license) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cl:format stream "~A"
               (title (info object)))))

(json-class server nil
            (("url" :string
                    :documentation "REQUIRED. A URL to the target host. This URL supports Server Variables and MAY be relative, to indicate that the host location is relative to the location where the OpenAPI document is being served. Variable substitutions will be made when a variable is named in {brackets}.")
             ("description" :string
                            :documentation "An optional string describing the host designated by the URL. CommonMark syntax MAY be used for rich text representation.")
             ("variables" (:hash-table server-variable)
                          :documentation "A map between a variable name and its value. The value is used for substitution in the server’s URL template."))
            (:documentation "An object representing a Server."))

(defmethod print-object ((object server) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cl:format stream "url: ~A" (url object))))

(json-class server-variable nil
            (("enum" (:list :string)
                     :documentation "An enumeration of string values to be used if the substitution options are from a limited set. The array MUST NOT be empty.")
             ("default" :string
                        :documentation "REQUIRED. The default value to use for substitution, which SHALL be sent if an alternate value is not supplied. Note this behavior is different than the Schema Object’s treatment of default values, because in those cases parameter values are optional. If the enum is defined, the value MUST exist in the enum’s values.")
             ("description" :string
                            :documentation "An optional description for the server variable. CommonMark syntax MAY be used for rich text representation."))
            (:documentation "An object representing a Server Variable for server URL template substitution."))

(defmethod print-object ((object server-variable) stream)
  (print-unreadable-object (object stream :type t)
    (cl:format stream "default: ~A" (default object))))

(json-class components nil
            (("schemas" (:hash-table schema)
                        :documentation "An object to hold reusable Schema Objects.")
             ("responses" (:hash-table response)
                          :documentation "An object to hold reusable Response Objects.")
             ("parameters" (:hash-table parameter)
                           :documentation "An object to hold reusable Parameter Objects.")
             ("examples" (:hash-table example)
                         :documentation "An object to hold reusable Example Objects.")
             ("requestBodies" (:hash-table request-body)
                              :documentation "An object to hold reusable Request Body Objects.")
             ("headers" (:hash-table header)
                        :documentation "An object to hold reusable Header Objects.")
             ("securitySchemes" (:hash-table security-scheme)
                                :documentation "An object to hold reusable Security Scheme Objects.")
             ("links" (:hash-table link)
                      :documentation "An object to hold reusable Link Objects.")
             ("callbacks" (:hash-table :any)
                          :documentation "An object to hold reusable Callback Objects.")
             ("pathItems" (:hash-table :any)
                          :documentation "An object to hold reusable Path Item Object.")
             ("$ref" :string))
            (:documentation "Holds a set of reusable objects for different aspects of the OAS. All objects defined within the components object will have no effect on the API unless they are explicitly referenced from properties outside the components object."))

(json-class path nil
            (("$ref" :string
                     :documentation "Allows for a referenced definition of this path item. The referenced structure MUST be in the form of a Path Item Object. In case a Path Item Object field appears both in the defined object and the referenced object, the behavior is undefined. See the rules for resolving Relative References.")
             ("summary" :string
                        :documentation "An optional, string summary, intended to apply to all operations in this path.")
             ("description" :string
                            :documentation "An optional, string description, intended to apply to all operations in this path. CommonMark syntax MAY be used for rich text representation.")
             ("get" operation
                    :documentation "A definition of a GET operation on this path.")
             ("head" operation
                     :documentation "A definition of a HEAD operation on this path.")
             ("options" operation
                        :documentation "A definition of a OPTIONS operation on this path.")
             ("trace" operation
                      :documentation "A definition of a TRACE operation on this path.")
             ("delete" operation
                       :documentation "A definition of a DELETE operation on this path.")
             ("put" operation
                    :documentation "A definition of a PUT operation on this path.")
             ("post" operation
                     :documentation "A definition of a POST operation on this path.")
             ("patch" operation
                      :documentation "A definition of a PATCH operation on this path.")
             ("parameters" (:list parameter)
                           :documentation "A list of parameters that are applicable for all the operations described under this path. These parameters can be overridden at the operation level, but cannot be removed there. The list MUST NOT include duplicated parameters. A unique parameter is defined by a combination of a name and location. The list can use the Reference Object to link to parameters that are defined at the OpenAPI Object’s components/parameters.")
             ("servers" (:list server)
                        :documentation "An alternative server array to service all operations in this path."))
            (:documentation "Describes the operations available on a single path. A Path Item MAY be empty, due to ACL constraints. The path itself is still exposed to the documentation viewer but they will not know which operations and parameters are available."))

(defmethod print-object ((object path) stream)
  (print-unreadable-object (object stream)
    (cl:format stream "~A"
               (str:unwords (mapcar (function (lambda (operation-type)
                                      (concat "(" (downcase (symbol-name operation-type)) " "
                                              (param-case (operation-id (slot-value object operation-type)))
                                              ")")))
                                    (collect-path-types object))))))

(json-class operation nil
            (("tags" (:list :string)
                     :documentation "A list of tags for API documentation control. Tags can be used for logical grouping of operations by resources or any other qualifier.")
             ("summary" :string
                        :documentation "A short summary of what the operation does.")
             ("description" :string
                            :documentation "A verbose explanation of the operation behavior. CommonMark syntax MAY be used for rich text representation.")
             ("externalDocs" external-documentation
                             :documentation "Additional external documentation for this operation.")
             ("operationId" :string
                            :documentation "Unique string used to identify the operation. The id MUST be unique among all operations described in the API. The operationId value is case-sensitive. Tools and libraries MAY use the operationId to uniquely identify an operation, therefore, it is RECOMMENDED to follow common programming naming conventions.")
             ("parameters"  (:list parameter)
                            :documentation "A list of parameters that are applicable for this operation. If a parameter is already defined at the Path Item, the new definition will override it but can never remove it. The list MUST NOT include duplicated parameters. A unique parameter is defined by a combination of a name and location. The list can use the Reference Object to link to parameters that are defined at the OpenAPI Object’s components/parameters.")
             ("requestBody" request-body
                            :documentation "The request body applicable for this operation. The requestBody is fully supported in HTTP methods where the HTTP 1.1 specification [RFC7231] has explicitly defined semantics for request bodies. In other cases where the HTTP spec is vague (such as GET, HEAD and DELETE), requestBody is permitted but does not have well-defined semantics and SHOULD be avoided if possible.")
             ("responses" (:hash-table response)
                          :documentation "The list of possible responses as they are returned from executing this operation.

A container for the expected responses of an operation. The container maps a HTTP response code to the expected response.

The documentation is not necessarily expected to cover all possible HTTP response codes because they may not be known in advance. However, documentation is expected to cover a successful operation response and any known errors.

The default MAY be used as a default response object for all HTTP codes that are not covered individually by the Responses Object.

The Responses Object MUST contain at least one response code, and if only one response code is provided it SHOULD be the response for a successful operation call.")
             ("callbacks" (:hash-table :any)
                          :documentation "A map of possible out-of band callbacks related to the parent operation. The key is a unique identifier for the Callback Object. Each value in the map is a Callback Object that describes a request that may be initiated by the API provider and the expected responses.")
             ("deprecated" :any ;; bool
                           :documentation "Declares this operation to be deprecated. Consumers SHOULD refrain from usage of the declared operation. Default value is false.")
             ("security" :any
                         :documentation "A declaration of which security mechanisms can be used for this operation. The list of values includes alternative security requirement objects that can be used. Only one of the security requirement objects need to be satisfied to authorize a request. To make security optional, an empty security requirement ({}) can be included in the array. This definition overrides any declared top-level security. To remove a top-level security declaration, an empty array can be used.")
             ("servers" (:list server)
                        :documentation "An alternative server array to service this operation. If an alternative server object is specified at the Path Item Object or Root level, it will be overridden by this value."))
            (:documentation "Describes a single API operation on a path."))

(defmethod print-object ((object operation) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((operation-id
            (slot-value-safe object (quote operation-id))))
      (if operation-id
          (cl:format stream "operation-id: ~A" operation-id)
          (cl:format stream "")))))

(json-class external-documentation nil
            (("description" :string
                            :documentation "A description of the target documentation. CommonMark syntax MAY be used for rich text representation.")
             ("url" :string
                    :documentation "REQUIRED. The URL for the target documentation. This MUST be in the form of a URL."))
            (:documentation "Allows referencing an external resource for extended documentation."))

(defmethod print-object ((object external-documentation) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cl:format stream "url: ~A" (url object))))

(json-class parameter ()
            (("name" :string
                     :documentation "REQUIRED. The name of the parameter. Parameter names are case sensitive.

    If in is \"path\", the name field MUST correspond to a template expression occurring within the path field in the Paths Object. See Path Templating for further information.
    If in is \"header\" and the name field is \"Accept\", \"Content-Type\" or \"Authorization\", the parameter definition SHALL be ignored.
    For all other cases, the name corresponds to the parameter name used by the in property.")
             ("in" :string
                   :documentation "REQUIRED. The location of the parameter. Possible values are \"query\", \"header\", \"path\" or \"cookie\".")
             ("description" :string
                            :documentation "A brief description of the parameter. This could contain examples of use. CommonMark syntax MAY be used for rich text representation.")
             ("required" :any ;; :bool
                         :documentation "Determines whether this parameter is mandatory. If the parameter location is \"path\", this property is REQUIRED and its value MUST be true. Otherwise, the property MAY be included and its default value is false.")
             ("deprecated" :any ;; :bool
                           :documentation "Specifies that a parameter is deprecated and SHOULD be transitioned out of usage. Default value is false.")
             ("allowEmptyValue" :any ;; :bool
                                :documentation "Sets the ability to pass empty-valued parameters. This is valid only for query parameters and allows sending a parameter with an empty value. Default value is false. If style is used, and if behavior is n/a (cannot be serialized), the value of allowEmptyValue SHALL be ignored. Use of this property is NOT RECOMMENDED, as it is likely to be removed in a later revision.")
             ("style" :string
                      :documentation "Describes how the parameter value will be serialized depending on the type of the parameter value. Default values (based on value of in): for query - form; for path - simple; for header - simple; for cookie - form.")
             ("explode" :any
                        :documentation "When this is true, parameter values of type array or object generate separate parameters for each value of the array or key-value pair of the map. For other types of parameters this property has no effect. When style is form, the default value is true. For all other styles, the default value is false.") ;; bool
             ("allowReserved" :any
                              :documentation "Determines whether the parameter value SHOULD allow reserved characters, as defined by [RFC3986] :/?#[]@!$&'()*+,;= to be included without percent-encoding. This property only applies to parameters with an in value of query. The default value is false.") ;; bool, only if in-query
             ("schema" schema
                       :documentation "The schema defining the type used for the parameter.") ;; only if in is body
             ("example" :any
                        :documentation "Example of the parameter’s potential value. The example SHOULD match the specified schema and encoding properties if present. The example field is mutually exclusive of the examples field. Furthermore, if referencing a schema that contains an example, the example value SHALL override the example provided by the schema. To represent examples of media types that cannot naturally be represented in JSON or YAML, a string value can contain the example with escaping where necessary.")
             ("examples" (:hash-table example)
                         :documentation "Examples of the parameter’s potential value. Each example SHOULD contain a value in the correct format as specified in the parameter encoding. The examples field is mutually exclusive of the example field. Furthermore, if referencing a schema that contains an example, the examples value SHALL override the example provided by the schema.")
             ("content" (:hash-table content)
                        :documentation "A map containing the representations for the parameter. The key is the media type and the value describes it. The map MUST only contain one entry.")
             ("$ref" :string))
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

(json-class request-body nil
            (("$ref"     :string)
             ("description" :string
                            :documentation "A brief description of the request body. This could contain examples of use. CommonMark syntax MAY be used for rich text representation.")
             ("required" :any
                         :documentation "Determines if the request body is required in the request. Defaults to false.") ;; bool
             ("content"  (:hash-table media-type)
                         :documentation "REQUIRED. The content of the request body. The key is a media type or media type range and the value describes it. For requests that match multiple keys, only the most specific key is applicable. e.g. text/plain overrides text/*"))
            (:documentation "Describes a single request body."))

(defmethod print-object ((object request-body) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (let ((content-type
            (car (hash-keys (slot-value-safe object (quote content))))))
      (if content-type
          (cl:format stream "content-type: ~A" content-type)
          (cl:format stream "")))))

(json-class media-type nil
            (("schema" schema
                       :documentation "The schema defining the content of the request, response, or parameter.")
             ("example" :any
                        :documentation "Example of the media type. The example object SHOULD be in the correct format as specified by the media type. The example field is mutually exclusive of the examples field. Furthermore, if referencing a schema which contains an example, the example value SHALL override the example provided by the schema.")
             ("examples" (:hash-table example)
                         :documentation "Examples of the media type. Each example object SHOULD match the media type and specified schema if present. The examples field is mutually exclusive of the example field. Furthermore, if referencing a schema which contains an example, the examples value SHALL override the example provided by the schema.")
             ("encoding" (:hash-table encoding)
                         :documentation "A map between a property name and its encoding information. The key, being the property name, MUST exist in the schema as a property. The encoding object SHALL only apply to requestBody objects when the media type is multipart or application/x-www-form-urlencoded."))
            (:documentation "Each Media Type Object provides schema and examples for the media type identified by its key."))

(json-class encoding nil
            (("contentType" :string
                            :documentation "The Content-Type for encoding a specific property. Default value depends on the property type: for object - application/json; for array – the default is defined based on the inner type; for all other cases the default is application/octet-stream. The value can be a specific media type (e.g. application/json), a wildcard media type (e.g. image/*), or a comma-separated list of the two types.")
             ("headers" (:hash-table header)
                        :documentation "A map allowing additional information to be provided as headers, for example Content-Disposition. Content-Type is described separately and SHALL be ignored in this section. This property SHALL be ignored if the request body media type is not a multipart.")
             ("style" :string
                      :documentation "Describes how a specific property value will be serialized depending on its type. See Parameter Object for details on the style property. The behavior follows the same values as query parameters, including default values. This property SHALL be ignored if the request body media type is not application/x-www-form-urlencoded or multipart/form-data. If a value is explicitly defined, then the value of contentType (implicit or explicit) SHALL be ignored.")
             ("explode" :any
                        :documentation "When this is true, property values of type array or object generate separate parameters for each value of the array, or key-value-pair of the map. For other types of properties this property has no effect. When style is form, the default value is true. For all other styles, the default value is false. This property SHALL be ignored if the request body media type is not application/x-www-form-urlencoded or multipart/form-data. If a value is explicitly defined, then the value of contentType (implicit or explicit) SHALL be ignored.") ;; bool
             ("allowReserved" :any
                              :documentation "Determines whether the parameter value SHOULD allow reserved characters, as defined by [RFC3986] :/?#[]@!$&'()*+,;= to be included without percent-encoding. The default value is false. This property SHALL be ignored if the request body media type is not application/x-www-form-urlencoded or multipart/form-data. If a value is explicitly defined, then the value of contentType (implicit or explicit) SHALL be ignored."))
            (:documentation "A single encoding definition applied to a single schema property.")) ;; bool

(json-class response nil
            (("description" :string
                            :documentation "REQUIRED. A description of the response. CommonMark syntax MAY be used for rich text representation.")
             ("headers" (:hash-table header)
                        :documentation "Maps a header name to its definition. [RFC7230] states header names are case insensitive. If a response header is defined with the name \"Content-Type\", it SHALL be ignored.")
             ("content" (:hash-table media-type)
                        :documentation "Maps a header name to its definition. [RFC7230] states header names are case insensitive. If a response header is defined with the name \"Content-Type\", it SHALL be ignored.")
             ("links" (:hash-table link)
                      :documentation "A map of operations links that can be followed from the response. The key of the map is a short name for the link, following the naming constraints of the names for Component Objects.")
             ("$ref" :string))
            (:documentation "Describes a single response from an API Operation, including design-time, static links to operations based on the response."))

(json-class example nil
            (("summary" :string
                        :documentation "Short description for the example.")
             ("description" :string
                            :documentation "Long description for the example. CommonMark syntax MAY be used for rich text representation.")
             ("value" :any
                      :documentation "Embedded literal example. The value field and externalValue field are mutually exclusive. To represent examples of media types that cannot naturally represented in JSON or YAML, use a string value to contain the example, escaping where necessary.")
             ("externalValue" :string
                              :documentation "A URI that points to the literal example. This provides the capability to reference examples that cannot easily be included in JSON or YAML documents. The value field and externalValue field are mutually exclusive. See the rules for resolving Relative References.")
             ("$ref"        :string)))

(json-class link nil
            (("operationRef" :string
                             :documentation "A relative or absolute URI reference to an OAS operation. This field is mutually exclusive of the operationId field, and MUST point to an Operation Object. Relative operationRef values MAY be used to locate an existing Operation Object in the OpenAPI definition. See the rules for resolving Relative References.")
             ("operationId" :string
                            :documentation "The name of an existing, resolvable OAS operation, as defined with a unique operationId. This field is mutually exclusive of the operationRef field.")
             ("parameters" (:hash-table :any)
                           :documentation "A map representing parameters to pass to an operation as specified with operationId or identified via operationRef. The key is the parameter name to be used, whereas the value can be a constant or an expression to be evaluated and passed to the linked operation. The parameter name can be qualified using the parameter location [{in}.]{name} for operations that use the same parameter name in different locations (e.g. path.id).") ;; was list
             ("requestBody" :any
                            :documentation "A literal value or {expression} to use as a request body when calling the target operation.")
             ("description" :string
                            :documentation "A description of the link. CommonMark syntax MAY be used for rich text representation.")
             ("server" server
                       :documentation "	 server object to be used by the target operation.")
             ("$ref" :string))
            (:documentation "The Link object represents a possible design-time link for a response. The presence of a link does not guarantee the caller’s ability to successfully invoke it, rather it provides a known relationship and traversal mechanism between responses and other operations.

Unlike dynamic links (i.e. links provided in the response payload), the OAS linking mechanism does not require link information in the runtime response.

For computing links, and providing instructions to execute them, a runtime expression is used for accessing values in an operation and using them as parameters while invoking the linked operation.

A linked operation MUST be identified using either an operationRef or operationId. In the case of an operationId, it MUST be unique and resolved in the scope of the OAS document. Because of the potential for name clashes, the operationRef syntax is preferred for OpenAPI documents with external references."))

(json-class header nil
            (("description" :string)
             ("required"    :any) ;; bool
             ("deprecated"  :any) ;; bool
             ("schema"      schema)
             ("content"     :content)
             ("style"       :string)
             ("explode"     :any)  ;; bool
             ("$ref"        :string))
            (:documentation "The Header Object follows the structure of the Parameter Object with the following changes:
  1. name MUST NOT be specified, it is given in the corresponding headers map.
  2. in MUST NOT be specified, it is implicitly in header.
  3. All traits that are affected by the location MUST be applicable to a location of header (for example, style)."))

(json-class tag nil
            (("name" :string
                     :documentation "REQUIRED. The name of the tag.")
             ("description" :string
                            :documentation "A description for the tag. CommonMark syntax MAY be used for rich text representation.")
             ("externalDocs" external-documentation
                             :documentation "Additional external documentation for this tag."))
            (:documentation "Adds metadata to a single tag that is used by the Operation Object. It is not mandatory to have a Tag Object per tag defined in the Operation Object instances."))

(defmethod print-object ((object tag) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cl:format stream "~A" (name object))))

(json-class reference nil
            (("$ref" :string
                     :documentation "REQUIRED. The reference identifier. This MUST be in the form of a URI.")
             ("summary" :string
                        :documentation "A short summary which by default SHOULD override that of the referenced component. If the referenced object-type does not allow a summary field, then this field has no effect.")
             ("description" :string
                            :documentation "A description which by default SHOULD override that of the referenced component. CommonMark syntax MAY be used for rich text representation. If the referenced object-type does not allow a description field, then this field has no effect."))
            (:documentation "A simple object to allow referencing other components in the OpenAPI document, internally and externally.

The $ref string value contains a URI [RFC3986], which identifies the location of the value being referenced.

See the rules for resolving Relative References.

This object cannot be extended with additional properties and any properties added SHALL be ignored.

Note that this restriction on additional properties is a difference between Reference Objects and Schema Objects that contain a $ref keyword."))

(defmethod print-object ((object reference) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cl:format stream "~A" (slot-value object (quote ref)))))

(defmethod print-object ((object reference) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cl:format stream "~A" (ref object))))

(json-class schema nil
            (;;root
             ("title"       :string)
             ;; core
             ("$schema"     :any
                            :documentation "Specifying Schema Dialects

It is important for tooling to be able to determine which dialect or meta-schema any given resource wishes to be processed with: JSON Schema Core, JSON Schema Validation, OpenAPI Schema dialect, or some custom meta-schema.

The $schema keyword MAY be present in any root Schema Object, and if present MUST be used to determine which dialect should be used when processing the schema. This allows use of Schema Objects which comply with other drafts of JSON Schema than the default Draft 2020-12 support. Tooling MUST support the OAS dialect schema id, and MAY support additional values of $schema.

To allow use of a different default $schema value for all Schema Objects contained within an OAS document, a jsonSchemaDialect value may be set within the OpenAPI Object. If this default is not set, then the OAS dialect schema id MUST be used for these Schema Objects. The value of $schema within a Schema Object always overrides any default.

When a Schema Object is referenced from an external resource which is not an OAS document (e.g. a bare JSON Schema resource), then the value of the $schema keyword for schemas within that resource MUST follow JSON Schema rules")
             ("$vocabulary" :any)

             ;; Base URI
             ("$id"            :any)
             ;; location-independent
             ("$anchor"        :any)
             ("$dynamicAnchor" :any)
             ;; Schema Refeences
             ("$ref"        :string)
             ("$dynamicRef" :string)
             ;; Schema Re-Use
             ("$defs" :any)
             ;; Comments
             ("$comment" :any)

             ;; fixed fields
             ("discriminator" discriminator
                              :documentation "Adds support for polymorphism. The discriminator is an object name that is used to differentiate between other schemas which may satisfy the payload description. See Composition and Inheritance for more details.")
             ("xml" :any
                    :documentation "This MAY be used only on properties schemas. It has no effect on root schemas. Adds additional metadata to describe the XML representation of this property.

XML Modeling
The xml property allows extra definitions when translating the JSON definition to XML. The XML Object contains additional information about the available options.") ;; xml object
             ("externalDocs" external-documentation
                             :documentation "Additional external documentation for this schema.")

             ;; any
             ("type"  :any)
             ("enum" :any)
             ("const" :any)
             ;; numeric
             ("multipleOf"  :number)
             ("maximum"     :number)
             ("exclusiveMaximum" :any) ;; should be bool
             ("minimum"     :number)
             ("exclusiveMinimum" :any) ;; should be bool
             ;; Validation Keywords for Strings
             ("maxLength"   :number :documentation "non-negative integer")
             ("minLength" :number :documentation "non-negative integer")
             ("pattern" :string :documentation "ECMA-262 regular expression")
             ;; Validation Keywords for Arrays = list
             ("maxItems" :number :documentation "non-negative integer")
             ("minItems" :number :documentation "non-negative integer")
             ("uniqueItems" :any :documentation "boolean")
             ("maxContains" :number :documentation "non-negative integer")
             ("minContains" :number :documentation "non-negative integer")
             ;; Validation Keywords for Objects (hash-table)
             ("maxProperties" :number :documentation "non-negative integer")
             ("minProperties" :number :documentation "non-negative integer")
             ("required"    (:list :string))
             ("dependendRequired" :hash-table)

             ;; String-Encoded Data
             ("contentEncoding" :string)
             ("contentMediaType" :string)
             ("contentSchema" :string)

             ;; Basic Meta-Data
             ("description" :string)
             ("default"     :any)
             ("deprecated" :any) ;; bool
             ("readOnly"   :any)
             ("examples"   :any)

             ;; Subschema Logic
             ("allOf" :any)
             ("anyOf" (:list any-of))
             ("oneOf" (:list items))
             ("not" :any)

             ;; Subschemas Conditionally

             ;; ("if"   :any)
             ;; ("then" :any)
             ("else" :any)
             ("dependentSchemas" :any)

             ;; Keywords for Applying Subschemas to Child Instances
             ;; Subschemas to Arrays
             ("prefixItems" :any)
             ("items"       items)
             ("contains" :any)
             ;;Subschemas to Objects
             ("properties" (:hash-table property))
             ("patternProperties" :any)
             ("additionalProperties"  :any)
             ("propertyNames" :any)

             ;; dependent
             ("unevaluatedItems" :any)
             ("unevaluatedProperties" :any)

             ("format" :any))
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
(json-class discriminator nil
            (("propertyName" :string
                             :documentation "REQUIRED. The name of the property in the payload that will hold the discriminator value.")
             ("mapping" (:hash-table :string)
                        :documentation "An object to hold mappings between payload values and schema names or references."))
            (:documentation "When request bodies or response payloads may be one of a number of different schemas, a discriminator object can be used to aid in serialization, deserialization, and validation. The discriminator is a specific object in a schema which is used to inform the consumer of the document of an alternative schema based on the value associated with it.

When using the discriminator, inline schemas will not be considered.

This object MAY be extended with Specification Extensions.

The discriminator object is legal only when using one of the composite keywords oneOf, anyOf, allOf."))

(defmethod print-object ((object discriminator) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cl:format stream "~A" (property-name object))))

(json-class xml nil
            (("name" :string
                     :documentation "Replaces the name of the element/attribute used for the described schema property. When defined within items, it will affect the name of the individual XML elements within the list. When defined alongside type being array (outside the items), it will affect the wrapping element and only if wrapped is true. If wrapped is false, it will be ignored.")
             ("namespace" :string
                          :documentation "The URI of the namespace definition. This MUST be in the form of an absolute URI.")
             ("prefix" :string
                       :documentation "The prefix to be used for the name.")
             ("attribute" :any
                          :documentation "Declares whether the property definition translates to an attribute instead of an element. Default value is false.")
             ("wrapped" :any ;; bool
                        :documentation "MAY be used only for an array definition. Signifies whether the array is wrapped (for example, <books><book/><book/></books>) or unwrapped (<book/><book/>). Default value is false. The definition takes effect only when defined alongside type being array (outside the items)."))
            (:documentation "A metadata object that allows for more fine-tuned XML model definitions.

When using arrays, XML element names are not inferred (for singular/plural forms) and the name property SHOULD be used to add that information. See examples for expected behavior."))

(json-class security-scheme nil
            (("type" :string
                     :documentation "REQUIRED. The type of the security scheme. Valid values are \"apiKey\", \"http\", \"mutualTLS\", \"oauth2\", \"openIdConnect\".")
             ("description" :string
                            :documentation "A description for security scheme. CommonMark syntax MAY be used for rich text representation.")
             ("name" :string
                     :documentation "REQUIRED. The name of the header, query or cookie parameter to be used.")
             ("in" :string
                   :documentation "REQUIRED. The location of the API key. Valid values are \"query\", \"header\" or \"cookie\".")
             ("scheme" :string
                       :documentation "REQUIRED. The name of the HTTP Authorization scheme to be used in the Authorization header as defined in [RFC7235]. The values used SHOULD be registered in the IANA Authentication Scheme registry.")
             ("bearerFormat" :string
                             :documentation "A hint to the client to identify how the bearer token is formatted. Bearer tokens are usually generated by an authorization server, so this information is primarily for documentation purposes.")
             ("flows" o-auth-flow
                      :documentation "REQUIRED. An object containing configuration information for the flow types supported.")
             ("openIdConnectUrl" :string
                                 :documentation "REQUIRED. OpenId Connect URL to discover OAuth2 configuration values. This MUST be in the form of a URL. The OpenID Connect standard requires the use of TLS.")
             ("$ref" :string))
            (:documentation "Defines a security scheme that can be used by the operations.

Supported schemes are HTTP authentication, an API key (either as a header, a cookie parameter or as a query parameter), mutual TLS (use of a client certificate), OAuth2’s common flows (implicit, password, client credentials and authorization code) as defined in [RFC6749], and OpenID Connect Discovery. Please note that as of 2020, the implicit flow is about to be deprecated by OAuth 2.0 Security Best Current Practice. Recommended for most use case is Authorization Code Grant flow with PKCE."))

(defmethod print-object ((object security-scheme) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cl:format stream "type: ~A, name: ~A, in: ~A, scheme: ~A"
               (type object) (name object) (in object) (scheme object))))

(json-class o-auth-flows nil
            (("implicit"  o-auth-flow
                          :documentation "Configuration for the OAuth Implicit flow")
             ("password" o-auth-flow
                         :documentation "Configuration for the OAuth Resource Owner Password flow")
             ("clientCredentials" o-auth-flow
                                  :documentation "Configuration for the OAuth Client Credentials flow. Previously called application in OpenAPI 2.0.")
             ("authorizationCode" o-auth-flow
                                  :documentation "Configuration for the OAuth Authorization Code flow. Previously called accessCode in OpenAPI 2.0."))
            (:documentation "Allows configuration of the supported OAuth Flows."))

(json-class o-auth-flow nil
            (("authorizationUrl" :string
                                 :documentation "REQUIRED. The authorization URL to be used for this flow. This MUST be in the form of a URL. The OAuth2 standard requires the use of TLS.")
             ("tokenUrl" :string
                         :documentation "REQUIRED. The token URL to be used for this flow. This MUST be in the form of a URL. The OAuth2 standard requires the use of TLS.")
             ("refreshUrl" :string
                           :documentation "The URL to be used for obtaining refresh tokens. This MUST be in the form of a URL. The OAuth2 standard requires the use of TLS.")
             ("scopes" (:hash-table :string)
                       :documentation "REQUIRED. The available scopes for the OAuth2 security scheme. A map between the scope name and a short description for it. The map MAY be empty."))
            (:documentation "Configuration details for a supported OAuth Flow"))

(defmethod print-object ((object o-auth-flow) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (cl:format stream "authorizationUrl: ~A, tokenUrl: ~A"
               (authorization-url object) (token-url object))))

(json-class path-item-or-reference nil
            (("title"       :string)))

(json-class items nil
            (("title"       :string)
             ("type"        :any
                            :documentation "String or list (vector)")
             ("items"       items)
             ("minItems"    :number)
             ("maxItems"    :number)
             ("minLength"   :number)
             ("maxLength"   :number)
             ("example"     :any)
             ("default"     :any)
             ("$ref"        :string)
             ("description" :string)
             ("anyOf"       (:list any-of))
             ("required"    (:list :string))
             ("properties"  (:hash-table property))
             ("enum"        :list)))

(json-class property nil
            (("description" :string)
             ("$ref"        :string)
             ("title"       :string)
             ("type"        :any
                            :documentation "string or vector (list)")
             ("additionalProperties" :any) ;; should be :bool
             ("required"    :list)
             ("maximum"     :number)
             ("minimum"     :number)
             ("enum"        :any) ;;hash-table?
             ("anyOf"       :any) ;;hash-table?
             ("oneOf"       (:list items))
             ("contentMediaType" :string)
             ("properties"  (:hash-table property))
             ("format"      :string)
             ("items"       items)
             ("example"     :any)
             ("default"     :any)
             ("nullable"    :any))) ;; should be :bool

(json-class all-of nil
            (("title"       :string)
             ("$ref"        :string)
             ("type"        :string)
             ("description" :string)
             ("required"    :list)
             ("allOf"       (:list all-of))
             ("properties"  (:hash-table property))))

(json-class any-of nil
            (("title"       :string)
             ("$ref"        :string)
             ("type"        :string)
             ("description" :string)
             ("allOf"       (:list all-of))))

(json-class definition nil
            (("description" :string)
             ("properties"  (:hash-table property))
             ("required"    :list)
             ("type"        :string)))

(json-class content nil
            (("schema" schema)
             ("example" :any)
             ("examples" :any))) ;; string or list
