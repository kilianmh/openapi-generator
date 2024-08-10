(in-package #:openapi-generator)

(define-json-class apis-guru-list nil
  (("api-list" (:hash-table apis-guru-api) :reader api-list)))

(define-json-class apis-guru-api nil
  (("added" :string
            :documentation "Timestamp when the API was first added to the directory")
   ("preferred" :string
                :reader preferred
                :documentation "Recommended version")
   ("versions" (:hash-table apis-guru-api-version)
               :reader versions)))

(defmethod print-object ((object apis-guru-api) stream)
  (print-unreadable-object (object stream :type t)
    (cl:format stream "updated: ~A, version: ~A"
               (substring 0 10 (updated (gethash (preferred object) (versions object))))
               (preferred object))))

(define-json-class apis-guru-api-version nil
  (("added" :string
            :documentation "Timestamp when the version was added")
   ("externalDocs" apis-guru-external-documentation
                   :documentation "Copy of `externalDocs` s…from OpenAPI definition")
   ("info" apis-guru-info
           :reader info
           :documentation "Copy of `info` section from OpenAPI definition")
   ("swaggerURL" :string
                 :documentation "URL to OpenAPI definition in JSON format")
   ("swaggerYamlUrl" :string
                     :documentation "URL to OpenAPI definition in YAML format")
   ("updated" :any ; should be string or nil
              :reader updated
              :documentation "Timestamp when the version was updated")
   ("openapiVer" :string)))

(define-json-class apis-guru-external-documentation (external-documentation)
  (("updated" :string)))

(define-json-class apis-guru-info nil
  (("contact" :hash-table)
   ("description" :string)
   ("title" :string)
   ("version" :string)
   ("x-apisguru-categories" (:list :string))
   ("x-logo" apis-guru-url)
   ("x-origin" (:list apis-guru-origin) :reader x-origin)
   ("x-providerName" :string)))

(define-json-class apis-guru-url nil
  (("url" :string :reader url)))

(define-json-class apis-guru-origin nil
  (("format" :string :reader apis-guru-origin-format)
   ("url" :string :reader url)
   ("version" :string)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-apis-guru-list ()
    "https://api.apis.guru/v2/"
    (dex:get
     (render-uri (make-uri :scheme "https" :host "api.apis.guru"
                           :path "v2/list.json"))))

  (defun get-api-guru-apis (&key refresh)
    (let ((file-path
            (system-relative-pathname "openapi-generator"
                                      "data/apis-guru-apis.json")))
      (when refresh
        (delete-file-if-exists file-path))
      (ensure-directories-exist file-path)
      (let* ((content-hash-table
               (yason:parse
                (if (file-exists-p file-path)
                    (from-file file-path)
                    (to-file file-path
                             (get-apis-guru-list)))))
             (apis-guru-apis-object
               (api-list (json-to-clos (dict "api-list"
                                             content-hash-table)
                                       (quote apis-guru-list)))))
        apis-guru-apis-object))))

(defparameter apis-guru-apis
  (get-api-guru-apis))

(defun apis-guru-url (api-name &key (origin t))
  (let ((api-object
          (gethash api-name apis-guru-apis)))
    (if api-object
        (let* ((preferred-version
                 (gethash (preferred api-object)
                          (versions api-object)))
               (origin-url
                 (url (car (x-origin (info preferred-version)))))
               (swagger-url
                 (slot-value-safe preferred-version (quote swagger-url)))
               (swagger-yaml-url
                 (slot-value-safe preferred-version (quote swagger-yaml-url))))
          (cond (origin
                 (return-from apis-guru-url
                   origin-url))
                (swagger-url
                 swagger-url)
                (swagger-yaml-url
                 swagger-yaml-url)
                (t
                 origin-url)))
        (error (concat "The id " api-name " does not exist in the apis-guru database.")))))
