(cl:in-package #:openapi-generator)

(defgeneric get-openapi-version (openapi)
  (:documentation "Extract Swagger/Openapi version from api spec")
  (:method ((openapi openapi))
    (openapi openapi))
  (:method ((openapi hash-table))
    (let* ((version-string
             (or (gethash "openapi" openapi)
                 (gethash "swagger" openapi)))
           (version
             (read-version-from-string version-string)))
      (cond ((version= (read-version-from-string "2.0")
                       version)
             (quote openapi-2.0))
            ((and (version>= version
                             (read-version-from-string "3.0"))
                  (version<= version
                             (read-version-from-string "3.0.3")))
             (quote openapi-3.0))
            ((version= (read-version-from-string "3.1")
                       version)
             (quote openapi-3.1))
            (t
             (error "Version ~A is not supported by this library" version-string)))))
  (:method ((openapi string))
    (funcall (function get-openapi-version) (yason:parse openapi)))
  (:method ((openapi pathname))
    (funcall (function get-openapi-version) (from-file openapi))))

(defgeneric parse-url (name url)
  (:documentation "Parse url into package and save file in openapi-generator/data.
Supported are: url / apis-guru / path / name (in openapi-generator/data folder)")
  (:method ((file-name string) (url string))
    (to-file (get-data-file file-name)
             (funcall (function parse-url) nil url)))
  (:method ((file-name null) (url string))
    (let ((url-ending
            (substring (- (length url) 4)
                       (length url)
                       url)))
      (case-using (function string-equal) url-ending
        (("yaml" ".yml")
         (convert-to-openapi-3 :url url))
        ("json"
         (let ((result
                 (dex:get url)))
           (if (eql (get-openapi-version result)
                    (quote openapi-2.0))
               (convert-to-openapi-3 :url url)
               result)))))))

(defgeneric parse-string (file-name file)
  (:documentation "Safe string to file in local folder")
  (:method ((file-name string) (file string))
    (let ((target-directory
            (get-data-file file-name)))
      (if (starts-with-p "{" file)
          (case (get-openapi-version file)
            (openapi-2.0
             (str:to-file target-directory
                          (convert-to-openapi-3 :content file
                                                :content-type "json")))
            ((openapi-3.0 openapi-3.1)
             file)
            (otherwise
             (error "Unsupported Openapi Version")))
          (str:to-file target-directory (convert-to-openapi-3 :content file
                                                              :content-type "yaml"))))))

(defgeneric parse-directory (source-directory target-directory)
  (:documentation "Parse file from source directory to target-directory as usable JSON Openapi 3.X")
  (:method ((source-directory pathname) (target-directory pathname))
    (let* ((file-content
             (from-file source-directory))
           (json-content
             (ecase-using (function string=) (file-type source-directory)
               ("yaml"
                (to-file target-directory
                         (convert-to-openapi-3 :content file-content :content-type "yaml")))
               ("json"
                (if (eql (get-openapi-version file-content)
                         (quote openapi-2.0))
                    (to-file target-directory
                             (convert-to-openapi-3 :content file-content))
                    file-content)))))
      json-content)))

(defgeneric parse-apis-guru-id (file-name apis-guru-id)
  (:documentation "parse api guru name with parse url")
  (:method ((file-name string) (apis-guru-id string))
    (parse-url file-name (apis-guru-url apis-guru-id))))

(defgeneric parse-openapi (name &key url source-directory collection-id content)
  (:documentation "Parse json/yaml from a file or uri into openapi class instance
You should mostly submit a file-name, and either ")
  (:method ((name string) &key url source-directory collection-id content)
    (let* ((file-pathname
             (make-pathname :directory (trim (directory-namestring constant-data-directory)
                                             :char-bag "/")
                            :name name :type "json"))
           (result
             (cond (source-directory
                    (parse-directory source-directory file-pathname))
                   (url
                    (parse-url name url))
                   (collection-id
                    (parse-apis-guru-id name collection-id))
                   (content
                    (parse-string name content))
                   (t
                    (cond ((file-exists-p (get-data-file name))
                           (let* ((content
                                    (from-file (get-data-file name)))
                                  (openapi-version
                                    (get-openapi-version content)))
                             (case openapi-version
                               (openapi-2.0 (convert-to-openapi-3 :content content))
                               ((openapi-3.0 openapi-3.1) content)
                               (otherwise (error "The Version ~W is not supported"
                                                 openapi-version)))))
                          ((uiop:file-exists-p (get-data-file name :type "yaml"))
                           (str:to-file (get-data-file name)
                                        (convert-to-openapi-3 :pathname (get-data-file name :type "yaml")
                                                              :content-type "yaml")))
                          ((uiop:file-exists-p (get-data-file name :type "yml"))
                           (str:to-file (get-data-file name)
                                        (convert-to-openapi-3 :pathname (get-data-file name :type "yml")
                                                              :content-type "yaml")))
                          (t
                           (error (concat "There is no " name " json/yaml in the openapi-generator/data folder
Alternativeyl you can supply one of the keyword parameters (source-directory, apis-guru-id, file-content, url)"))))))))
      (json-to-clos result (get-openapi-version result)))))
