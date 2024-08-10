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

(defgeneric parse-url (name url &key converter-url dereference)
  (:documentation "Parse url into package and save file in openapi-generator/data.
Supported are: url / apis-guru / path / name (in openapi-generator/data folder)")
  (:method ((file-name string) (url string) &key (converter-url *converter-url*) dereference)
    (to-file (get-data-file file-name)
             (funcall (function parse-url) nil url
                      :converter-url converter-url
                      :dereference dereference)))
  (:method ((file-name null) (url string) &key (converter-url *converter-url*) dereference)
    (let* ((url-ending
             (substring (- (length url) 4)
                        (length url)
                        url))
           (converted-content
             (case-using (function string-equal) url-ending
               (("yaml" ".yml")
                (convert-to-openapi-3 :url url :converter-url converter-url))
               ("json"
                (let ((result
                        (dex:get url)))
                  (if (eql (get-openapi-version result)
                           (quote openapi-2.0))
                      (convert-to-openapi-3 :url url :converter-url converter-url)
                      result))))))
      (if dereference
          (stringify (dereference (parse converted-content)
                                  :path-uri (quri:uri url)))
          converted-content))))

(defgeneric parse-string (file-name file &key converter-url)
  (:documentation "Safe string to file in local folder")
  (:method ((file-name string) (file string) &key (converter-url *converter-url*))
    (let ((target-directory
            (get-data-file file-name)))
      (if (starts-with-p "{" file)
          (case (get-openapi-version file)
            (openapi-2.0
             (to-file target-directory
                      (convert-to-openapi-3 :content file
                                            :content-type "json"
                                            :converter-url converter-url)))
            ((openapi-3.0 openapi-3.1)
             file)
            (otherwise
             (error "Unsupported Openapi Version")))
          (to-file target-directory (convert-to-openapi-3 :content file
                                                          :content-type "yaml"))))))

(defgeneric parse-directory (source-directory target-directory &key converter-url dereference)
  (:documentation "Parse file from source directory to target-directory as usable JSON Openapi 3.X")
  (:method ((source-directory pathname) (target-directory pathname) &key (converter-url *converter-url*) dereference)
    (let* ((file-content
             (if dereference
                 (stringify (dereference (pathname source-directory)))
                 (from-file source-directory)))
           (json-content
             (ecase-using (function string=) (file-type source-directory)
               ("yaml"
                (to-file target-directory
                         (convert-to-openapi-3 :content file-content
                                               :content-type "yaml"
                                               :converter-url converter-url)))
               ("json"
                (if (eql (get-openapi-version file-content)
                         (quote openapi-2.0))
                    (to-file target-directory
                             (convert-to-openapi-3 :content file-content
                                                   :converter-url converter-url))
                    file-content)))))
      json-content)))

(defgeneric parse-apis-guru-id (file-name apis-guru-id &key converter-url)
  (:documentation "parse api guru name with parse url")
  (:method ((file-name string) (apis-guru-id string) &key (converter-url *converter-url*))
    (parse-url file-name (apis-guru-url apis-guru-id) :converter-url converter-url)))

(defmethod ensure-json ((content string))
  "Ensures document is a json"
  (let ((first-letter
          (subseq content 0 1)))
    (serapeum:case-using #'string= first-letter
      (("[" "{") content)
      (t (stringify (cl-yaml:parse content))))))

(defmethod dereference ((table hash-table) &key path-uri pathname)
  "Dereference all references recursively.
Also grab external files.
  Exit recursion & warn when circular pointer detected"
  (let ((dereferenced-table (hash-copy-recursive table))
        (circular-pointer nil))
    (labels ((map-vector (vec used-pointer path)
               (let ((counter -1))
                 (mapc (function (lambda (element)
                         (declare (ignore element))
                         (funcall (function %dereference)
                                  :used-pointer used-pointer
                                  :path (append path (list (incf counter))))))
                       (coerce vec 'list))))
             (map-ht (ht used-pointer path)
               (maphash (function (lambda (key value)
                          (declare (ignore value))
                          (funcall (function %dereference)
                                   :path (append path (list key))
                                   :used-pointer used-pointer)))
                        ht))
             (%dereference (&key path used-pointer)
               (let ((entry
                       (if path (hash-get dereferenced-table path)
                           dereferenced-table)))
                 (typecase entry
                   (hash-table
                    (let ((pointer (gethash "$ref" entry)))
                      (if (and pointer (stringp pointer))
                          (if (member pointer used-pointer :test (function string=))
                              (unless (member pointer circular-pointer :test (function string=))
                                (push pointer circular-pointer)
                                (warn "circular pointer detected in ~A" pointer))
                              (if (str:starts-with-p "." pointer)
                                  (let* ((split-pointer
                                           (str:split "#" pointer))
                                         (document-location
                                           (first split-pointer))
                                         (next-pointer
                                           (second split-pointer))
                                         (rendered-uri
                                           (quri:render-uri (quri:merge-uris document-location (or path-uri
                                                                                                   (namestring pathname)))))
                                         (next-document nil))
                                    (cond (path-uri
                                           (setf next-document
                                                 (ensure-json (dex:get rendered-uri))))
                                          (pathname
                                           (setf next-document
                                                 (ensure-json (str:from-file (pathname rendered-uri))))))
                                    (setf (hash-get dereferenced-table path)
                                          (if next-pointer
                                              (get-by-json-pointer
                                               next-document
                                               next-pointer)
                                              (parse next-document))))
                                  (let ((pointer-value
                                          (get-by-json-pointer dereferenced-table pointer)))
                                    (etypecase pointer-value
                                      (null nil)
                                      (hash-table
                                       (map-ht (setf (hash-get dereferenced-table path)
                                                     (hash-copy-recursive pointer-value))
                                               (cons pointer used-pointer) path))
                                      (vector
                                       (map-vector (setf (hash-get dereferenced-table path)
                                                         (copy-seq pointer-value))
                                                   (cons pointer used-pointer)
                                                   path))))))
                          (map-ht entry used-pointer path))))
                   (vector
                    (map-vector entry used-pointer path))))))
      (funcall (function %dereference)))
    dereferenced-table))

(defmethod dereference ((uri uri) &key)
  (dereference (parse (ensure-json (dex:get (render-uri uri)))) :path-uri uri))

(defmethod dereference ((path pathname) &key)
  (dereference (parse (ensure-json (str:from-file path))) :pathname path))

(defgeneric parse-openapi (name &key url source-directory collection-id content dereference converter-url)
  (:documentation "Parse json/yaml from a file or uri into openapi class instance
You should mostly submit a file-name, and either ")
  (:method ((name string) &key url source-directory collection-id content (dereference *dereference*) (converter-url *converter-url*))
    (let* ((file-pathname
             (make-pathname :directory (trim (directory-namestring constant-data-directory)
                                             :char-bag "/")
                            :name name :type "json"))
           (result
             (cond (source-directory
                    (parse-directory source-directory file-pathname
                                     :converter-url converter-url
                                     :dereference dereference))
                   (url
                    (parse-url name url
                               :converter-url converter-url
                               :dereference dereference))
                   (collection-id
                    (parse-apis-guru-id name collection-id))
                   (content
                    (parse-string name content :converter-url converter-url))
                   (t
                    (cond ((file-exists-p (get-data-file name))
                           (let* ((content
                                    (from-file (get-data-file name)))
                                  (openapi-version
                                    (get-openapi-version content)))
                             (case openapi-version
                               (openapi-2.0 (convert-to-openapi-3 :content content
                                                                  :converter-url converter-url))
                               ((openapi-3.0 openapi-3.1) content)
                               (otherwise (error "The Version ~W is not supported"
                                                 openapi-version)))))
                          ((uiop:file-exists-p (get-data-file name :type "yaml"))
                           (to-file (get-data-file name)
                                    (convert-to-openapi-3 :pathname (get-data-file name :type "yaml")
                                                          :content-type "yaml"
                                                          :converter-url converter-url)))
                          ((uiop:file-exists-p (get-data-file name :type "yml"))
                           (to-file (get-data-file name)
                                    (convert-to-openapi-3 :pathname (get-data-file name :type "yml")
                                                          :content-type "yaml"
                                                          :converter-url converter-url)))
                          (t
                           (error (concat "There is no " name " json/yaml in the openapi-generator/data folder
Alternativeyl you can supply one of the keyword parameters (source-directory, apis-guru-id, file-content, url)"))))))))
      (json-to-clos result (get-openapi-version result)))))
