(cl:in-package :openapi-generator)

(defun convert-by-url (url)
  (dex:get
   (render-uri (make-uri :scheme "https" :host "converter.swagger.io"
                         :path "api/convert" :query (list (cons "url" url))))))

(defun convert-by-content (&key content (content-type "application/json"))
  (dex:post
   (render-uri (make-uri :scheme "https" :host "converter.swagger.io" :path "api/convert"))
   :content content
   :headers (remove-empty-values
             (list
              (cons "Content-Type"
                    (cond
                      (content-type
                       (if (find content-type
                                 (list "application/json" "application/yaml")
                                 :test (function string-equal))
                           content-type
                           (warn
                            "The body type ~A is not mentioned. Valid content-types are: ~A"
                            content-type "application/json application/yaml")))
                      (t "application/json")))))))

(defun convert-to-openapi-3 (&key url content pathname (content-type "json"))
  (when (and url content pathname)
    (error "You can only supply either url or content, not both simultaneously."))
  (let ((content-type
          (string-case content-type
                       ("json" "application/json")
                       ("yaml" "application/yaml"))))
    (cond (url
           (convert-by-url url))
          (content
           (convert-by-content :content content :content-type content-type))
          (pathname
           (convert-by-content :content (from-file pathname) :content-type content-type))
          (t
           (error "You have to supply either url, content, or pathname.")))))
