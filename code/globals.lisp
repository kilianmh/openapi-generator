(cl:in-package #:openapi-generator)

(define-constant constant-data-directory
    (system-relative-pathname "openapi-generator" "data/")
  :test (function equal))

(define-constant constant-projects-directory
    (system-relative-pathname "openapi-generator" "projects/")
  :test (function equal)
  :documentation "Projects directory within libraries data folder")

(defvar *dereference* t
  "This variable influences whether the openapi document will be derefenced or not.")

(defparameter *converter-url* "https://converter.swagger.io"
  "Default swagger converter url.")
