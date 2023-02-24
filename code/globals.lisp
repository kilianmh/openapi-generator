(cl:in-package #:openapi-generator)

(define-constant constant-local-projects-directory
    (car *local-project-directories*)
  :test (function equal))

(define-constant constant-data-directory
    (system-relative-pathname "openapi-generator" "data/")
  :test (function equal))

(define-constant constant-projects-directory
    (system-relative-pathname "openapi-generator" "projects/")
  :test (function equal)
  :documentation "Projects directory within libraries data folder")
