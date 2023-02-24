(cl:in-package #:openapi-generator)

(defconstant constant-local-projects-directory
  (car *local-project-directories*))

(defconstant constant-data-directory
   (system-relative-pathname "openapi-generator" "data/"))

(defconstant constant-projects-directory
  (system-relative-pathname "openapi-generator" "projects/")
  "Projects directory within libraries data folder")
