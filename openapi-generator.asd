(asdf:defsystem :openapi-generator
  :author "Kilian M. Haemmerle"
  :mailto "kilian.haemmerle@protonmail.com"
  :version (:read-file-form "version.sexp")
  :description "Parse OpenAPI into CLOS object for client generation"
  :license "AGPLv3-later"
  :depends-on (#:str #:cl-hash-util #:cl-semver #:pathname-utils
               #:json-mop #:yason #:com.inuoe.jzon #:cl-project
               #:listopia #:alexandria #:serapeum #:quri #:dexador
               #:cl-json-pointer)
  :pathname "code/"
  :serial t
  :components ((:file "package")
               (:file "json-mop")
               (:file "globals")
               (:file "util")
               (:file "classes")
               (:file "collections")
               (:file "convert")
               (:file "parser")
               (:file "function-generation")
               (:file "openapi-generator"))
  :in-order-to ((test-op (test-op "openapi-generator/test"))))

(asdf:defsystem "openapi-generator/test"
  :description "Test suite for the openapi-generator library"
  :depends-on (openapi-generator str fiveam)
  :pathname "test/"
  :serial t
  :components ((:file "test"))
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call '#:fiveam '#:run!
                                           (uiop:find-symbol* '#:test-suite
                                                              '#:openapi-generator/test))))
