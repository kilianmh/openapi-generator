(defpackage #:openapi-generator/test
  (:use #:cl
        #:openapi-generator
        #:fiveam)
  (:shadowing-import-from #:str
                          #:concat
                          #:downcase)
  (:import-from #:openapi-generator
                #:parse-openapi)
  (:shadow #:type)
  (:export #:run!
           #:test-suite))

(in-package #:openapi-generator/test)

(def-suite test-suite :description "Main Test Suite for Openapi-generator")

(def-test parsing (:suite test-suite)
  (finishes (openapi-generator:parse-openapi "openai"
             :url "https://raw.githubusercontent.com/hirosystems/stacks-blockchain-api/gh-pages/openapi.resolved.yaml")))

