(uiop:define-package #:openapi-generator/test
  (:use #:cl)
  (:import-from #:fiveam
		#:def-suite
		#:def-test
		#:finishes
		#:is-true)
  (:shadowing-import-from #:str
                          #:concat
                          #:downcase)
  (:import-from #:openapi-generator
                #:parse-openapi
		#:make-openapi-client)
  (:shadow #:type)
  (:export #:run!
           #:test-suite))

(in-package #:openapi-generator/test)

(def-suite test-suite :description "Main Test Suite for Openapi-generator")

(def-test parsing (:suite test-suite)
  (finishes (parse-openapi "openai"
             :url "https://raw.githubusercontent.com/hirosystems/stacks-blockchain-api/gh-pages/openapi.resolved.yaml")))


(def-test make-openapi-client (:suite test-suite)
  (finishes (make-openapi-client "codeberg"
				 :url "https://codeberg.org/swagger.v1.json"
				 :server "https://codeberg.org/api/v1"
				 :parse t))
  (is-true (funcall (uiop:find-symbol* '#:repo-get '#:codeberg) "kilianmh" "openapi-generator")))
