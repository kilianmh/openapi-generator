(uiop:define-package openapi-generator
    (:use #:cl)
  (:shadowing-import-from #:closer-mop
                          #:defclass #:defgeneric #:slot-definition-name)
  (:import-from #:uiop
                #:default-temporary-directory #:delete-file-if-exists
                #:read-file-string #:file-exists-p #:ensure-pathname)
  (:import-from #:asdf
                #:system-relative-pathname #:load-system)
  (:import-from #:cl-project
                #:make-project)
  (:import-from #:alexandria
                #:length= #:define-constant #:string-designator #:when-let #:if-let)
  (:import-from #:json-mop
                #:json-serializable-class #:json-to-clos)
  (:import-from #:yason
                #:true)
  (:import-from #:str
                #:param-case #:downcase #:upcase #:concat #:param-case
                #:emptyp #:containsp #:ends-with-p #:starts-with-p
                #:split #:substring #:replace-first #:replace-using #:unwords
                #:trim #:trim-left #:string-case)
  (:import-from #:com.inuoe.jzon
                #:parse
                #:stringify)
  (:import-from #:listopia
                #:intersperse)
  (:import-from #:quri
                #:uri #:uri-scheme #:uri-host #:uri-port #:uri-path
                #:make-uri #:render-uri #:merge-uris)
  (:import-from #:dexador
                #:request)
  (:import-from #:pathname-utils
                #:file-type)
  (:import-from #:serapeum
                #:case-using #:ecase-using #:slot-value-safe #:dict #:assuref)
  (:import-from #:cl-hash-util
                #:hash-keys)
  (:import-from #:semver
                #:read-version-from-string
                #:version= #:version>= #:version<=)
  (:import-from #:cl-json-pointer
                #:get-by-json-pointer)
  (:import-from #:cl-hash-util
                #:hash-get)
  (:import-from #:parse-float
                #:parse-float)
  (:export #:parse-openapi
           #:generate-client
           #:make-openapi-client
           #:*dereference*
           #:dereference
           #:*converter-url*
           #:convert-to-openapi-3))
