(uiop:define-package openapi-generator
    (:use #:cl)
  (:shadow #:get #:type #:continue #:delete #:trace #:format #:search #:not)
  (:shadowing-import-from #:closer-mop
                          #:defgeneric #:slot-definition-name)
  (:import-from #:uiop
                #:default-temporary-directory #:delete-file-if-exists
                #:read-file-string #:file-exists-p #:ensure-pathname)
  (:import-from #:asdf
                #:system-relative-pathname #:load-asd #:load-system)
  (:import-from #:cl-project
                #:make-project)
  (:import-from #:alexandria
                #:length= #:define-constant)
  (:import-from #:json-mop
                #:json-serializable-class #:json-to-clos)
  (:import-from #:yason
                #:true)
  (:import-from #:str
                #:param-case #:downcase #:upcase #:concat
                #:emptyp #:containsp #:ends-with-p #:starts-with-p
                #:split #:substring #:replace-first #:replace-using #:unwords
                #:from-file #:to-file #:trim #:trim-left
                #:string-case)
  (:import-from #:com.inuoe.jzon
                #:parse)
  (:import-from #:listopia
                #:intersperse)
  (:import-from #:quri
                #:uri #:uri-scheme #:uri-host #:uri-path
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
  (:export #:parse-openapi
           #:make-openapi-client
           #:convert-to-openapi-3
           #:remove-empty-values))
