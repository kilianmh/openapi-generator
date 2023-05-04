(in-package #:openapi-generator)

(defgeneric collect-path-types (path)
  (:documentation "Collect all bound operation types as symbols")
  (:method ((path path))
    (let ((types
            nil))
      (mapc (function (lambda (operation-type)
              (when (slot-boundp path operation-type)
                (push operation-type types))))
            (quote (get head options trace delete put post patch)))
      types)))

(defgeneric collect-function-names (api &key param-case)
  (:documentation "Generate all function names")
  (:method ((api openapi) &key param-case)
    (flet ((path-function-names (api path &key param-case)
             (mapcar (function (lambda (operator)
                       (function-name path operator :param-case param-case)))
	             (collect-path-types (gethash path (paths api))))))
      (let ((names nil))
        (maphash (function (lambda (path value)
		   (declare (ignore value))
		   (push (path-function-names api path :param-case param-case) names)))
	         (paths api))
        (listopia:concat names)))))

(defgeneric collect-alias-exports (api slot)
  (:documentation "Create list of upcased string to include in export clause of defpackage.")
  (:method ((api openapi) (slot string))
    (funcall (function collect-alias-exports) api (intern (upcase slot) "OPENAPI-GENERATOR")))
  (:method ((api openapi) (slot symbol))
    (let ((result-list
            ()))
      (maphash (function (lambda (path path-object)
                    (declare (ignore path))
                    (mapc (function (lambda (operator)
                            (multiple-value-bind (value exist-p)
                                (slot-value-safe (slot-value path-object operator)
                                                 slot)
                              (unless exist-p
                                (return-from collect-alias-exports))
                              (push (upcase (param-case value))
                                    result-list))))
                          (collect-path-types path-object))))
                  (paths api))
      result-list)))

(defgeneric generate-defpackage (name api &key alias)
  (:documentation "Generate defpackage code including alias functions")
  (:method (name (api openapi) &key alias)
    `(uiop:define-package ,(intern (upcase name))
         (:use)
       (:import-from #:cl
                     #:append #:declaim #:declare #:ignorable #:ignore #:optimize #:speed #:space #:safety #:debug #:compilation-speed #:defparameter #:null
                     #:case #:array #:write-to-string #:setf #:defparameter #:defun #:fdefinition #:t #:nil #:&key #:when #:let* #:or #:if #:cdr #:assoc #:quote #:string-equal #:cons #:list #:cond #:find #:warn #:boundp #:symbol-value #:string #:integer #:number #:boolean)
       (:import-from #:quri #:uri #:make-uri #:render-uri #:uri-scheme #:uri-host)
       (:import-from #:str #:concat)
       (:import-from #:com.inuoe.jzon #:parse)
       (:import-from #:openapi-generator #:remove-empty-values)
       (:import-from #:dexador #:request)
       (:import-from #:serapeum #:assuref)
       (:export #:*authorization* #:*headers* #:*cookie* #:*output* #:*server*
                ,@(append (when (member :path alias)
                            (collect-function-names api :param-case nil))
                          (when (member :summary alias)
                            (collect-alias-exports api "summary"))
                          (when (member :description alias)
                            (collect-alias-exports api "description"))
                          (when (member :operation-id alias)
                            (collect-alias-exports api "operation-id")))))))

(defgeneric generate-function-code (api name)
  (:documentation "Generate all function code as list")
  (:method ((api openapi) name)
    (flet ((path-function-code (api path)
             "Create functions for each path operator"
             (mapcar (function (lambda (operator)
	               (generate-function api path operator)))
	             (collect-path-types (gethash path (paths api))))))
      (let ((result-list
              ()))
        (maphash (function (lambda (path value)
                   (declare (ignore value))
                   (let ((concatenated-list
                           (append (path-function-code api path) result-list)))
                     (setf result-list concatenated-list))))
                 (paths api))
        result-list))))

(defgeneric generate-slot-alias (api slot)
  (:documentation "Create list of setf with slot as alias")
  (:method ((api openapi) (slot string))
    (funcall (function generate-slot-alias) api (intern (upcase slot) "OPENAPI-GENERATOR")))
  (:method ((api openapi) (slot symbol))
    (let ((result-list
            ()))
      (maphash (function (lambda (path path-object)
                 (mapc (function (lambda (operator)
                         (multiple-value-bind (value exist-p)
                             (slot-value-safe (slot-value path-object operator)
                                              slot)
                           (unless exist-p
                             (return-from generate-slot-alias))
                           (push `(fdefinition (quote ,(intern (function-name path operator :param-case nil))))
                                 result-list)
                           (push `(fdefinition (quote ,(intern (upcase (param-case value)))))
                                 result-list))))
                       (collect-path-types path-object))))
               (paths api))
      result-list)))

(defgeneric generate-parameters (&key query headers authorization cookie output server)
  (:documentation "Creates code to be included in main.lisp for parameters")
  (:method (&key query headers authorization cookie output server)
    (cl:format nil "~{~S~%~}"
               (list `(defparameter ,(intern "*OUTPUT*") ,(if output output :hash-table))
                     `(defparameter ,(intern "*AUTHORIZATION*") ,authorization)
                     `(defparameter ,(intern "*SERVER*") ,server)
                     `(defparameter ,(intern "*COOKIE*") ,cookie)
                     `(defparameter ,(intern "*HEADERS*") ',headers)
                     `(defparameter ,(intern "*QUERY*") ',query)))))

(defgeneric check-api-slots (api list)
  (:documentation "Make sure that the function (alias) can be generated.
Prefered alias source is operation-id. Last resort option is path.")
  (:method ((api openapi) (list list))
    (flet ((check-path-slot (api slot)
             (maphash (function (lambda (path path-object)
                        (declare (ignore path))
                        (mapc (function (lambda (operator)
                                (multiple-value-bind (value exist-p)
                                    (slot-value-safe (slot-value path-object operator)
                                                     (intern (upcase slot) "OPENAPI-GENERATOR"))
                                  (declare (ignore value))
                                  (unless exist-p
                                    (return-from check-path-slot)))))
                              (collect-path-types path-object))))
                      (paths api))
             t))
      (let ((result-list
              nil))
        (when (and (member :operation-id list)
                   (check-path-slot api "operation-id"))
          (push :operation-id result-list))
        (when (and (member :summary list)
                   (check-path-slot api "summary"))
          (push :summary result-list))
        (when (and (member :description list)
                   (check-path-slot api "description"))
          (push :description result-list))
        (when (or (member :path list)
                  (length= 0 result-list))
          (push :path result-list))
        result-list))))

(defgeneric generate-code (api name &key output headers authorization server cookie alias)
  (:documentation "Generate all code to be included in the main.lisp file. Includes defpackage + functions + setf alias")
  (:method (api name &key (output :hash-table) headers authorization server cookie alias)
    (let ((alias-list
            (check-api-slots api alias)))
      (concat
       (cl:format nil "~S" (generate-defpackage name api :alias alias-list))
       (string #\Newline)(string #\Newline)
       (cl:format nil "(in-package :~A)" (downcase (symbol-name name)))
       (string #\Newline)(string #\Newline)
       (string #\Newline)(string #\Newline)
       (generate-parameters :headers headers :authorization authorization :cookie cookie
                            :output output :server server)
       (string #\Newline)(string #\Newline)
       (cl:format nil "~{~W~% ~}" (generate-function-code api name))
       (when (member :operation-id alias-list)
         (let ((operation-id-alias
                 (generate-slot-alias api "operation-id")))
           (when operation-id-alias
             (cl:format nil "~S" `(setf ,@operation-id-alias)))))
       (when (member :summary alias-list)
         (let ((summary-alias
                 (generate-slot-alias api "summary")))
           (when summary-alias
             (cl:format nil "~S" `(setf ,@summary-alias)))))
       (when (member :description alias-list)
         (let ((description-alias
                 (generate-slot-alias api "description")))
           (when description-alias
             (cl:format nil "~S" `(setf ,@description-alias)))))))))

(defgeneric ensure-project-directory (directory)
  (:documentation "Makes sure that the directory is existing before the template is generated.")
  (:method (directory)
    (typecase directory
      (pathname
       (ensure-directories-exist directory))
      (keyword
       (case directory
         (:temporary
          (ensure-directories-exist
           (make-pathname :directory
                          (trim (concat (namestring (default-temporary-directory))
                                        "openapi-generator/")
                                :char-bag "/"))))
         (:library
          (ensure-directories-exist constant-projects-directory))))
      (null
       (funcall (function ensure-project-directory) :temporary)))))

(defun make-openapi-client (system-name
                            &key
                              server (output :hash-table) headers authorization cookie
                              (alias (list :operation-id)) (system-directory :library) (load-system t)
                              openapi (api-name system-name) url source-directory collection-id content
                              (dereference *dereference*))
  "Creates Openapi client by combining a project template with generated code.
Source options are url, source-directory, collection-id, or openapi (openapi class instance).
The options server, output, headers, authorization, cookie, content are stored in the library code
as dynamic parameters.."
  (let* ((project-pathname
           (make-pathname :directory (concat (trim-left
                                              (directory-namestring (ensure-project-directory system-directory))
                                              :char-bag "/")
                                             system-name)))
         (pathname-main
           (make-pathname :directory (concat (trim (directory-namestring project-pathname)
                                                   :char-bag "/")
                                             "/src")
                          :name "main" :type "lisp"))
         (pathname-asd
           (make-pathname :directory (trim (directory-namestring project-pathname)
                                           :char-bag "/")
                          :name system-name :type "asd")))
    (make-project
     project-pathname
     :name system-name
     :depends-on (quote (quri str com.inuoe.jzon dexador uiop openapi-generator))
     :without-tests t)
    (with-open-file (system pathname-main
                            :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-sequence
       (generate-code
        (if openapi
            openapi
            (parse-openapi api-name
                           :url url
                           :source-directory source-directory
                           :collection-id collection-id
                           :dereference dereference
                           :content content))
        (intern (upcase system-name))
        :headers headers :authorization authorization :cookie cookie
        :output output :alias alias :server server)
       system))
    (load-asd pathname-asd :name system-name)
    (when load-system
      (load-system (intern system-name) :force t))))
