(defpackage ningle-tutorial-project
  (:use :cl :sxql)
  (:import-from
   :ningle-tutorial-project/forms
   #:email
   #:username
   #:password
   #:password-verify
   #:register)
  (:export #:start
           #:stop))

(in-package ningle-tutorial-project)

(defvar *app* (make-instance 'ningle:app))

(dotenv:load-env (asdf:system-relative-pathname :ningle-tutorial-project ".env"))

(setf (ningle:route *app* "/")
      (lambda (params)
        (let ((user  (list :username "NMunro"))
              (posts (list (list :author (list :username "Bob")  :content "Experimenting with Dylan" :created-at "2025-01-24 @ 13:34")
                           (list :author (list :username "Jane") :content "Wrote in my diary today"  :created-at "2025-01-24 @ 13:23"))))
          (djula:render-template* "index.html" nil :title "Home" :user user :posts posts))))

(setf (ningle:route *app* "/people")
      (lambda (params)
        (let ((users (mito:retrieve-dao 'ningle-tutorial-project/models:user)))
          (djula:render-template* "people.html" nil :title "People" :users users))))

(setf (ningle:route *app* "/people/:person")
      (lambda (params)
        (let* ((person (ingle:get-param :person params))
               (user (first (mito:select-dao
                              'ningle-tutorial-project/models:user
                              (sxql:where (:or (:= :username person)
                                               (:= :email person)))))))
          (djula:render-template* "person.html" nil :title "Person" :user user))))

(setf (ningle:route *app* "/register" :method '(:GET :POST))
    (lambda (params)
        (let ((form (cl-forms:find-form 'register)))
          (if (string= "GET" (lack.request:request-method ningle:*request*))
            (djula:render-template* "register.html" nil :title "Register" :form form)
            (handler-case
                (progn
                    (cl-forms:handle-request form) ; Can throw an error if CSRF fails
                    (multiple-value-bind (valid errors)
                        (cl-forms:validate-form form)

                      (when errors
                        (format t "Errors: ~A~%" errors))

                      (when valid
                        (cl-forms:with-form-field-values (email username password password-verify) form
                          (when (mito:select-dao 'ningle-tutorial-project/models:user
                                 (where (:or (:= :username username)
                                             (:= :email email))))
                            (error "Either username or email is already registered"))

                          (when (string/= password password-verify)
                            (error "Passwords do not match"))

                          (mito:create-dao 'ningle-tutorial-project/models:user
                                           :email email
                                           :username username
                                           :password password)
                          (ingle:redirect "/people")))))

                (error (err)
                    (djula:render-template* "error.html" nil :title "Error" :error err))

                (simple-error (csrf-error)
                    (setf (lack.response:response-status ningle:*response*) 403)
                    (djula:render-template* "error.html" nil :title "Error" :error csrf-error)))))))

(defmethod ningle:not-found ((app ningle:<app>))
    (declare (ignore app))
    (setf (lack.response:response-status ningle:*response*) 404)
    (djula:render-template* "error.html" nil :title "Error" :error "Not Found"))

(defun start (&key (server :woo) (address "127.0.0.1") (port 8000) (sql-type :sqlite3))
    (djula:add-template-directory (asdf:system-relative-pathname :ningle-tutorial-project "src/templates/"))
    (djula:set-static-url "/public/")
    (clack:clackup
      (lack.builder:builder
       :session
       `(:mito
          (:sqlite3
           :database-name ,(or (uiop:getenv "SQLITE_DB_NAME") (error "Environment variable SQLITE_DB_NAME is not set!"))))
       (:static
        :root (asdf:system-relative-pathname :ningle-tutorial-project "src/static/")
        :path "/public/")
       *app*)
     :server server
     :address address
     :port port))

(defun stop (instance)
    (clack:stop instance))
