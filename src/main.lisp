(defpackage ningle-tutorial-project
  (:use :cl :lack/middleware/postmodern)
  (:import-from
   :ningle-tutorial-project/forms
   #:register
   #:email
   #:username
   #:first-name
   #:last-name
   #:password
   #:password-verify)
  (:export #:start
           #:stop
           #:migrate))

(in-package ningle-tutorial-project)

;; setup
(defvar *app* (make-instance 'ningle:app))
(dotenv:load-env (asdf:system-relative-pathname :ningle-tutorial-project ".env"))
(djula:add-template-directory (asdf:system-relative-pathname :ningle-tutorial-project "src/templates/"))
(djula:set-static-url "/public/")

(setf (ningle:route *app* "/")
      (lambda (params)
        (let ((user  (list :username "NMunro"))
              (posts (list (list :author (list :username "Bob")  :content "Experimenting with Dylan" :created-at "2025-01-24 @ 13:34")
                           (list :author (list :username "Jane") :content "Wrote in my diary today"  :created-at "2025-01-24 @ 13:23"))))
          (djula:render-template* "index.html" nil :title "Home" :user user :posts posts))))

(setf (ningle:route *app* "/people")
    (lambda (params)
        (with-postmodern (:mydb)
          (djula:render-template* "people.html" nil :title "People" :users (ningle-tutorial-project/user:users)))))

(setf (ningle:route *app* "/register" :method '(:GET :POST))
    (lambda (params)
        (let ((form (cl-forms:find-form 'register)))
          (if (string= "GET" (lack.request:request-method ningle:*request*))
            (djula:render-template* "register.html" nil :form form)
            (handler-case
                (progn
                    (cl-forms:handle-request form) ; Can throw an error if CSRF fails
                    (multiple-value-bind (valid errors)
                        (cl-forms:validate-form form)
                      (when errors
                        (format t "Errors: ~A~%" errors))
                      (when valid
                        ;; @TODO: Throw error if passwords dont match
                        (cl-forms:with-form-field-values (email first-name last-name username password password-verify) form
                          (format t "~A ~A: email - ~A, password - ~A, password-verify - ~A~%" first-name last-name email password password-verify)
                          (with-postmodern (:mydb)
                            (ningle-tutorial-project/user:create first-name last-name email username password))))
                    (djula:render-template* "register.html" nil :form form)))

                (simple-error (csrf-error)
                    (setf (lack.response:response-status ningle:*response*) 403)
                    (djula:render-template* "error.html" nil :error csrf-error)))))))

(defmethod ningle:not-found ((app ningle:<app>))
    (declare (ignore app))
    (setf (lack.response:response-status ningle:*response*) 404)
    (djula:render-template* "error.html" nil :error "Not Found"))

(defun start (&key (server :woo) (address "127.0.0.1") (port 8000))
    (clack:clackup
      (lack.builder:builder :session
                            (:postmodern :pools `((:pool-id :mydb
                             :database ,(uiop:getenv "POSTGRES_DB_NAME")
                             :username ,(uiop:getenv "POSTGRES_DB_USER")
                             :password ,(uiop:getenv "POSTGRES_DB_PASSWORD")
                             :host ,(uiop:getenv "POSTGRES_DB_HOST")
                             :use-binary t
                             :application-name "ningle-tutorial-project"
                             :max-open-count 12
                             :max-idle-count 3
                             :timeout 5000
                             :idle-timeout 40000)))
                            (:static
                             :root (asdf:system-relative-pathname :ningle-tutorial-project "src/static/")
                             :path "/public/")
                            *app*)
     :server server
     :address address
     :port port))

(defun stop (instance)
    (clack:stop instance))
