(defpackage ningle-tutorial-project
  (:use :cl)
  (:export #:start
           #:stop))

(in-package ningle-tutorial-project)

(defvar *app* (make-instance 'ningle:app))

(setf (ningle:route *app* "/")
      (lambda (params)
        (let ((user  (list :username "NMunro"))
              (posts (list (list :author (list :username "Bob")  :content "Experimenting with Dylan" :created-at "2025-01-24 @ 13:34")
                           (list :author (list :username "Jane") :content "Wrote in my diary today"  :created-at "2025-01-24 @ 13:23"))))
          (djula:render-template* "main/index.html" nil :title "Home" :user user :posts posts))))

(setf (ningle:route *app* "/people")
      (lambda (params)
        (let ((users (mito:retrieve-dao 'ningle-auth/models:user)))
          (djula:render-template* "main/people.html" nil :title "People" :users users))))

(setf (ningle:route *app* "/people/:person")
      (lambda (params)
        (let* ((person (ingle:get-param :person params))
               (user (first (mito:select-dao
                              'ningle-auth/models:user
                              (where (:or (:= :username person)
                                          (:= :email person)))))))
          (djula:render-template* "main/person.html" nil :title "Person" :user user))))

(defmethod ningle:not-found ((app ningle:<app>))
    (declare (ignore app))
    (setf (lack.response:response-status ningle:*response*) 404)
    (djula:render-template* "error.html" nil :title "Error" :error "Not Found"))

(defun start (&key (server :woo) (address "127.0.0.1") (port 8000))
    (djula:add-template-directory (asdf:system-relative-pathname :ningle-tutorial-project "src/templates/"))
    (djula:set-static-url "/public/")
    (clack:clackup
     (lack.builder:builder (envy-ningle:build-middleware :ningle-tutorial-project/config *app*))
     :server server
     :address address
     :port port))

(defun stop (instance)
    (clack:stop instance))
