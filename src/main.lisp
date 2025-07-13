(defpackage ningle-tutorial-project
  (:use :cl :sxql)
  (:export #:start
           #:stop))

(in-package ningle-tutorial-project)

(defvar *app* (make-instance 'ningle:app))

(setf (ningle:route *app* "/")
      (lambda (params)
        (let ((user  (gethash :user ningle:*session*))
              (posts (list (list :author (list :username "Bob")  :content "Experimenting with Dylan" :created-at "2025-01-24 @ 13:34")
                           (list :author (list :username "Jane") :content "Wrote in my diary today"  :created-at "2025-01-24 @ 13:23"))))
          (djula:render-template* "main/index.html" nil :title "Home" :user user :posts posts))))

(setf (ningle:route *app* "/profile")
      (lambda (params)
        (let ((user (gethash :user ningle:*session*)))
            (if user
                (djula:render-template* "main/profile.html" nil :title "Profile" :user user)
                (progn
                    (setf (lack.response:response-status ningle:*response*) 403)
                    (djula:render-template* "error.html" nil :title "Error" :error "Unauthorized"))))))

(setf (ningle:route *app* "/people")
      (lambda (params)
        (let ((users (mito:retrieve-dao 'ningle-auth/models:user)))
          (djula:render-template* "main/people.html" nil :title "People" :users users :user (cu-sith:logged-in-p)))))

(setf (ningle:route *app* "/people/:person")
      (lambda (params)
        (let* ((username-or-email (ingle:get-param :person params))
               (person (first (mito:select-dao
                              'ningle-auth/models:user
                              (where (:or (:= :username username-or-email)
                                          (:= :email username-or-email)))))))
          (djula:render-template* "main/person.html" nil :title "Person" :person person :user (cu-sith:logged-in-p)))))

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
