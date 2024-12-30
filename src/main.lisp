(defpackage ningle-tutorial-project
  (:use :cl)
  (:export #:start
           #:stop))

(in-package ningle-tutorial-project)

(defvar *app* (make-instance 'ningle:app))

(setf (ningle:route *app* "/")
      (lambda (params)
        (let ((user  (list :username "NMunro"))
              (posts (list (list :author (list :username "Bob")  :content "Experimenting with Dylan")
                           (list :author (list :username "Jane") :content "Wrote in my diary today"))))
          (djula:render-template* "index.html" nil :title "Home"
                                                   :user user
                                                   :posts posts))))

(defmethod ningle:not-found ((app ningle:<app>))
    (declare (ignore app))
    (setf (lack.response:response-status ningle:*response*) 404)
    "Not Found")

(defun start (&key (server :woo) (address "127.0.0.1") (port 8000))
    (djula:add-template-directory (asdf:system-relative-pathname :ningle-tutorial-project "src/templates/"))
    (clack:clackup
     *app*
     :server server
     :address address
     :port port))

(defun stop (instance)
    (clack:stop instance))
