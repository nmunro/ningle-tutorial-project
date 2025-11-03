(defpackage ningle-tutorial-project
  (:use :cl :ningle-tutorial-project/controllers)
  (:export #:start
           #:stop))

(in-package ningle-tutorial-project)

(defvar *app* (make-instance 'ningle:app))

;; requirements
(setf (ningle:requirement *app* :logged-in-p)
      (lambda (value)
        (and (cu-sith:logged-in-p) value)))

;; routes
(setf (ningle:route *app* "/") #'index)
(setf (ningle:route *app* "/post/:id/likes" :method :POST :logged-in-p t) #'post-likes)
(setf (ningle:route *app* "/post/:id") #'single-post)
(setf (ningle:route *app* "/post" :method :POST :logged-in-p t) #'post-content)
(setf (ningle:route *app* "/post/comment" :method :POST :logged-in-p t) #'post-comment)
(setf (ningle:route *app* "/profile" :logged-in-p t) #'logged-in-profile)
(setf (ningle:route *app* "/profile") #'unauthorized-profile)
(setf (ningle:route *app* "/people") #'people)
(setf (ningle:route *app* "/people/:person") #'person)

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
