(defpackage :ningle-tutorial-project/middleware
  (:use :cl)
  (:export :auth-middleware
           :auth-mount))

(in-package :ningle-tutorial-project/middleware)

(defun auth-middleware (app &key (mount-path "/auth"))
  (ningle-auth:set-config `(:login-redirect ,(getf (envy:config :ningle-tutorial-project/config) :login-redirect) :mount-path ,mount-path))
  (ningle-auth:set-config `(:mito ,(getf (envy:config :ningle-tutorial-project/config) :mito)))
  app)

(defun auth-mount (app &key (mount-path "/auth"))
  `(:mount ,mount-path ,(auth-middleware app :mount-path mount-path)))
