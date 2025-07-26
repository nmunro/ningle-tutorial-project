(defpackage :ningle-tutorial-project/middleware
  (:use :cl :sxql :ningle-tutorial-project/db)
  (:export #:auth-middleware
           #:auth-mount
           #:refresh-roles))

(in-package :ningle-tutorial-project/middleware)

(defun auth-middleware (app &key (mount-path "/auth"))
  (ningle-auth:set-config `(:login-redirect ,(getf (envy:config :ningle-tutorial-project/config) :login-redirect)
                            :mount-path ,mount-path
                            :mito ,(getf (envy:config :ningle-tutorial-project/config) :mito)))
  app)

(defun auth-mount (app &key (mount-path "/auth"))
  `(:mount ,mount-path ,(auth-middleware app :mount-path mount-path)))

(defun refresh-roles (app)
  (lambda (env)
    (let ((res (funcall app env)))
      (with-db-connection
        (handler-case
            (let ((session (getf env :lack.session)))
              (when (and session (hash-table-p session) (> (hash-table-count session) 0))
                (let ((user (gethash :user session)))
                  (when (typep user 'ningle-auth/models:user)
                    (format t "[refreshing-roles]~%")
                    (let ((roles (mito:select-dao 'ningle-auth/models:permission (where (:= :user user)))))
                      (setf (gethash :roles session) roles)
                      (format t "[refreshed-roles for ~A] result: ~A~%" user roles))))))
          (error (e)
            (format *error-output* "Error refreshing roles: ~A~%" e))))
      res)))
