(defpackage :ningle-tutorial-project/middleware
  (:use :cl :sxql :ningle-tutorial-project/contrib)
  (:export #:refresh-roles))

(in-package :ningle-tutorial-project/middleware)

(defun refresh-roles (app)
  (lambda (env)
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
    (funcall app env)))
