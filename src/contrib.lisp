(defpackage ningle-tutorial-project/contrib
  (:use :cl :mito)
  (:export #:create-super-user
           #:with-db-connection))

(in-package :ningle-tutorial-project/contrib)

(defmacro with-db-connection (&body body)
    `(multiple-value-bind (backend args) (envy-ningle:extract-middleware-config :ningle-tutorial-project/config :mito)
        (unless backend
            (error "No MITO backend found for config ~A" cfg))

        (unwind-protect
             (progn
               (apply #'mito:connect-toplevel backend args)
               ,@body
               (mito:disconnect-toplevel)))))

(defun create-super-user (&key username email password)
  (with-db-connection
      (let ((user (create-dao 'ningle-auth/models:user :username username :email email :password password :active 1)))
        (create-dao 'ningle-auth/models:permission :user user :role (find-dao 'ningle-auth/models:role :name "admin"))
        user)))
