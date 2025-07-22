(defpackage ningle-tutorial-project/contrib
  (:use :cl :mito :ningle-tutorial-project/db)
  (:export #:create-super-user))

(in-package :ningle-tutorial-project/contrib)

(defun create-super-user (&key username email password)
  (with-db-connection
      (let* ((u (create-dao 'ningle-auth/models:user :username username :email email :password password))
             (r (create-dao 'ningle-auth/models:role :name "admin" :description "Admin")))
        (create-dao 'ningle-auth/models:permission :user u :role r))))
