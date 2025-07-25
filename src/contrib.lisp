(defpackage ningle-tutorial-project/contrib
  (:use :cl :mito :ningle-tutorial-project/db)
  (:export #:create-super-user))

(in-package :ningle-tutorial-project/contrib)

(defun create-super-user (&key username email password)
  (with-db-connection
      (let ((user (create-dao 'ningle-auth/models:user :username username :email email :password password :active 1)))
        (create-dao 'ningle-auth/models:permission :user user :role (find-dao 'ningle-auth/models:role :name "admin"))
        user)))
