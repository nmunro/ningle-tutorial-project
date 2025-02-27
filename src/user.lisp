(defpackage ningle-tutorial-project/user
  (:use :cl :uiop :ningle-tutorial-project/migrations)
  (:export #:create
           #:email
           #:first-name
           #:id
           #:last-name
           #:password
           #:update
           #:username
           #:user
           #:users))

(in-package ningle-tutorial-project/user)

(yesql:import migrations
    :from "src/sql/user/migrations.sql"
    :as :cl-yesql/postmodern
    :binding :all-functions)

(yesql:import user-queries
    :from "src/sql/user/queries.sql"
    :as :cl-yesql/postmodern
    :binding :all-functions)

(defclass user (model)
  ((email      :initarg :email      :initform ""  :accessor email)
   (first-name :initarg :first-name :initform ""  :accessor first-name)
   (last-name  :initarg :last-name  :initform ""  :accessor last-name)
   (password   :initarg :password   :initform ""  :accessor password)
   (username   :initarg :username   :initform ""  :accessor username)))

(defun users ()
  (mapcar
   (lambda (row)
     (destructuring-bind (id first-name last-name email username password) row
       (make-instance 'user :id id :first-name first-name :last-name last-name :email email :username username :password password)))
   (all-users)))

(defun create (email first-name last-name username password)
  (create-user first-name last-name email username password))

(defmethod update ((user user))
  (update-user (first-name user) (last-name user) (email user) (username user) (password user) (id user)))

(defun migrate-user ()
    (postmodern:with-connection `(,(getenv "POSTGRES_DB_NAME") ,(getenv "POSTGRES_DB_USER") ,(getenv "POSTGRES_DB_PASSWORD") ,(getenv "POSTGRES_DB_HOST"))
        (0001-users-initial)
        (0002-users-add-username)
        (0003-users-add-password)))

(register-model 'user 'migrate-user)
