(defpackage ningle-tutorial-project/models
  (:use :cl :mito)
  (:export #:user))

(in-package ningle-tutorial-project/models)

(deftable user (mito-auth:has-secure-password)
  ((email    :col-type (:varchar 255) :initarg :email    :accessor email)
   (username :col-type (:varchar 255) :initarg :username :accessor username))
  (:unique-keys email username))
