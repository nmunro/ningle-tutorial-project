(defpackage ningle-tutorial-project/forms
  (:use :cl)
  (:export #:register
           #:email
           #:password
           #:password-verify
           #:*password-validator*))

(in-package ningle-tutorial-project/forms)

(defparameter *password-validator* (list (clavier:not-blank)
                                         (clavier:is-a-string)
                                         (clavier:len :min 8)))

(cl-forms:defform register (:action "/register" :id "signup" :csrf-protection t :csrf-field-name "csrftoken")
  ((email :email :value "")
   (password :password :value "" :constraints *password-validator*)
   (password-verify :password :value "" :constraints *password-validator*)
   (submit :submit :label "Register")))

