(defpackage ningle-tutorial-project/forms
  (:use :cl)
  (:export #:register
           #:email
           #:username
           #:first-name
           #:last-name
           #:password
           #:password-verify
           #:*password-validator*))

(in-package ningle-tutorial-project/forms)

(defparameter *password-validator* (list (clavier:not-blank)
                                         (clavier:is-a-string)
                                         (clavier:len :min 8)))

(cl-forms:defform register (:action "/register" :id "signup" :csrf-protection t :csrf-field-name "csrftoken")
  ((email :email :value "")
   (username :string :constraints (list (clavier:not-blank) (clavier:is-a-string)))
   (first-name :string :constraints (list (clavier:not-blank) (clavier:is-a-string)))
   (last-name :string :constraints (list (clavier:not-blank) (clavier:is-a-string)))
   (password :password :value "" :constraints *password-validator*)
   (password-verify :password :value "" :constraints *password-validator*)
   (submit :submit :label "Register")))

