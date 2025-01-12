(defpackage ningle-tutorial-project/forms
  (:use :cl)
  (:export #:register
           #:email
           #:password
           #:password-verify))

(in-package ningle-tutorial-project/forms)

(cl-forms:defform register (:action "/register" :id "signup" :csrf-protection t :csrf-field-name "csrftoken")
  ((email :email :value "")
   (password :password :value "")
   (password-verify :password :value "")
   (submit :submit :label "Register")))
