(defpackage ningle-tutorial-project/forms
  (:use :cl :cl-forms)
  (:export #:post
           #:content
           #:submit))

(in-package ningle-tutorial-project/forms)

(defparameter *post-validator* (list (clavier:not-blank)
                                     (clavier:is-a-string)
                                     (clavier:len :max 140)))

(defform post (:id "post" :csrf-protection t :csrf-field-name "csrftoken" :action "/post")
  ((content  :string   :value "" :constraints *post-validator*)
   (submit   :submit   :label "Post")))
