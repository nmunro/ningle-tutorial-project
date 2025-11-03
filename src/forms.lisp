(defpackage ningle-tutorial-project/forms
  (:use :cl :cl-forms)
  (:export #:post
           #:content
           #:submit
           #:comment
           #:parent))

(in-package ningle-tutorial-project/forms)

(defparameter *post-validator* (list (clavier:not-blank)
                                     (clavier:is-a-string)
                                     (clavier:len :max 140)))

(defparameter *post-parent-validator* (list (clavier:not-blank)
                                            (clavier:fn (lambda (x) (> (parse-integer x) 0)) "Checks positive integer")))

(defform post (:id "post" :csrf-protection t :csrf-field-name "csrftoken" :action "/post")
  ((content  :string   :value "" :constraints *post-validator*)
   (submit   :submit   :label "Post")))

(defform comment (:id "post" :csrf-protection t :csrf-field-name "csrftoken" :action "/post/comment")
  ((content  :string   :value "" :constraints *post-validator*)
   (parent   :hidden   :value 0  :constraints *post-parent-validator*)
   (submit   :submit   :label "Post")))
