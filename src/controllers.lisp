(defpackage ningle-tutorial-project/controllers
  (:use :cl :sxql :ningle-tutorial-project/forms)
  (:export #:logged-in-index
           #:index
           #:post-likes
           #:single-post
           #:post-content
           #:logged-in-profile
           #:unauthorized-profile
           #:people
           #:person))

(in-package ningle-tutorial-project/controllers)


(defun logged-in-index (params)
    (let* ((user (gethash :user ningle:*session*))
           (form (cl-forms:find-form 'post))
           (posts (ningle-tutorial-project/models:logged-in-posts user)))
        (djula:render-template* "main/index.html" nil :title "Home" :user user :posts posts :form form)))


(defun index (params)
    (let ((posts (ningle-tutorial-project/models:not-logged-in-posts)))
        (djula:render-template* "main/index.html" nil :title "Home" :user (gethash :user ningle:*session*) :posts posts)))


(defun post-likes (params)
    (let* ((user (gethash :user ningle:*session*))
           (post (mito:find-dao 'ningle-tutorial-project/models:post :id (parse-integer (ingle:get-param :id params))))
           (res (make-hash-table :test 'equal)))
        (setf (gethash :post res) (ingle:get-param :id params))
        (setf (gethash :likes res) (ningle-tutorial-project/models:likes post))
        (setf (gethash :liked res) (ningle-tutorial-project/models:toggle-like user post))
        (com.inuoe.jzon:stringify res)))


(defun single-post (params)
    (handler-case
        (let ((post (mito:find-dao 'ningle-tutorial-project/models:post :id (parse-integer (ingle:get-param :id params)))))
            (djula:render-template* "main/post.html" nil :title "Post" :post post))

        (parse-error (err)
            (setf (lack.response:response-status ningle:*response*) 404)
            (djula:render-template* "error.html" nil :title "Error" :error err))))


(defun post-content (params)
    (let ((user (gethash :user ningle:*session*))
          (form (cl-forms:find-form 'post)))
        (handler-case
            (progn
                (cl-forms:handle-request form) ; Can throw an error if CSRF fails

                (multiple-value-bind (valid errors)
                    (cl-forms:validate-form form)

                    (when errors
                        (format t "Errors: ~A~%" errors))

                    (when valid
                        (cl-forms:with-form-field-values (content) form
                            (mito:create-dao 'ningle-tutorial-project/models:post :content content :user user)
                            (ingle:redirect "/")))))

            (simple-error (err)
                (setf (lack.response:response-status ningle:*response*) 403)
                (djula:render-template* "error.html" nil :title "Error" :error err)))))


(defun logged-in-profile (params)
    (let ((user (gethash :user ningle:*session*)))
        (djula:render-template* "main/profile.html" nil :title "Profile" :user user)))


(defun unauthorized-profile (params)
    (setf (lack.response:response-status ningle:*response*) 403)
    (djula:render-template* "error.html" nil :title "Error" :error "Unauthorized"))


(defun people (params)
    (let ((users (mito:retrieve-dao 'ningle-auth/models:user)))
        (djula:render-template* "main/people.html" nil :title "People" :users users :user (cu-sith:logged-in-p))))


(defun person (params)
    (let* ((username-or-email (ingle:get-param :person params))
           (person (first (mito:select-dao
                            'ningle-auth/models:user
                            (where (:or (:= :username username-or-email)
                                        (:= :email username-or-email)))))))
        (djula:render-template* "main/person.html" nil :title "Person" :person person :user (cu-sith:logged-in-p))))
