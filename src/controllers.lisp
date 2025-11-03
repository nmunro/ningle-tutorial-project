(defpackage ningle-tutorial-project/controllers
  (:use :cl :sxql)
  (:import-from :ningle-tutorial-project/forms
                #:post
                #:content
                #:parent
                #:comment)
  (:export #:index
           #:post-likes
           #:single-post
           #:post-content
           #:post-comment
           #:logged-in-profile
           #:unauthorized-profile
           #:people
           #:person))

(in-package ningle-tutorial-project/controllers)


(defun index (params)
    (let* ((user (gethash :user ningle:*session*))
           (posts (ningle-tutorial-project/models:posts user)))
        (djula:render-template* "main/index.html" nil :title "Home" :user user :posts posts :form (if user (cl-forms:find-form 'post) nil))))


(defun post-likes (params)
  (let* ((user (gethash :user ningle:*session*))
         (post (mito:find-dao 'ningle-tutorial-project/models:post :id (parse-integer (ingle:get-param :id params))))
         (res (make-hash-table :test 'equal)))
    ;; Bail out if post does not exist
    (unless post
      (setf (getf (lack.response:response-headers ningle:*response*) :content-type) "application/json")
      (setf (gethash "error" res) "post not found")
      (setf (lack.response:response-status ningle:*response*) 404)
      (return-from post-likes (com.inuoe.jzon.stringify res)))

    ;; success, continue
    (setf (gethash "post" res) (mito:object-id post))
    (setf (gethash "liked" res) (ningle-tutorial-project/models:toggle-like user post))
    (setf (gethash "likes" res) (ningle-tutorial-project/models:likes post))
    (setf (getf (lack.response:response-headers ningle:*response*) :content-type) "application/json")
    (setf (lack.response:response-status ningle:*response*) 201)
    (com.inuoe.jzon:stringify res)))


(defun single-post (params)
    (handler-case
        (let ((post (mito:find-dao 'ningle-tutorial-project/models:post :id (parse-integer (ingle:get-param :id params))))
              (form (cl-forms:find-form 'comment)))
          (cl-forms:set-field-value form 'ningle-tutorial-project/forms:parent (mito:object-id post))
          (djula:render-template* "main/post.html" nil
                                  :title "Post"
                                  :post post
                                  :comments (ningle-tutorial-project/models:comments post (gethash :user ningle:*session*))
                                  :likes (ningle-tutorial-project/models:likes post)
                                  :form form
                                  :user (gethash :user ningle:*session*)))

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
                            (mito:create-dao 'ningle-tutorial-project/models:post :content content :user user :parent nil)
                            (ingle:redirect "/")))))

            (simple-error (err)
                (setf (lack.response:response-status ningle:*response*) 403)
                (djula:render-template* "error.html" nil :title "Error" :error err)))))


(defun post-comment (params)
    (let ((user (gethash :user ningle:*session*))
          (form (cl-forms:find-form 'comment)))
        (handler-case
            (progn
                (cl-forms:handle-request form) ; Can throw an error if CSRF fails

                (multiple-value-bind (valid errors)
                    (cl-forms:validate-form form)

                    (when errors
                        (format t "Errors: ~A~%" errors))

                    (when valid
                        (cl-forms:with-form-field-values (content parent) form
                            (mito:create-dao 'ningle-tutorial-project/models:post :content content :user user :parent (parse-integer parent))
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
