(defpackage ningle-tutorial-project/controllers
  (:use :cl :sxql)
  (:import-from :ningle-tutorial-project/forms
                #:post
                #:content
                #:parent
                #:comment)
  (:export #:logged-in-index
           #:index
           #:post-likes
           #:single-post
           #:post-content
           #:post-comment
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
    ;; Bail out if post does not exist
    (unless post
      (setf (lack.response:response-status ningle:*response*) 404)
      (setf (getf (lack.response:response-headers ningle:*response*) :content-type) "application/json")
      (setf (gethash "ok" res) nil)
      (setf (gethash "error" res) "post not found")
      (setf (lack.response:response-status ningle:*response*) 404)
      (return-from post-likes (com.inuoe.jzon.stringify res)))

    (let ((liked (ningle-tutorial-project/models:toggle-like user post))
          (likes (ningle-tutorial-project/models:likes post)))
      (setf (gethash "post" res) (slot-value post 'mito.dao.mixin::id))
      (setf (gethash "likes" res) likes)
      (setf (gethash "liked" res) (if liked t nil))
      (setf (getf (lack.response:response-headers ningle:*response*) :content-type) "application/json")
      (setf (lack.response:response-status ningle:*response*) 201)
      (com.inuoe.jzon:stringify res))))

(defun single-post (params)
    (handler-case
        (let* ((post-id (parse-integer (ingle:get-param :id params)))
               (post (mito:find-dao 'ningle-tutorial-project/models:post :id post-id))
               (comments (ningle-tutorial-project/models:comments post))
               (likes (ningle-tutorial-project/models:likes post))
               (form (cl-forms:find-form 'comment))
               (user (gethash :user ningle:*session*)))
          (cl-forms:set-field-value form 'ningle-tutorial-project/forms:parent post-id)
          (djula:render-template* "main/post.html" nil
                                  :title "Post"
                                  :post post
                                  :comments comments
                                  :likes likes
                                  :form form
                                  :user user))

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
                            (mito:create-dao 'ningle-tutorial-project/models:post :content content :user user :parent 0)
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
