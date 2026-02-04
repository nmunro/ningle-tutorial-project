(defpackage ningle-tutorial-project/models
  (:use :cl :mito :sxql)
  (:import-from :ningle-auth/models #:user)
  (:export #:post
           #:id
           #:content
           #:comments
           #:likes
           #:user
           #:liked-post-p
           #:posts
           #:parent
           #:toggle-like))

(in-package ningle-tutorial-project/models)

(deftable post ()
  ((user    :col-type ningle-auth/models:user :initarg :user    :accessor user)
   (parent  :col-type (or :post :null)        :initarg :parent  :reader parent :initform nil)
   (content :col-type (:varchar 140)          :initarg :content :accessor content)))

(deftable likes ()
  ((user :col-type ningle-auth/models:user :initarg :user :reader user)
   (post :col-type post                    :initarg :post :reader post))
  (:unique-keys (user post)))

(defgeneric likes (post)
  (:documentation "Returns the number of likes a post has"))

(defmethod likes ((post post))
  (mito:count-dao 'likes :post post))

(defgeneric comments (post user)
  (:documentation "Gets the comments for a logged in user"))

(defmethod comments ((post post) (user user))
    (mito:retrieve-by-sql
        (sxql:yield
            (sxql:select
                (:post.*
                    (:as :user.username :username)
                    (:as (:count :likes.id) :like_count)
                    (:as (:count :user_likes.id) :liked_by_user))
                (sxql:from :post)
                (sxql:where (:= :parent :?))
                (sxql:left-join :user :on (:= :post.user_id :user.id))
                (sxql:left-join :likes :on (:= :post.id :likes.post_id))
                (sxql:left-join (:as :likes :user_likes)
                                :on (:and (:= :post.id :user_likes.post_id)
                                          (:= :user_likes.user_id :?)))
                (sxql:group-by :post.id)
                (sxql:order-by (:desc :post.created_at))
                (sxql:limit 50)))
            :binds (list (mito:object-id post) (mito:object-id user))))

(defmethod comments ((post post) (user null))
    (mito:retrieve-by-sql
        (sxql:yield
        (sxql:select
            (:post.*
              (:as :user.username :username)
              (:as (:count :likes.id) :like_count))
            (sxql:from :post)
            (sxql:where (:= :parent :?))
            (sxql:left-join :user :on (:= :post.user_id :user.id))
            (sxql:left-join :likes :on (:= :post.id :likes.post_id))
            (sxql:group-by :post.id)
            (sxql:order-by (:desc :post.created_at))
            (sxql:limit 50)))
        :binds (list (mito:object-id post))))

(defgeneric toggle-like (user post)
  (:documentation "Toggles the like of a user to a given post"))

(defmethod toggle-like ((ningle-auth/models:user user) (post post))
  (let ((liked-post (liked-post-p user post)))
    (if liked-post
        (mito:delete-dao liked-post)
        (mito:create-dao 'likes :post post :user user))
    (not liked-post)))

(defgeneric liked-post-p (user post)
  (:documentation "Returns true if a user likes a given post"))

(defmethod liked-post-p ((ningle-auth/models:user user) (post post))
  (mito:find-dao 'likes :user user :post post))

(defgeneric posts (user &key offset limit)
  (:documentation "Gets the posts"))

(defmethod posts ((user user) &key offset limit)
  (multiple-value-bind (sql params)
        (sxql:yield
              (sxql:select
                  (:post.*
                    (:as :user.username :username)
                    (:as (:count :likes.id) :like_count)
                    (:as (:count :user_likes.id) :liked_by_user))
                  (sxql:from :post)
                  (sxql:left-join :user :on (:= :post.user_id :user.id))
                  (sxql:left-join :likes :on (:= :post.id :likes.post_id))
                  (sxql:left-join (:as :likes :user_likes)
                                  :on (:and (:= :post.id :user_likes.post_id)
                                            (:= :user_likes.user_id (mito:object-id user))))
                  (sxql:group-by :post.id)
                  (sxql:order-by (:desc :post.created_at))
                  (sxql:offset offset)
                  (sxql:limit limit)))
      (values
        (mito:retrieve-by-sql sql :binds params)
        (mito:count-dao 'post))))

(defmethod posts ((user null) &key offset limit)
  (multiple-value-bind (sql)
      (sxql:yield
        (sxql:select
            (:post.*
              (:as :user.username :username)
              (:as (:count :likes.id) :like_count))
            (sxql:from :post)
            (sxql:left-join :user :on (:= :post.user_id :user.id))
            (sxql:left-join :likes :on (:= :post.id :likes.post_id))
            (sxql:group-by :post.id)
            (sxql:order-by (:desc :post.created_at))
            (sxql:limit limit)
            (sxql:offset offset)))
    (values
      (mito:retrieve-by-sql sql)
      (mito:count-dao 'post))))
