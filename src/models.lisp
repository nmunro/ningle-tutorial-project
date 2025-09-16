(defpackage ningle-tutorial-project/models
  (:use :cl :mito :sxql)
  (:import-from :ningle-auth/models #:user)
  (:export #:post
           #:id
           #:content
           #:likes
           #:user
           #:liked-post-p
           #:logged-in-posts
           #:not-logged-in-posts
           #:toggle-like))

(in-package ningle-tutorial-project/models)

(deftable post ()
  ((user    :col-type ningle-auth/models:user :initarg :user    :accessor user)
   (content :col-type (:varchar 140)          :initarg :content :accessor content)))

(deftable likes ()
  ((user :col-type ningle-auth/models:user :initarg :user :reader user)
   (post :col-type post                    :initarg :post :reader post))
  (:unique-keys (user post)))

(defgeneric likes (post)
  (:documentation "Returns the number of likes a post has"))

(defmethod likes ((post post))
  (mito:count-dao 'likes :post post))

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

(defgeneric logged-in-posts (user)
  (:documentation "Gets the posts for a logged in user"))

(defmethod logged-in-posts ((user user))
  (let ((uid (slot-value user 'mito.dao.mixin::id)))
    (mito:retrieve-by-sql
        (sxql:yield
            (sxql:select
                (:post.*
                (:as (:count :likes.id) :like_count)
                (:as (:count :user_likes.id) :liked_by_user))
                (sxql:from :post)
                (sxql:left-join :likes :on (:= :post.id :likes.post_id))
                (sxql:left-join (:as :likes :user_likes)
                                :on (:and (:= :post.id :user_likes.post_id)
                                          (:= :user_likes.user_id :?)))
                (sxql:group-by :post.id)
                (sxql:order-by (:desc :post.created_at))
                (sxql:limit 50)))
            :binds (list uid))))

(defun not-logged-in-posts ()
    (mito:retrieve-by-sql
        (sxql:yield
        (sxql:select
            (:post.* (:as (:count :likes.id) :like_count))
            (sxql:from :post)
            (sxql:left-join :likes :on (:= :post.id :likes.post_id))
            (sxql:group-by :post.id)
            (sxql:order-by (:desc :post.created_at))
            (sxql:limit 50)))))
