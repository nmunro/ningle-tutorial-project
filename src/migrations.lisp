(defpackage ningle-tutorial-project/migrations
  (:use :cl)
  (:export #:*migration-registry*
           #:migrate
           #:model
           #:register-model))

(in-package ningle-tutorial-project/migrations)

(defvar *migration-registry* (make-hash-table :test 'eq))

(defun register-model (class-symbol migration-function)
  "Register a migration function for a specific class."
  (setf (gethash class-symbol *migration-registry*) migration-function))

(defun migrate (class-symbol)
  "Look up the migration function for the given class and execute it."
  (let ((migration-fn (gethash class-symbol *migration-registry*)))
    (if migration-fn
        (funcall migration-fn)
        (error "No migration function registered for class ~A" class-symbol))))

(defclass model ()
  ((id :initarg :id :initform nil :reader id)))
