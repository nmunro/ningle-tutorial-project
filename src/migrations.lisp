(defpackage ningle-tutorial-project/migrations
  (:use :cl :mito)
  (:export #:migrate))

(in-package :ningle-tutorial-project/migrations)

(defun migrate ()
  "Explicitly apply migrations when called."
  (dotenv:load-env (asdf:system-relative-pathname :ningle-tutorial-project ".env"))
  (format t "Applying migrations...~%")
  (mito:connect-toplevel
    :postgres
    :database-name (uiop:getenv "POSTGRES_DB_NAME")
    :host (uiop:getenv "POSTGRES_ADDRESS")
    :port (parse-integer (uiop:getenv "POSTGRES_PORT"))
    :username (uiop:getenv "POSTGRES_USER")
    :password (uiop:getenv "POSTGRES_PASSWORD"))
  (mito:ensure-table-exists 'ningle-tutorial-project/models:user)
  (mito:migrate-table 'ningle-tutorial-project/models:user)
  (mito:disconnect-toplevel)
  (format t "Migrations complete.~%"))
