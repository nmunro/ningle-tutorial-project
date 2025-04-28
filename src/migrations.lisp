(defpackage ningle-tutorial-project/migrations
  (:use :cl :mito)
  (:export #:migrate))

(in-package :ningle-tutorial-project/migrations)

(defun migrate ()
  "Explicitly apply migrations when called."
  (dotenv:load-env (asdf:system-relative-pathname :ningle-tutorial-project ".env"))
  (format t "Applying migrations...~%")
  (mito:connect-toplevel
    :mysql
    :database-name (uiop:native-namestring (uiop:parse-unix-namestring (uiop:getenv "MYSQL_DB_NAME")))
    :username (uiop:getenv "MYSQL_USER")
    :password (uiop:getenv "MYSQL_PASSWORD")
    :host (uiop:getenv "MYSQL_ADDRESS")
    :port (parse-integer (uiop:getenv "MYSQL_PORT")))
  (mito:ensure-table-exists 'ningle-tutorial-project/models:user)
  (mito:migrate-table 'ningle-tutorial-project/models:user)
  (mito:disconnect-toplevel)
  (format t "Migrations complete.~%"))
