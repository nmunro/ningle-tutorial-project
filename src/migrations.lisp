(defpackage ningle-tutorial-project/migrations
  (:use :cl :mito)
  (:export #:migrate))

(in-package :ningle-tutorial-project/migrations)

(defun migrate ()
  "Explicitly apply migrations when called."
  (dotenv:load-env (asdf:system-relative-pathname :ningle-tutorial-project ".env"))
  (mito:connect-toplevel
    :sqlite3
    :database-name (uiop:native-namestring (uiop:parse-unix-namestring (uiop:getenv "SQLITE_DB_NAME"))))

  (format t "Applying migrations...~%")
  (mito:ensure-table-exists 'ningle-tutorial-project/models:user)
  (mito:migrate-table 'ningle-tutorial-project/models:user)
  (format t "Migrations complete.~%")

  (mito:disconnect-toplevel))
