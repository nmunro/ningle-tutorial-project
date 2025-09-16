(defpackage ningle-tutorial-project/migrations
  (:use :cl :ningle-tutorial-project/contrib)
  (:export #:migrate-apps))

(in-package :ningle-tutorial-project/migrations)

(defun migrate ()
  "Explicitly apply migrations when called."
  (format t "Applying migrations...~%")
  (mito:ensure-table-exists 'ningle-tutorial-project/models:post)
  (mito:ensure-table-exists 'ningle-tutorial-project/models:likes)
  (mito:migrate-table 'ningle-tutorial-project/models:post)
  (mito:migrate-table 'ningle-tutorial-project/models:likes)
  (format t "Migrations complete.~%"))

(defun migrate-apps (&optional (apps nil) &key skip-root)
  "Run migrate function for each app in APPS list. If APPS is nil, migrate all apps listed in *config* :installed-apps."

  (let ((apps (or apps (getf (envy:config :ningle-tutorial-project/config) :installed-apps))))
    (unless apps
      (error "No apps specified and no :installed-apps found in config."))

    (with-db-connection
        (unless skip-root
          (format t "Running root project migrations...~%")
          (migrate))

        (dolist (app apps)
            (let* ((migrations-pkg-name (string-upcase (format nil "~A/MIGRATIONS" (string-upcase (symbol-name app)))))
                   (migrations-pkg (find-package migrations-pkg-name)))
                (unless migrations-pkg
                    (error "Migrations package ~A not found." migrations-pkg-name))

                ;; Set app-specific config before calling migrate
                (let ((migrate-fn (find-symbol "MIGRATE" migrations-pkg))) ;; Name known to project
                    (unless (and migrate-fn (fboundp migrate-fn))
                        (error (format nil "Migrate function not found in package ~A." migrations-pkg-name)))
                    (funcall migrate-fn)))))))
