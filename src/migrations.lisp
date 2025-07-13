(defpackage ningle-tutorial-project/migrations
  (:use :cl :ningle-tutorial-project/contrib)
  (:export #:migrate-apps))

(in-package :ningle-tutorial-project/migrations)

(defun migrate-apps (&optional (apps nil))
  "Run migrate function for each app in APPS list. If APPS is nil, migrate all apps listed in *config* :installed-apps."
  (let ((apps (or apps (getf (envy:config :ningle-tutorial-project/config) :installed-apps))))
    (unless apps
      (error "No apps specified and no :installed-apps found in config."))

    (with-db-connection
        (dolist (app apps)
            (let* ((migrations-pkg-name (string-upcase (format nil "~A/MIGRATIONS" (string-upcase (symbol-name app)))))
                   (migrations-pkg (find-package migrations-pkg-name)))
                (unless migrations-pkg
                    (error "Migrations package ~A not found." migrations-pkg-name))

                ;; Set app-specific config before calling migrate
                (let ((migrate-fn (find-symbol "MIGRATE" migrations-pkg))) ;; Name known to project
                    (unless (and migrate-fn (fboundp migrate-fn))
                        (error "Migrate function not found in package ~A." migrations-pkg-name))
                    (funcall migrate-fn)))))))
