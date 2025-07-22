(defpackage ningle-tutorial-project/db
  (:use :cl :mito)
  (:export #:with-db-connection))

(in-package :ningle-tutorial-project/db)

(defmacro with-db-connection (&body body)
    `(multiple-value-bind (backend args) (envy-ningle:extract-mito-config :ningle-tutorial-project/config)
        (unless backend
            (error "No MITO backend found for config ~A" cfg))

        (unwind-protect
             (progn
               (apply #'mito:connect-toplevel backend args)
               ,@body
               (mito:disconnect-toplevel)))))
