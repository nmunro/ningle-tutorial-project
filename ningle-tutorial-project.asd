(defsystem "ningle-tutorial-project"
  :version "0.0.1"
  :author "nmunro"
  :license "BSD3-Clause"
  :description ""
  :depends-on (:cl-dotenv
               :clack
               :djula
               :cl-forms
               :cl-forms.djula
               :cl-forms.ningle
               :envy
               :envy-ningle
               :ingle
               :mito
               :mito-auth
               :ningle)
  :components ((:module "src"
                :components
                ((:file "config")
                 (:file "models")
                 (:file "migrations")
                 (:file "forms")
                 (:file "main"))))
  :in-order-to ((test-op (test-op "ningle-tutorial-project/tests"))))

(defsystem "ningle-tutorial-project/tests"
  :author "nmunro"
  :license "BSD3-Clause"
  :depends-on ("ningle-tutorial-project"
               :rove)
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for ningle-tutorial-project"
  :perform (test-op (op c) (symbol-call :rove :run c)))
