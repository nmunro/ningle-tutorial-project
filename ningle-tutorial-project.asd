(defsystem "ningle-tutorial-project"
  :version "0.0.1"
  :author "nmunro"
  :license "BSD3-Clause"
  :description ""
  :depends-on (:clack
               :ningle
               :djula
               :cl-dotenv
               :cl-forms
               :cl-forms.djula
               :cl-forms.ningle
               :postmodern
               :foo.lisp.lack-middleware-postmodern
               :cl-yesql
               :cl-yesql/postmodern)
  :components ((:module "src"
                :components
                ((:file "forms")
                 (:file "migrations")
                 (:file "user")
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
