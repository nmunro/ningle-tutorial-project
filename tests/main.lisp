(defpackage ningle-tutorial-project/tests/main
  (:use :cl
        :ningle-tutorial-project
        :rove))
(in-package :ningle-tutorial-project/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :ningle-tutorial-project)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
  (format t "Testing~%")
    (ok (= 1 1))))