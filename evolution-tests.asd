;;;; evolution-tests.asd
(asdf:defsystem #:evolution-tests
  :description "Unit tests for evolution algorithms."
  :author "Thomas Shaffer <tjshaffer21@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (#:lisp-unit2
               #:evolution)
  :components ((:file "tests/package")
               (:file "tests/unit_tests")
               (:file "tests/test_helpers")
               (:file "tests/test_evolution")
               (:file "tests/test_roulette")))
