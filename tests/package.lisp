;;;; package.lisp
(defpackage #:evolution.tests
  (:use #:cl #:lisp-unit2 #:evolution)
  (:export :run-all-tests
           :run-basic-helper-tests
           :run-basic-evo-tests
           :run-roulette-tests))
