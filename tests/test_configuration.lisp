;;;;test_configuration.lisp
;;;;
;;;; Unit Testing for the configuration file.
;;;;
(in-package #:evolution.tests)

(lisp-unit2:define-test test-chromosome-length
  (:tags '(:configuration))
  (setf evolution::*chromosome-length* 0) ;; Just in case this isn't a fresh system.
  (lisp-unit2:assert-eql 0 evolution::*chromosome-length*)
  (lisp-unit2:assert-error 'evolution::data-error (evolution::set-chromosome-length -1))
  (evolution::set-chromosome-length 5)
  (lisp-unit2:assert-eql 5 evolution::*chromosome-length*))

(lisp-unit2:define-test test-global-local-search
  (:tags '(:configuration))
  (setf evolution::*global-local-search* 0) ;; Just in case this isn't a fresh system.
  (lisp-unit2:assert-eql 0 evolution::*global-local-search*)
  (lisp-unit2:assert-error 'evolution::data-error (evolution::set-global-local-search -1))
  (lisp-unit2:assert-error 'evolution::data-error (evolution::set-global-local-search 2))
  (evolution::set-global-local-search 1)
  (lisp-unit2:assert-eql 1 evolution::*global-local-search*))
