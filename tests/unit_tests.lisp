;;;; unit_tests.lisp
;;;;
;;;; Main file for the unit tests that handle the calls to the actual tests.
;;;;
(in-package #:evolution.tests)

(defun run-all-tests ()
  (lisp-unit2:run-tests :package :evolution.tests
                         :run-contexts #'with-summary-context))

(defun run-basic-helper-tests ()
  (lisp-unit2:run-tests
    :name :basic-helper-tests
    :tests '(test-flatten-list-empty
             test-flatten-list-single
             test-flatten-list-single-nested
             test-split-list-empty
             test-split-list-neg-at
             test-split-list-at-past-end
             test-split-list-at-end
             test-split-list-at-beg)
    :tags '(:basic-tests)))

(defun run-basic-evo-tests ()
  (lisp-unit2:run-tests
    :name :basic-evolution-tests
    :tests '(test-create-candidate
             test-matchp-true
             test-matchp-false-1
             test-matchp-false-2
             test-get-next-ascii-cap
             test-get-next-ascii-cap-roll
             test-get-next-ascii-cap-inval
             test-all-matches
             test-best-match)
    :tags '(basic-tests)))

(defun run-roulette-tests ()
  (lisp-unit2:run-tests
    :name :roulette-tests
    :tests '(test-create-wheel
             test-search-wheel-min)
    :tags '(basic-tests)))
