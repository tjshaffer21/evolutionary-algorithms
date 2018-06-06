;;;; unit_tests.lisp
;;;;
;;;; Main file for the unit tests that handle the calls to the actual tests.
;;;;
(in-package #:evolution.tests)

(defun run-all-tests ()
  (lisp-unit2:run-tests :package :evolution.tests
                         :run-contexts #'with-summary-context))

(defun run-basic-config-tests ()
  (lisp-unit2:run-tests
    :name :basic-config-tests
    :tests '(test-chromosome-length
             test-global-local-search)))

(defun run-basic-helper-tests ()
  (lisp-unit2:run-tests
    :name :basic-helper-tests
    :tests '(test-flatten-list-empty
             test-flatten-list-single
             test-flatten-list-single-nested
             test-get-next-ascii-cap
             test-get-next-ascii-cap-roll
             test-get-next-ascii-cap-inval
             test-split-empty
             test-split-neg-at
             test-split-at-past-end
             test-split-at-end
             test-split-at-beg)
    :tags '(:basic-tests)))

(defun run-basic-evo-tests ()
  (lisp-unit2:run-tests
    :name :basic-evolution-tests
    :tests '(test-create-candidate
             test-generate-candidate
             test-generate-candidate-error
             test-genesis
             test-genesis-error
             test-cross-over-error
             test-cross-over
             test-matchp-true
             test-matchp-false
             test-all-matches
             test-best-match)
    :tags '(basic-tests)))

(defun run-roulette-tests ()
  (lisp-unit2:run-tests
    :name :roulette-tests
    :tests '(test-create-wheel
             test-search-wheel-min)
    :tags '(basic-tests)))
