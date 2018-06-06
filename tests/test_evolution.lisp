;;;;test_evolution.lisp
;;;;
;;;; Unit Testing for the evolution.lisp file.
;;;;
(in-package #:evolution.tests)

(lisp-unit2:define-test test-create-candidate
  (:tags '(:evolution :positive))
  (let ((c (evolution::create-candidate "HELLO")))
    (lisp-unit2:assert-true (typep c 'evolution::candidate))
    (lisp-unit2:assert-equal (list #\H #\E #\L #\L #\O) (evolution::candidate-genotype c))
    (lisp-unit2:assert-eql 0 (evolution::candidate-fitness c))))

(lisp-unit2:define-test test-cross-over-error
  (:tags '(:evolution :negative))
  (lisp-unit2:assert-error 'evolution::data-error
                           (evolution::cross-over (evolution::create-candidate "ABCD") nil))
  (lisp-unit2:assert-error 'evolution::data-error
                           (evolution::cross-over nil (evolution::create-candidate "ABCD")))
  (lisp-unit2:assert-error 'evolution::data-error (evolution::cross-over nil nil)))

(lisp-unit2:define-test test-cross-over
  (:tags '(:evolution :positive))
  (let* ((p1 (evolution::create-candidate "ABCD"))
         (p2 (evolution::create-candidate "EFGH"))
         (ch (evolution::cross-over p1 p2))
         (c1 (first ch))
         (c2 (first (last ch))))
    (lisp-unit2:assert-true (typep ch 'list))
    (lisp-unit2:assert-true (typep c1 'evolution::candidate))
    (lisp-unit2:assert-true (typep c2 'evolution::candidate))))

(lisp-unit2:define-test test-generate-candidate
  (:tags '(:evolution :positive))
  (let ((test (evolution::generate-candidate :ascii-caps)))
    (lisp-unit2:assert-true (typep test 'evolution::candidate))
    (lisp-unit2:assert-eql 0 (evolution::candidate-fitness test))))

(lisp-unit2:define-test test-generate-candidate-error
  (:tags '(:evolution :negative))
  (lisp-unit2:assert-error 'evolution::implementation-error
                            (evolution::generate-candidate :ascii-low)))

(lisp-unit2:define-test test-genesis-error
  (:tags '(:evolution :negative))
  (setf evolution::*chromosome-length* 1)
  (lisp-unit2:assert-error 'evolution::data-error
                            (evolution::genesis :ascii-caps 100))
  (setf evolution::*chromosome-length* 3)
  (lisp-unit2:assert-error 'evolution::data-error
                            (evolution::genesis :ascii-caps 1))
  (setf evolution::*chromosome-length* 3)
  (lisp-unit2:assert-error 'evolution::implementation-error
                            (evolution::genesis :ascii-low 10)))

(lisp-unit2:define-test test-genesis
  (:tags '(:evolution :positive))
  (let ((g (evolution::genesis :ascii-caps 100)))
    (lisp-unit2:assert-eql 100 (length g))))
