;;;;test_evolution.lisp
;;;;
;;;; Unit Testing for te evolution.lisp file.
;;;;
(in-package #:evolution.tests)

(lisp-unit2:define-test test-create-candidate
  (:tags '(:evolution))
  (let ((c (evolution::create-candidate "HELLO")))
    (lisp-unit2:assert-true (typep c 'evolution::candidate))
    (lisp-unit2:assert-equal "HELLO" (evolution::candidate-representation c))
    (lisp-unit2:assert-eql 0 (evolution::candidate-fitness c))))

(lisp-unit2:define-test test-matchp-true
  (:tags '(:evolution))
  (let ((c (evolution::create-candidate "HELLO" 1.0)))
    (lisp-unit2:assert-true (evolution::matchp c))))

(lisp-unit2:define-test test-matchp-false
  (:tags '(:evolution))
  (let ((c (evolution::create-candidate "HELLW" 0.9)))
    (lisp-unit2:assert-false (evolution::matchp c)))

  (lisp-unit2:assert-false (evolution::matchp 2)))

(lisp-unit2:define-test test-get-next-ascii-cap
  (:tags '(:evolution))
  (lisp-unit2:assert-equal #\D (evolution::get-next-ascii-cap #\C)))

(lisp-unit2:define-test test-get-next-ascii-cap-roll
  (:tags '(:evolution))
  (lisp-unit2:assert-equal #\A (evolution::get-next-ascii-cap #\Z)))

(lisp-unit2:define-test test-get-next-ascii-cap-inval
  (:tags '(:evolution))
  (lisp-unit2:assert-equal #\A (evolution::get-next-ascii-cap -3)))

(lisp-unit2:undefine-test test-all-matches-nil
  (:tags '(:evolution)))

(lisp-unit2:define-test test-all-matches
  (:tags '(:evolution))
  (let* ((c1 (evolution::create-candidate #\A 1.0))
         (c2 (evolution::create-candidate #\B 0.9))
         (c3 (evolution::create-candidate #\C 0.3))
         (c4 (evolution::create-candidate #\D 0.5))
         (c5 (evolution::create-candidate #\E 0.75))
         (cl (list c1 c2 c3 c4 c5)))
    (lisp-unit2:assert-equal (list c1 c2 c5) (evolution::all-matches cl 0.7))))

    (lisp-unit2:define-test test-best-match
      (:tags '(:evolution))
      (let* ((c1 (evolution::create-candidate #\A 1.0))
             (c2 (evolution::create-candidate #\B 0.9))
             (c3 (evolution::create-candidate #\C 0.3))
             (c4 (evolution::create-candidate #\D 0.5))
             (c5 (evolution::create-candidate #\E 0.75))
             (cl (list c1 c2 c3 c4 c5)))
        (lisp-unit2:assert-equal c1 (evolution::best-match cl 0.7))))
