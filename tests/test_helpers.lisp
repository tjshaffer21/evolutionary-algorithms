;;;;test_helpers.lisp
;;;;
;;;; Unit Tests for the helper functions.
(in-package #:evolution.tests)

(lisp-unit2:define-test test-flatten-list-empty
  (:tags '(:helpers :flatten))
  (lisp-unit2:assert-false (evolution::flatten-list '())))

(lisp-unit2:define-test test-flatten-list-single
  (:tags '(:helpers :flatten))
  (let ((c1 (evolution::create-candidate "ABCD")))
    (lisp-unit2:assert-equal (list c1) (evolution::flatten-list (list c1)))))

(lisp-unit2:define-test test-flatten-list-single-nested
  (:tags '(:helpers :flatten))
  (let* ((c1 (evolution::create-candidate "ABCD"))
         (c2 (evolution::create-candidate "EFGH"))
         (c3 (evolution::create-candidate "IJKL"))
         (c4 (evolution::create-candidate "MNOP"))
         (clist (list c1 (list c2 c3) c4))
         (cres (list c1 c2 c3 c4)))
    (lisp-unit2:assert-equal cres (evolution::flatten-list clist))))

(lisp-unit2:define-test test-split-list-empty
  (:tags '(:helpers :split))
  (lisp-unit2:assert-false (evolution::split-list '() 0)))

(lisp-unit2:define-test test-split-list-neg-at
  (:tags '(:helpers :split))
  (lisp-unit2:assert-false (evolution::split-list (list 1 2 3) -4)))

(lisp-unit2:define-test test-split-list
  (:tags '(:helpers :split))
  (lisp-unit2::assert-equal (list (list 1 2 3) (list 4 5))
                          (evolution::split-list (list 1 2 3 4 5) 3)))

(lisp-unit2:define-test test-split-list-at-past-end
  (:tags '(:helpers :split))
  (lisp-unit2:assert-equal nil
                          (evolution::split-list (list 1 2 3) 4)))

(lisp-unit2:define-test test-split-list-at-end
  (:tags '(:helpers :split))
  (lisp-unit2:assert-equal (list (list 1 2 3 4) (list 5))
                          (evolution::split-list (list 1 2 3 4 5) 4)))

(lisp-unit2:define-test test-split-list-at-beg
  (:tags '(:helpers :split))
  (lisp-unit2:assert-eql nil (evolution::split-list (list 1 2 3 4 5) 0)))
