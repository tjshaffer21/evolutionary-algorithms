;;;;test_roulette.lisp
;;;;
;;;; Unit Testing for the Roulette Wheel.
;;;;
(in-package #:evolution.tests)

;;; test-create-wheel also ends up testing: align-slots and get-slots
(lisp-unit2:define-test test-create-wheel
  (:tags '(:roulette))
  (let* ((c0 (evolution::create-candidate "AAAAA"))
         (c1 (evolution::create-candidate "ABCDE"))
         (c2 (evolution::create-candidate "FGHIJ"))
         (c3 (evolution::create-candidate "KLMNO"))
         (clist (list c0 c1 c2 c3))
         (wheel (evolution::create-wheel clist 5)))
    (lisp-unit2::assert-eql 0 (first (nth 0 wheel)))
    (lisp-unit2::assert-eql 1 (first (nth 1 wheel)))
    (lisp-unit2::assert-eql 2 (first (nth 2 wheel)))
    (lisp-unit2::assert-eql 3 (first (nth 3 wheel)))

    (lisp-unit2::assert-eql 0 (car (cadr (nth 0 wheel))))
    (lisp-unit2::assert-eql 89.9 (cadadr (nth 0 wheel)))

    (lisp-unit2::assert-eql 90 (car (cadr (nth 1 wheel))))
    (lisp-unit2::assert-eql 179.9 (cadadr (nth 1 wheel)))

    (lisp-unit2::assert-eql 180 (car (cadr (nth 2 wheel))))
    (lisp-unit2::assert-eql 269.9 (cadadr (nth 2 wheel)))

    (lisp-unit2::assert-eql 270 (car (cadr (nth 3 wheel))))
    (lisp-unit2::assert-eql 359.9 (cadadr (nth 3 wheel)))))

(lisp-unit2:define-test test-search-wheel-min
  (:tags '(:roulette))
  (let* ((c0 (evolution::create-candidate "AAAAA"))
         (c1 (evolution::create-candidate "ABCDE"))
         (c2 (evolution::create-candidate "FGHIJ"))
         (c3 (evolution::create-candidate "KLMNO"))
         (clist (list c0 c1 c2 c3))
         (wheel (evolution::create-wheel clist 5)))
    (lisp-unit2::assert-eql 0 (evolution::search-wheel wheel 0))
    (lisp-unit2::assert-eql 0 (evolution::search-wheel wheel 45))
    (lisp-unit2::assert-eql 1 (evolution::search-wheel wheel 90))
    (lisp-unit2::assert-eql 2 (evolution::search-wheel wheel 180))
    (lisp-unit2::assert-eql 3 (evolution::search-wheel wheel 270))
    (lisp-unit2::assert-eql 4 (evolution::search-wheel wheel 359.9))
    (lisp-unit2::assert-eql 1 (evolution::search-wheel wheel 95))
    (lisp-unit2::assert-eql 3 (evolution::search-wheel wheel 260))))
