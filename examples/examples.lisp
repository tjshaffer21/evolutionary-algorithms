;;;;examples.lisp
(in-package #:evolution.examples)

(defun main-tru (fitness-func rsize psize limit)
  (let* ((tru (evolution :truncation :ascii-caps fitness-func
                         rsize psize limit)))
    (evolution::print-candidate (first tru))
    (format t "Generation: ~a~%~%" (first (last tru)))))

; (defun main-rou (target rsize psize limit fit)
;   (let* ((rou (time (evolution :roulette :ascii-caps target rsize psize limit))))
;     (format t "~%~a || ~a || ~a~%----~%" target limit fit)
;     (format t "Roulette:~%")
;     (print-candidate (first rou))
;     (format t "Generation: ~a~%~%" (first (last rou)))))

(defun main-hello ()
  (evolution:set-global-local-search 0) ; Default
  (main-tru (function fitness-hello) 5 100 10000))

(defun fitness-hello (candidate)
  (reduce #'+ (iter
                (for ele in-string "HELLO")
                (for rep in (evolution::candidate-genotype candidate))
                (collecting (if (= (evolution::char- rep ele) 0) 1 0)))))
