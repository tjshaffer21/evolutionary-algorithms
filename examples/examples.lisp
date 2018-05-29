;;;;examples.lisp
(in-package #:evolution.examples)

(defun print-candidate (c)
  "Print the representation and fitness of the given candidate."
  (when (null c) (return-from print-candidate nil))
  (format t "~a ~T(~a)~%" (evolution::candidate-representation c)
                          (evolution::candidate-fitness c)))

(defun print-population (p)
  "Print the representations and fitness for all candidates in the given population P."
  (when (null p) (return-from print-population nil))
  (dotimes (i (length p))
    (format t "~2<~d~>. ~T~a ~T(~a)~%" (1+ i)
            (evolution::candidate-representation (nth i p))
            (evolution::candidate-fitness (nth i p)))))

(defun main-tru (target rsize psize limit fit)
  (let* ((tru (time (evolution :truncation :ascii-caps target rsize psize limit
                    fit))))
    (format t "~%~a || ~a || ~a~%----~%" target limit fit)
    (format t "Truncation:~%")
    (print-candidate (first tru))
    (format t "Generation: ~a~%~%" (first (last tru)))))

(defun main-rou (target rsize psize limit fit)
  (let* ((rou (time (evolution :roulette :ascii-caps target rsize psize limit))))
    (format t "~%~a || ~a || ~a~%----~%" target limit fit)
    (format t "Roulette:~%")
    (print-candidate (first rou))
    (format t "Generation: ~a~%~%" (first (last rou)))))

(defun main-hello ()
  (main-tru "HELLO" 5 100 1000 0.75))
