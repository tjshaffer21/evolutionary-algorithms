;;;;truncate.lisp
;;;;
;;;; Funcations and macros used only for Truncation Selection.
;;;;
(in-package #:evolution)

(defun evolution-truncation (population target limit fit-ratio)
  "Truncation selection.

  Parameter
    population : list
    target
    limit      : int : Max generations.
    fit-ratio  : float (0.0-1.0) : How strict the desires results should be.
  Return
    (nil, generation) if no result was found.
    (match, generation) if match was found."
  (iter
    (with generation = 1)
    (with pop = (fitness population target))
    (with match = (list (best-match pop fit-ratio) 1))
    (with match-next)
    (while (and (<= generation limit) (not (matchp (first match)))))
    (incf generation)

    (setf pop
          (fitness (flatten-list
                    (determine-children
                     (et-determine-parents pop fit-ratio)))
                   target))

    (best-match-check pop fit-ratio generation match match-next)
    (finally (return match))))

(defun et-determine-parents (population fit-ratio)
  "Determine parents using Truncation Selection by taking the candidate list
   POPULATION and the desired min. FIT-RATIO to find fitter parents. If
   no fit parents exist then parents are choosen from all of POPULATION."
  (let ((parents (all-matches population fit-ratio))
        (pop-size (cond ((oddp (length population)) (+ 1 (length population)))
                        (t (length population)))))
    (when (null parents) (setf parents (all-matches population 0.001))) ; Non-zero

    (cond ((and (> (length parents) 0) (< (length parents) pop-size))
           (dotimes (i (- pop-size (length parents)))
             (push (nth (random (length parents)) parents) parents)))
          (t
           (dotimes (i pop-size)
             (push (nth (random pop-size) population) parents))))
    parents))
