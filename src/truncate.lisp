;;;;truncate.lisp
;;;;
;;;; Funcations and macros used only for Truncation Selection.
;;;;
(in-package #:evolution)

(defun truncation (population fitness limit)
  "Truncation selection.

   Parameters
    population : list
    fitness    : function : fitnes-func
    limit      : int
   Return
    list : (nil, generation) if no result was found.
           (match, generation) if match was found."
    (iter
      (with generation = 1)
      (with pop = (ncalculate-population-fitness population fitness))
      (with potential = nil)
      (with match = nil)
      (while (<= generation limit))
      (if (null match)
          (progn
            (setf potential (max-candidate pop))
            (setf match (list potential generation)))
          (progn
            (setf potential (max-candidate pop))
            (when (> (candidate-fitness potential) (candidate-fitness (first match)))
                  (setf match (list potential generation)))))
      (when (= (candidate-fitness (first match)) 1) (return match))

      (incf generation)
      (setf pop (ncalculate-population-fitness
                (flatten-list (determine-children (determine-parents pop)))
                fitness))
      (finally (return match))))

(defun determine-parents (population)
  "Determine the parents used for the next generation.

   Parameters
    population : list
   Return
    list"
  (let ((parents (cull-population population 0))
        (pop-size (if (oddp (length population))
                      (1+ (length population))
                      (length population))))
    (when (null parents) (setf parents population))
    (iter
      (until (>= (length parents) pop-size))
      (push (nth (random (length parents)) parents) parents))
    parents))
