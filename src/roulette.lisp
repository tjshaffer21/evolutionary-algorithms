;;;; roulette.lisp
;;;;
;;;; Functions and macros for the Roulette evolutionary algorithms.
;;;;
(in-package #:evolution)

(defun align-slots (slots counter)
  "Align slots so fitter candidates take more space.

  To prevent overlap, the ending side is subtracted by 0.1. As such, a full
  circle is considered (0, 359.9).

  Parameters
    slots   : list : List of slots ((pos, fit) ...)
    counter : int  : Length of candidate list.
  Return
    list ((location (start stop)))"
  (let ((slot-list '()))
    (iterate:iter
      (iterate:with current = 0)
      (iterate:with c-counter = counter)
      (iterate:for i in slots)
      (iterate:iter
        (iterate:with slot-size = (+ (/ (- 360 current) c-counter)
                                     (* (/ (- 360 current) c-counter)
                                        (first (last (first i))))))
        (iterate:for slot in i)
        (push
         (list (first slot) (list current (cond ((= c-counter 0)
                                                 (+ current slot-size))
                                                (t
                                                 (+ current (- slot-size 0.1))))))
         slot-list)
        (setf current (+ current slot-size))
        (decf c-counter)))
    slot-list))

(defun create-wheel (data rsize)
  "Create the roulette wheel.

  Parameters
    data : list : List of candidates.
    rsize : int : Size of the representation.
  Return
    list (location (start stop)), where location is the data location and
         start/stop are the degree range (float) of the wheel."
  (reverse (align-slots (reverse (get-slots data rsize)) (length data))))

(defun evolution-roulette (population target limit fit-ratio)
  "Roulette Wheel algorithm.

  Parameters
    population : list
    target     :
    limit      : int : Max generation.
    fit-ratio  : float : (0.0 - 1.0)
  Return
    (nil, generation) if no result.
    (match, generation) if result."
  (iter
    (with generation = 1)
    (with pop = (fitness population target))
    (with wheel = (create-wheel pop r-size))
    (with match = (list (best-match pop fit-ratio) 1))
    (with match-next)
    (while (and (<= generation limit) (not (matchp (first match)))))
    (setf pop
          (fitness
           (flatten-list
            (determine-children
             (iter
               (for i from 0 to (1- (length pop)))
               (collect (nth (search-wheel wheel (random 360))
                             pop))))) target))

    (incf generation)
    (setf wheel (create-wheel pop r-size))

    (best-match-check pop fit-ratio generation match match-next)
    (finally (return match))))

(defun get-slots (data rsize)
  "Creates the initial slots.

  Parameters
    data : list : Candidate list
    rsize : int : Size of the representation.
  Return
    list : (pos fitness), where pos is the location of data and fitness is the
           fitness ratio."
  (remove-if #'(lambda (x) (null x))
            (iterate:iter
              (iterate:for step from 0 to (1+ rsize))
              (collect
                  (iterate:iter
                    (iterate:for pos from 0 to (1- (length data)))
                    (when (= (candidate-fitness (nth pos data)) (/ step rsize))
                      (collect (list pos (candidate-fitness (nth pos data))))))))))

(defun search-wheel (wheel degree)
  "Search the wheel for the desired slot.

  Parameters
    wheel : list
    degree : float
  Return
    int : Location of the candidate in the candidate list."
  (let* ((mid (floor (/ (length wheel) 2))))
    (cond
      ((= (length wheel) 1) (first (first wheel)))
      ((< (- (rational degree)
                  (rational (first (first (last (nth mid wheel))))))
          0.001)
       (search-wheel (subseq wheel 0 mid) degree))
      ((< (- (rational (first (last (first (last (last (nth mid wheel)))))))
             (rational degree))
          0.001)
       (search-wheel (subseq wheel mid (length wheel)) degree))
      (t
       (first (nth mid wheel))))))
