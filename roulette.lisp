;;;; roulette.lisp
;;;; A roulette wheel used for the evolutionary framework.
;;;; TODO Possibly merge into evolution.lisp depending on number of functions.

(in-package #:evolution)

(defun create-wheel (data rsize)
  "Create the roulette wheel.
   It takes the candidate list DATA and the integer representation
   size RSIZE then creates a list of (location (start stop)) values
   where location represents the position in DATA and (start stop) the
   degree range (floats) of the wheel."
  (reverse (align-slots (reverse (get-slots data rsize)) (length data))))

(defun get-slots (data rsize)
  "Creates the initial slots.
   Given the candidate list DATA and the integer representation size RSIZE
   it returns a list lf (pos, fitness) values; where, pos is the location in 
   DATA and fitness is the fitness ratio."
  (remove-if #'(lambda (x) (null x))
             (iterate:iter
               (iterate:for step from 0 to (1+ rsize))
               (collect
                   (iterate:iter
                     (iterate:for pos from 0 to (1- (length data)))
                     (when (= (candidate-fitness (nth pos data)) (/ step rsize))
                       (collect (list pos (candidate-fitness (nth pos data))))))))))

;;; QUESTION Is there a means of removing the nested loop?
(defun align-slots (slots counter)
  "Aligns the slots so fitter candidates are larger.
   It takes the list of SLOTS ((pos, fit) ...) and a the length of the
   candidate list COUNTER. A new list of (location (start stop)) is returned."
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

;;; TODO More testing for errors.
;;; BUG The inherent issues with float comparison causes boundary issues as the
;;;     size of the population increases.
(defun search-wheel (wheel degree)
  "Search the list WHEEL for which slot the DEGREES (float) is located at and
   return the slots location value which is the location in the candidate list."
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
