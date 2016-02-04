;;;; roulette.lisp
;;;; A roulette wheel used for the evolutionary framework.
;;;; TODO Possibly merge into evolution.lisp depending on number of functions.

(in-package #:evolution)

;;; Parameters
;;;    data  - The data used to create the slots on the wheel.
;;;    rsize - The rsize for the data.
;;; Returns
;;;    List containing lists of (location, (start, stop))
(defun create-wheel (data rsize)
  "Create the roulette wheel."
  (reverse (align-slots (reverse (get-slots data rsize)) (length data))))

;;; Parameters
;;;   data  - The data we are using for the slots
;;;   rsize - The representation size of each piece of data.
;;; Return
;;;   List of list (pos, fitness)
(defun get-slots (data rsize)
  (remove-if #'(lambda (x) (null x))
             (iterate:iter
               (iterate:for step from 0 to (1+ rsize))
               (collect
                   (iterate:iter
                     (iterate:for pos from 0 to (1- (length data)))
                     (when (= (candidate-fitness (nth pos data)) (/ step rsize))
                       (collect (list pos (candidate-fitness (nth pos data))))))))))

;;; Parameters
;;;    slots   - The list containing the slot and fitness values for the data.
;;;    counter - Counter used for loop (Length of the data).
;;; Returns
;;;    List of the list containing (location (start,stop)) for each slot.
;;; QUESTION Current slot is of form (((x,y) (z,w)) ((a,b))). Is there a better means to manage this without nested loops? 
(defun align-slots (slots counter)
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
         (list (first slot) (list current (+ current (cond ((= (1- c-counter) 0)
                                                            slot-size)
                                                           (t
                                                            (- slot-size 0.1))))))
               slot-list)
        (setf current (+ current slot-size))
        (decf c-counter)))
    slot-list))

;;; Search the wheel and return the location of the degree.
;;; TODO More testing for errors.
(defun search-wheel (wheel degree)
  "Search the wheel and return the location of the degree."
  (let* ((mid (floor (/ (length wheel) 2))))
    (cond
      ((= (length wheel) 1) (first (first wheel)))
      ((> (- (rational degree)
                  (rational (first (first (last (nth mid wheel))))))
          0.001)
       (search-wheel (subseq wheel 0 mid) degree))
      ((> (- (rational (first (last (first (last (last (nth mid wheel)))))))
             (rational degree))
          0.001)
       (search-wheel (subseq wheel (1+ mid) (length wheel)) degree))
      (t
       (first (nth mid wheel))))))
