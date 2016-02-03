;;;; roulette.lisp
;;;; A roulette wheel used for the evolutionary framework.
;;;; TODO Possibly merge into evolution.lisp depending on number of functions.

(in-package #:evolution)

(defun create-wheel (data)
  (reverse (align-slots data (length data) (get-r-size) (get-r-size))))

;;; WARNING
;;;   1. This code block is written using functions from evolution.lisp.
;;;      It may not be much of an issue for Lisp as it is with other languages.
;;;   2. (get-r-size) should be used rather than calling r-size directly.
;;;   3. The code is rather dirtly written, but TODO should be to see if there
;;;      is a better method to do the work.
;;; END WARNING
;;; Parameters
;;;    data      - Data used for the slot alignment. List of candidate structs.
;;;    counter   - Length of data.
;;;    step      - Starting step point. Assumed to be decrementing.
;;;    prev-step - Previous step point. Initially set to be equivialent to step.
;;;    degrees   - Number of degrees remaining.
;;; Returns
;;;    List of a list of degrees ((start, stop) (start, stop) ...)
(defun align-slots (data counter step prev-step &optional (current 0))
  "Align slots so better candidates are larger than worse candidates."
  (unless (< step 0)
    ;; QUESTION Is there a function similar to remove-if-not but just with the searching?
    (when (> counter 0) ; If counter = 0 then data then alignment is complete.
      (let* ((slot (remove-if-not #'(lambda (x)
                                      (and (>= (candidate-fitness x)
                                               (/ step (get-r-size)))
                                           (< (candidate-fitness x)
                                              (/ prev-step (get-r-size))))) data))
             (slot-list '())
             (slot-size (+ (/ (- 360 current) counter)
                           (* (/ (- 360 current) counter) (/ step (get-r-size))))))
        (cond ((and (> (length slot) 1) (>= step 5)) (print-population slot)(write "What to do when matches are found?"))
              ((> (length slot) 0) 
               (progn
                 (dotimes (i (length slot))
                   (push
                    (list current
                          (cond ((= current 0) (- slot-size 0.1))
                                (t (+ current
                                        ; Last value should be 360.
                                      (cond ((and (= i (- (length slot) 1)) 
                                                  (= step 0))
                                             slot-size)
                                            (t (- slot-size 0.1)))))))
                    slot-list)
                   (setf current (+ current slot-size))))))
        (append (align-slots data (- counter (length slot)) (- step 1)
                             step current) slot-list)))))
  
;;; Search the wheel and return the location of the degree.
;;; BUG Comparision chooses smaller case for float comp.
;;;   Example: Degree - 146; Wheel - (648/5 145.95718)
(defun search-wheel (wheel degree)
  (let* ((mid (floor (/ (length wheel) 2))))
    (cond
      ((= (length wheel) 1) mid)
      ((< (rational degree) (rational (first (nth mid wheel))))
       (search-wheel (subseq wheel 0 mid) degree))
      ((> (rational degree) (rational (first (last (nth mid wheel)))))
       (search-wheel (subseq wheel mid (length wheel)) degree))
      (t mid))))
