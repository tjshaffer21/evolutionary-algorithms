;;;;
;;;; evolution.lisp
;;;; A framework for evolutionary algorithms
;;;; TODO Should single character representations be allowed?
;;;;

(in-package #:evolution)

(defstruct candidate
  (representation nil :read-only t)
  (fitness 0))

(defconstant +ascii-caps+ (list #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L
                                #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X
                                #\Y #\Z))

(defvar r-size 0) ; Should only be set by (genesis) while read by other functions.

(define-condition implementation-error (error) ((text :initarg :text :reader text)))

(define-condition data-error (error) ((text :initarg :text :reader text)))

;;; Parameters
;;;    r-type - SYMBOL used to select which representation type to use.
;;;        :ascii-caps - Capital ASCII characters A-Z.
;;;    rsize  - The representation size
;;;    p-size - The population size.
;;; Side Effects
;;;    Sets the global r-size variable.
;;; Returns
;;;    List of candidate structures that contain the initial population.
(defun genesis (r-type rsize p-size)
  "Set up the initial population for the framework."
  (when (< p-size 2) (error 'data-error :text "POPULATION-SIZE should be at least 2"))
  (when (< rsize 1) (error 'data-error :text "REPRESENATATION-SIZE cannot be less than 1."))

  (setf r-size rsize)

  (let ((population ()))
    (cond ((eq  :ascii-caps r-type)
           (dotimes (i p-size)
             (setf population
                   (append (list (create-candidate r-type r-size)) population))))
          (t (error 'implementation-error :text "Feature currently not implemented.")))
    population))

;;; Parameters
;;;   population - Population seed.
;;;   target     - Target goal
;;;   limit      - Max number of generations to run.
;;;   fit-ratio  - Accuracy of results (0.0 - 1.0)
;;; Returns
;;;   If no solution is found then returns a list of (nil, generation);
;;;   If a match is found then returns a list of (match, generation)
;;;   where, match is a CANDIDATE structure that meets the criteria.
(defun evolution-truncation (population target limit fit-ratio)
  "Evolution using Truncation Selection."
  (when (null population)
    (error 'data-error :text "Population not found.")
    (return-from evolution-truncation nil))

  (let* ((generation 1)
         (pop (fitness population target))
         (match (match? pop fit-ratio))
         (parents ())
         (children ()))
    (loop while (and (<= generation limit) (null match)) do
         (setf parents (et-determine-parents pop fit-ratio))

         (loop for y from 0 to (- (length parents) 1) by 2 do
              (push (cross-over (nth y parents) (nth (+ y 1) parents)) children))

         (incf generation)
         (setf pop (fitness (flatten-list children) target))
         (print-population (all-matches pop 0.2))
         (setf match (match? pop fit-ratio))
         (setf children ()))
    (list match generation)))

(defun et-determine-parents (population fit-ratio)
  "Determine parents using Truncation Selection."
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

(defun fitness (population target)
  "Calculate the FITNESS values for the given POPULATION"
  (let ((n-population ()))
    (cond ((null population) ())
          ((typep population 'candidate)
           (push (make-candidate
                  :representation (candidate-representation population)
                  :fitness (/ (count t
                                     (mapcar #'eql
                                             (candidate-representation population)
                                             (coerce target 'list))) r-size))
                 n-population))
          (t (setf n-population
                   (append (concatenate 'list (fitness (first population) target)
                                        (fitness (rest population) target)) n-population))))
    n-population))

(defun cross-over (parent-1 parent-2)
  "Create new candidates by crossing two other candidates."
  (case (not (and parent-1 parent-2)) (error 'data-error :text "Invalid parent given."))
  (case (<= r-size 1)
    (error 'implementation-error
           :text "Current single character representations are not available."))

  (let* ((point (+ 1 (random r-size)))
         (gene-1 (split-list (candidate-representation parent-1) point))
         (gene-2 (split-list (candidate-representation parent-2) point)))
    (list (mutation (make-candidate
                     :representation (concatenate 'list (first gene-1)
                                                  (first (last gene-2)))
                     :fitness 0))
          (mutation (make-candidate
                     :representation (concatenate 'list (first gene-2)
                                                  (first (last gene-1)))
                     :fitness 0)))))

;;; Current method:
;;;   1.  Iterate through each piece of the representation.
;;;   2.  At each piece check if mutation occurs.
;;;   3.  If mutation occurs then changes to next available representation.
;;; TODO Better mutation algorithm
;;; TODO Refactor to use other representation types.
(defun mutation (c)
  "Determine if given candidate is changed."
  (cond ((null c) ())
        ((typep c 'candidate)
          (let ((cr (candidate-representation c)) (ncr ()))
            (loop for x from 0 to (- (length cr) 1) by 1 do
                 (cond ((< (- 50 (/ (random 20) 0.2)) 0)
                        (push (get-next-ascii-cap (nth x cr)) ncr))
                       (t (push (nth x cr) ncr))))

            (cond ((eql cr ncr) cr)
                  (t (make-candidate :representation (reverse ncr) :fitness 0)))))
        (t (concatenate 'list (mutation (first c)) (mutation (rest c))))))

(defun split-list (l at)
  "Split given list L at point AT."
  (cond ((null l) '())
        ((null (rest l)) (list (car l)))
        ((> at (length l)) '())
        ((<= at 0) '())
        (t (cons (subseq l 0 at) (cons (subseq l at (length l)) nil)))))

(defun flatten-list (l)
  (cond ((null l) nil)
        ((typep l 'candidate) (list l))
        (t (concatenate 'list (flatten-list (first l)) (flatten-list (rest l))))))

(defun all-matches (population fit-ratio)
  "Find all matches in the POPULATION within the given FIT-RATIO."
  (when (null population) (error 'data-error :text "Nil encountered."))
  (remove-if-not #'(lambda (x) (>= (candidate-fitness x) fit-ratio)) population))
  
(defun best-match (population fit-ratio) )

;;; Check the population for a candidate within the fitness ratio.
;;; Parameters
;;;   population  - List of candidate solutions.
;;;   error-ratio - Value from [0.0, 1.0] to determine the fitness range.
;;; TODO Either delete if unnecessary or refactor to make sure best match is found.
(defun match? (population error-ratio)
  (let ((match nil))
    (loop for x from 0 to (- (length population) 1) by 1 do
         (cond ((null (nth x population)) (error 'data-error :text "Nil encountered"))
               ((>= (candidate-fitness (nth x population)) error-ratio)
                (setf match (nth x population)))))
    match))

;;; Used by GENESIS for population initialization.
(defun create-candidate (r-type r-size)
  (cond ((eq :ascii-caps r-type)
         (make-candidate :representation (random-ascii-caps r-size) :fitness 0))
        (t (error 'implementation-error :text "Feature currently not implemented."))))

(defun random-ascii-caps (length)
  (let ((lst ()))
    (dotimes (i length)
      (setf lst (append (list (nth (random 26) +ascii-caps+)) lst))) lst))

(defun get-next-ascii-cap (current)
  (let ((x -1) (y 0))
    (loop while (and (< x 0) (< y (length +ascii-caps+))) do
         (cond ((string= current (nth y +ascii-caps+)) (setf x y)))
         (incf y))
    (nth (mod (+ x 1) 26) +ascii-caps+)))

(defun print-candidate (c)
  "Print the representation and fitness of the given candidate."
  (when (null c) (return-from print-candidate nil))
  (format t "~a ~T(~a)~%" (candidate-representation c) (candidate-fitness c)))

(defun print-population (p)
  "Print the representations and fitness for all candidates in the given population."
  (when (null p) (return-from print-population nil))
  (dotimes (i (length p))
    (format t "~2<~d~>. ~T~a ~T(~a)~%" (+ i 1)
            (candidate-representation (nth i p))
            (candidate-fitness (nth i p)))))
