;;;;
;;;; evolution.lisp
;;;; A framework for evolutionary algorithms
;;;; QUESTION Should single character representations be allowed?
;;;; TODO Add other representation types.
;;;;

(in-package #:evolution)
(load "roulette")

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
;;;  method        - SYMBOL used to select evolutionary method.
;;;    :truncation - Truncation Selection method
;;;  rtype         - SYMBOL used to selection the representation type to use.
;;;    :ascii-caps - Capital ASCII characters A-Z
;;;  target        - Target to be found.
;;;  rsize         - Size of the representation
;;;  psize         - Population size
;;;  limit         - Max times to run algorithm.
;;;  fit-ratio     - How close we can be to the target [0.0 - 1.0]
;;; Side Effects
;;;  r-size is modified
;;; Returns
;;;  List containing resulting candidate structure and generation.
(defun evolution (method rtype target rsize psize &optional (limit 1000) (fit-ratio 1.0))
  "Main method to run the evolution framework."
  (let ((init (genesis rtype rsize psize)))
    (cond ((eq :truncation method)
           (evolution-truncation init target limit fit-ratio))
          ((eq :roulette method)
           (evolution-roulette init target limit fit-ratio))
          (t (error 'implementation-error :text "Feature currently not implemented.")))))

;;; Parameters
;;;    r-type        - SYMBOL used to select which representation type to use.
;;;      :ascii-caps - Capital ASCII characters A-Z.
;;;    rsize         - The representation size
;;;    p-size        - The population size.
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
  (when (not (and parent-1 parent-2)) (error 'data-error :text "Invalid parent given."))
  (when (<= r-size 1)
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

(defun all-matches (population fit-ratio)
  "Find all matches in the POPULATION within the given FIT-RATIO."
  (when (null population) (error 'data-error :text "Nil encountered."))
  (remove-if-not #'(lambda (x) (>= (candidate-fitness x) fit-ratio)) population))

;;; Algorithm
;;;   1.  Find all matches >= fit-ratio
;;;   1.1 If no matches then return nil
;;;   1.2 If 1 match return match
;;;   2.  If more than one match increase fit-ratio and repeat
;;; TODO A way to dynamically choose step size?
(defun best-match (population fit-ratio)
  "Find the best match candidate."
  (let* ((choices (all-matches population fit-ratio))
         (best (first choices))
         (step 0.1))
    (when (> (length choices) 1)
      (let ((best-next (best-match choices (+ fit-ratio step))))
        (unless (null best-next)
          (when (< (candidate-fitness best) (candidate-fitness best-next))
            (setf best best-next)))))
    best))

;;; Used by GENESIS for population initialization.
;;; TODO Rewrite to make more generic.
(defun create-candidate (r-type r-size)
  (cond ((eq :ascii-caps r-type)
         (make-candidate :representation (random-ascii-caps r-size) :fitness 0))
        (t (error 'implementation-error :text "Feature currently not implemented."))))

(defun determine-children (parents)
  (let ((children '()))
    (loop for y from 0 to (- (length parents) 1) by 2 do
         (push (cross-over (nth y parents) (nth (+ y 1) parents)) children))
    children))

(defun flatten-list (l)
  "Flatten nested lists back into a list of structs."
  (cond ((null l) nil)
        ((typep l 'candidate) (list l))
        (t (concatenate 'list (flatten-list (first l)) (flatten-list (rest l))))))
(defun get-next-ascii-cap (current)
  (let ((x -1) (y 0))
    (loop while (and (< x 0) (< y (length +ascii-caps+))) do
         (cond ((string= current (nth y +ascii-caps+)) (setf x y)))
         (incf y))
    (nth (mod (+ x 1) 26) +ascii-caps+)))

;;; Used by code outside this file to limit messing with variable.
;;; TODO May or may not be remved later.
(defun get-r-size () r-size)

(defun matchp (c)
  "Check if candidate matches target"
  (unless (null c) (when (= (candidate-fitness c) 1.0) t)))

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

(defun random-ascii-caps (length)
  (let ((lst ()))
    (dotimes (i length)
      (setf lst (append (list (nth (random 26) +ascii-caps+)) lst))) lst))

(defun split-list (l at)
  "Split given list L at point AT."
  (cond ((null l) '())
        ((null (rest l)) (list (car l)))
        ((> at (length l)) '())
        ((<= at 0) '())
        (t (cons (subseq l 0 at) (cons (subseq l at (length l)) nil)))))

;;;; The following code is divided into sections based on methods used.
;;;; TODO Decided if selection functions is significant enough to separate into a new
;;;;      file, or if the selection functions should remain here.

;;;; Start Truncation Selecton

;;; Parameters
;;;   population - Population seed.
;;;   target     - Target goal
;;;   limit      - Max number of generations to run.
;;;   fit-ratio  - Accuracy of results (0.0 - 1.0)
;;; Returns
;;;   If no solution is found then returns a list of (nil, generation);
;;;   If a match is found then returns a list of (match, generation)
;;;   where, match is a CANDIDATE structure that meets the criteria.
;;;
;;; Example: (evolution:evolution-truncation population "HELLO" 10000 0.5)
(defun evolution-truncation (population target limit fit-ratio)
  "Evolution using Truncation Selection."
  (let* ((generation 1)
         (pop (fitness population target))
         (match (list (best-match pop fit-ratio) 1))
         (match-next nil))
    (loop while (and (<= generation limit) (not (matchp (first match)))) do
         (incf generation)
         (setf pop
               (fitness (flatten-list
                         (determine-children (et-determine-parents pop
                                                                   fit-ratio)))
                        target))

         ;; TODO How often is this used? Possibly factor out into a metho.
         ; Save initial match unless a better match appears.
         (cond ((null (first match))
                (setf match (list (best-match pop fit-ratio) generation)))
               (t
                (setf match-next (best-match pop fit-ratio))))
         
         (when (not (or (null (first match)) (null match-next)))
           (cond ((matchp match-next)
                  (setf match (list match-next generation)))
                 ((> (candidate-fitness match-next) (candidate-fitness (first match)))
                  (setf match (list match-next generation))))))
    match))

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

;;;; End Truncation Selection

;;;; Start Roulette Selection

;;; Parameters
;;;  population - Population seed
;;;  target     - Candidate being searched for.
;;;  fit-ratio  - How close to the result we can be.
(defun evolution-roulette (population target limit fit-ratio)
  "Evolution using a roulette wheel."
  (let* ((generation 1)
         (pop (fitness population target))
         (wheel (create-wheel pop))
         (match (list (best-match pop fit-ratio) 1))
         (match-next nil)
         (parents '()))
    (loop while (and (<= generation limit) (not (matchp (first match)))) do
       ;; TODO Check looping with collect method as an alternative
         (dotimes (i (length pop))
           (push (nth (search-wheel wheel (random 360)) pop) parents))

         (incf generation)
         (setf pop (fitness (flatten-list (determine-children parents)) target))
         
         (setf parents '())
         (setf wheel (create-wheel pop))

         ;; TODO See other. Second usage is here.
         ;; Save initial match unless better is found.
         (cond ((null (first match))
                (setf match (list (best-match pop fit-ratio) generation)))
               (t
                (setf match-next (best-match pop fit-ratio))))

         (when (not (or (null (first match)) (null match-next)))
           (cond ((matchp match-next)
                  (setf match (list match-next generation)))
                 ((> (candidate-fitness match-next) (candidate-fitness (first match)))
                  (setf match (list match-next generation))))))
    match))

;;; End Roulette

;;; Testing Code
(defun test (target rsize psize limit fit)
  (let* ((test-t (time (evolution :truncation :ascii-caps target rsize psize limit fit)))
         (test-r (time (evolution :roulette :ascii-caps target rsize psize limit fit))))
    (format t "~a || ~a || ~a~%----~%" target limit fit)
    (format t "Truncation:~%")
    (print-candidate (first test-t))
    (format t "Generation: ~a~%~%" (first (last test-t)))
    
    (format t "Roulette:~%")
    (print-candidate (first test-r))
    (format t "Generation: ~a~%~%" (first (last test-r)))))
;;; End Testing Code
