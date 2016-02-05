;;;;
;;;; evolution.lisp
;;;; A framework for evolutionary algorithms
;;;; TODO Add other representation types.
;;;; TODO Think about having r-size be something other than a global.

(in-package #:evolution)

(defstruct candidate
  (representation nil :read-only t)
  (fitness 0))

(defconstant +ascii-caps+ (list #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L
                                #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X
                                #\Y #\Z))

(defvar r-size 0) ; Should only be set by (genesis) while read by other functions.

(define-condition implementation-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
             (format stream "~@(~A~)." (text condition)))))

(define-condition data-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
             (format stream "~@(~A~)." (text condition)))))

(defmacro derror (check msg) `(,@check (error 'data-error :text ,msg)))

(defmacro ierror (msg) `(error 'implementation-error :text ,msg))

(defmacro best-match-check (population fit-ratio generation match match-next)
  "Macro take the POPULATION list, FIT-RATIO (0.0 - 1.0) float, current GENERATION,
   the MATCH candidate, and the MATCH-NEXT candidate then checks if there is a
   match of the given fit-ratio or better. MATCH is used for the current match;
   while MATCH-NEXT is used to check if there is a better match. Both MATCH and
   MATCH-NEXT are modified by the macro."
  `(progn
     (cond ((null (first ,match))
            (setf ,match (list (best-match ,population ,fit-ratio) ,generation)))
           (t
            (setf ,match-next (best-match ,population ,fit-ratio))))
  
     (when (not (or (null (first ,match)) (null ,match-next)))
       (cond ((matchp ,match-next)
              (setf ,match (list ,match-next ,generation)))
             ((> (candidate-fitness ,match-next) (candidate-fitness (first ,match)))
              (setf ,match (list ,match-next ,generation)))))))

;;; Available representation types:
;;;  ascii-caps
(defun evolution (method rtype target rsize psize &optional (limit 1000) (fit-ratio 1.0))
  "Runs the evolution framework.
   It takes a symbol METHOD to determine which method of evolution to run.
   The uses the symbol RTYPE and the representation size RSIZE and population
   size PSIZE to create an initial list of candidates. Then the specified 
   METHOD is run to search for the TARGET within the generation LIMIT 
   and minimum FIT-RATIO. Since genesis is called the global r-size is modified.
   The resulting solution is either no result (nil, generation) or (match, generation)."
  (let ((init (genesis rtype rsize psize)))
    (cond ((eq :truncation method)
           (evolution-truncation init target limit fit-ratio))
          ((eq :roulette method)
           (evolution-roulette init target limit fit-ratio))
          (t (ierror "Feature currently not implemented.")))))

;;; Available representation types:
;;;  ascii-caps
(defun genesis (r-type rsize p-size)
  "Creates the initial list of candidates.
   It takes a symbol R-TYPE to create representations of integer RSIZE to
   create a list of P-SIZE candidates. Global r-size is set to RSIZE, and
   the list of candidates is returned."
  (derror (when (< p-size 2)) "P-SIZE should be at least 2.")
  (derror (when (<= rsize 1)) "RSIZE must be greater than 1.")

  (setf r-size rsize)

  (let ((population ()))
    (cond ((eq  :ascii-caps r-type)
           (dotimes (i p-size)
             (setf population
                   (append (list (generate-candidate r-type r-size)) population))))
          (t (ierror "Feature currently not implemented.")))
    population))

(defun fitness (population target)
  "For the entire POPULATION list of candidates it calculates how close each
   candidate is to the given TARGET"
  (let ((n-population ()))
    (cond ((null population) ())
          ((typep population 'candidate)
           (push (create-candidate
                  (candidate-representation population)
                  (/ (count t
                            (mapcar #'eql
                                    (candidate-representation population)
                                    (coerce target 'list))) r-size))
                 n-population))
          (t (setf n-population
                   (append (concatenate 'list (fitness (first population) target)
                                        (fitness (rest population) target)) n-population))))
    n-population))

(defun cross-over (parent-1 parent-2)
  "Create two new candidates by crossing the representations of PARENT-1 and
   PARENT-2 returning a list with both new candidates."
  (derror (when (not (and parent-1 parent-2))) "Invalid parent given.")

  (let* ((point (+ 1 (random r-size)))
         (gene-1 (split-list (candidate-representation parent-1) point))
         (gene-2 (split-list (candidate-representation parent-2) point)))
    (list (mutation (create-candidate
                     (concatenate 'list (first gene-1) (first (last gene-2)))))
          (mutation (create-candidate
                     (concatenate 'list (first gene-2) (first (last gene-1))))))))

;;; Current method:
;;;   1.  Iterate through each piece of the representation.
;;;   2.  At each piece check if mutation occurs.
;;;   3.  If mutation occurs then changes to next available representation.
;;; TODO Better mutation algorithm
(defun mutation (c)
  "Takes a candidates C and evaluates whether its representation undergoes
   a change."
  (cond ((null c) ())
        ((typep c 'candidate)
          (let ((cr (candidate-representation c)) (ncr ()))
            (iterate:iter
              (iterate:for x from 0 to (1- (length cr)))
              (cond ((< (- 50 (/ (random 20) 0.2)) 0)
                     (push (get-next-ascii-cap (nth x cr)) ncr))
                    (t (push (nth x cr) ncr))))

            (cond ((eql cr ncr) cr)
                  (t (create-candidate (reverse ncr))))))
        (t (concatenate 'list (mutation (first c)) (mutation (rest c))))))

(defun all-matches (population fit-ratio)
  "Find all matches in the candidate POPULATION list within the given
   FIT-RATIO (0.0 - 1.0)."
  (derror (when (null population)) "Nil encountered.")
  (remove-if-not #'(lambda (x) (>= (candidate-fitness x) fit-ratio)) population))

;;; TODO A way to dynamically choose step size?
(defun best-match (population fit-ratio)
  "Find the best match candidate by examining the candidate list
   POPULATION with the FIT-RATIO (0.0 - 1.0)"
  (let* ((choices (all-matches population fit-ratio))
         (best (first choices))
         (step 0.1))
    (when (> (length choices) 1)
      (let ((best-next (best-match choices (+ fit-ratio step))))
        (unless (null best-next)
          (when (< (candidate-fitness best) (candidate-fitness best-next))
            (setf best best-next)))))
    best))

(defun generate-candidate (r-type rsize)
  "Create a new candidate structure of given R-TYPE and RSIZE."
  (cond ((eq :ascii-caps r-type)
         (make-candidate :representation (random-ascii-caps rsize) :fitness 0))
        (t (ierror "Feature currently not implemented."))))

(defun create-candidate (representation &optional (fitness 0))
  "Create a candidate with given REPRESENTATION and FITNESS."
  (make-candidate :representation representation :fitness fitness))
  
(defun determine-children (parents)
  "Take a list of PARENTS and create a list of children."
    (iterate:iter (iterate:for i from 0 to (1- (length parents)) by 2)
        (iterate:collect (cross-over (nth i parents) (nth (1+ i) parents)))))

(defun flatten-list (l)
  "Flatten nested list L back into a list of structs."
  (cond ((null l) nil)
        ((typep l 'candidate) (list l))
        (t (concatenate 'list (flatten-list (first l)) (flatten-list (rest l))))))

;;; TODO Rewrite loop to iterate.
(defun get-next-ascii-cap (current)
  "Finds the next character in the +ascii-caps++ list using CURRENT
   character as point of reference."
  (let ((x -1) (y 0))
    (loop while (and (< x 0) (< y (length +ascii-caps+))) do
         (cond ((string= current (nth y +ascii-caps+)) (setf x y)))
         (incf y))
    (nth (mod (+ x 1) 26) +ascii-caps+)))

(defun matchp (c)
  "Check if candidate C matches target"
  (unless (null c) (when (= (candidate-fitness c) 1.0) t)))

(defun print-candidate (c)
  "Print the representation and fitness of the given candidate."
  (when (null c) (return-from print-candidate nil))
  (format t "~a ~T(~a)~%" (candidate-representation c) (candidate-fitness c)))

(defun print-population (p)
  "Print the representations and fitness for all candidates in the given population P."
  (when (null p) (return-from print-population nil))
  (dotimes (i (length p))
    (format t "~2<~d~>. ~T~a ~T(~a)~%" (+ i 1)
            (candidate-representation (nth i p))
            (candidate-fitness (nth i p)))))

(defun random-ascii-caps (length)
  "Generate a list of capital ascii chars (A-Z) of size LENGTH."
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
;;; Example: (evolution:evolution-truncation population "HELLO" 10000 0.5)
(defun evolution-truncation (population target limit fit-ratio)
  "Evolution using Truncation Selection.
   The candidate list POPULATION is used to search for TARGET
   within the generation LIMIT and a minimum of FIT-RATIO (0.0 - 1.0).
   If a result is not found the (nil, generation) is returned; while a
   match returns (match, generation)."
  (iterate:iter
    (iterate:with generation = 1)
    (iterate:with pop = (fitness population target))
    (iterate:with match = (list (best-match pop fit-ratio) 1))
    (iterate:with match-next)
    (iterate:while (and (<= generation limit) (not (matchp (first match)))))
    (incf generation)

    (setf pop
          (fitness (flatten-list
                    (determine-children
                     (et-determine-parents pop fit-ratio)))
                   target))

    (best-match-check pop fit-ratio generation match match-next)
    (iterate:finally (return match))))

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

;;;; End Truncation Selection

;;;; Start Roulette Selection
(defun evolution-roulette (population target limit fit-ratio)
  "Evolution using a roulette wheel.
   The candidate list POPULATION is used to search for TARGET
   within the generation LIMIT and a minimum of FIT-RATIO (0.0 - 1.0).
   If a result is not found the (nil, generation) is returned; while a
   match returns (match, generation)."
  (iterate:iter
    (iterate:with generation = 1)
    (iterate:with pop = (fitness population target))
    (iterate:with wheel = (create-wheel pop r-size))
    (iterate:with match = (list (best-match pop fit-ratio) 1))
    (iterate:with match-next)   
    (iterate:while (and (<= generation limit) (not (matchp (first match)))))
    (setf pop
          (fitness (flatten-list (determine-children
                                  (iterate:iter
                                    (iterate:for i from 0 to (1- (length pop)))
                                    (iterate:collect (nth
                                                      (search-wheel wheel
                                                                    (random 360))
                                                      pop))))) target))

    (incf generation)
    (setf wheel (create-wheel pop r-size))

    (best-match-check pop fit-ratio generation match match-next)
    (iterate:finally (return match))))

;;; End Roulette

;;; Testing Code
(defun test (target rsize psize limit fit)
  (let* ((test-t (time (evolution :truncation :ascii-caps target rsize psize limit fit)))
         (test-r (time (evolution :roulette :ascii-caps target rsize psize limit fit))))
    (format t "~%~a || ~a || ~a~%----~%" target limit fit)
    (format t "Truncation:~%")
    (print-candidate (first test-t))
    (format t "Generation: ~a~%~%" (first (last test-t)))
    
    (format t "Roulette:~%")
    (print-candidate (first test-r))
    (format t "Generation: ~a~%~%" (first (last test-r)))))

(defun test-wheel (target rtype rsize psize)
  (let* ((population (fitness (genesis rtype rsize psize) target))
         (wheel (create-wheel population r-size)))
    (print-population population)
    (format t "----~%")
    (iterate:iter (iterate:for i iterate:in wheel)
                  (format t "~a~%" i))
    (format t "----~%")
    (format t " 54   : ~a~%" (search-wheel wheel 54))
    (format t "----~%")
    (format t "  0   : ~a~%" (search-wheel wheel 0))
    (format t "----~%")
    (format t " 75.9 : ~a~%" (search-wheel wheel 75.9))
    (format t "----~%")
    (format t "184.23: ~a~%" (search-wheel wheel 184.23))
    (format t "----~%")
    (format t "250   : ~a~%" (search-wheel wheel 250))
    (format t "----~%")
    (format t "269.9 : ~a~%" (search-wheel wheel 269.9))
    (format t "----~%")
    (format t "270   : ~a~%" (search-wheel wheel 270))
    (format t "----~%")
    (format t "360   : ~a~%" (search-wheel wheel 360))
    ))
;;; End Testing Code
