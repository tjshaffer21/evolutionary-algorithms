;;;; evolution.lisp
;;;;
;;;; The main file for various evolutionary algorithms.
;;;;

(in-package #:evolution)

(defstruct candidate
  (representation nil :read-only t)
  (fitness 0))

(defconstant +ascii-caps+ (list #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L
                                #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X
                                #\Y #\Z))

(defvar r-size 0) ; Should only be set by (genesis) while read by other functions.

(defmacro best-match-check (population fit-ratio generation match match-next)
  "Find the best match for the given criteria.

  Parameters
    population : list
    fit-ratio  : float : The ratio should be between 0.0 and 1.0
    generation : int   : The current generation.
    match      :       : The curret match.
    match-next :       : Used to check if there is a better match.
  Modified
    match
    match-next"
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

(defun evolution (method rtype target rsize psize
                  &optional (limit 1000) (fit-ratio 1.0))
  "Main function.

  Parameters
    method : symbol : Indicates the evolution algorithm to be used.
    rtype  : symbol : Indicates the representation type.
    target :        : The result that is being looked for.
    rsize  : int    : Size of /target/
    psize  : int    : Initial population size for candidate pool.
    limit  : int    : Max number of iterations before giving up.
    fit-ratio : float : Error tolerance between 0.0 and 1.0.
  Return
    candidate struct
  Error
    ierror is thrown if the feature is not implemented."
  (let ((init (genesis rtype rsize psize)))
    (case method
      (:truncation (evolution-truncation init target limit fit-ratio))
      (:roulette (evolution-roulette init target limit fit-ratio))
      (t (ierror "Feature not implemented")))))

(defun genesis (r-type rsize p-size)
  "Creates the initial list of candidates.

  Parameters
    r-type : symbol
    rsize  : int
    psize  : int
  Modified
    r-size global
  Return
    list of candidate structs
  Error
    data-error : p-size < 2
                 r-size <= 1
    implementation-error : Feature not implemented."
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
  "Calculate the fitness for the current population.

  Parameters
    population : list : Current population list.
    target     :      : The target
  Return
    list"
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
  "Create new candidates using two old ones.

  Due to the potential mutation, the split is unverifiable.

  Parameters
    parent-1 : candidate struct
    parent-2 : candidate struct
  Return
    list
  Error
    data-error : parent-1 and/or parent-2 are invalid."
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
(defun mutation (c)
  "Transform a candidate.

  Parameters
    c : candidate struct
        list
  Return
    candidate struct : If a candidate struct was passed in.
    list : If a list was passed in."
  (cond ((null c) ())
        ((typep c 'candidate)
          (let ((cr (candidate-representation c)) (ncr ()))
            (iter
              (for x from 0 to (1- (length cr)))
              (cond ((< (- 50 (/ (random 20) 0.2)) 0)
                     (push (get-next-ascii-cap (nth x cr)) ncr))
                    (t (push (nth x cr) ncr))))

            (cond ((eql cr ncr) cr)
                  (t (create-candidate (reverse ncr))))))
        (t (concatenate 'list (mutation (first c)) (mutation (rest c))))))

(defun all-matches (population fit-ratio)
  "Find all matches of candidates that have a given fit-ratio.

  Parameters
    population : list
    fit-ratio  : float
  Return
    list of all matches.
  Error
    derror if no population."
  (derror (when (null population)) "Nil encountered.")
  (remove-if-not #'(lambda (x) (>= (candidate-fitness x) fit-ratio)) population))

(defun best-match (population fit-ratio)
  "Find the best match candidate.

  Parameters
    population : list
    fit-ratio : float
  Return
    candidate struct"
  (let* ((choices (all-matches population fit-ratio))
         (best (first choices))
         (step 0.1))
    (when (> (length choices) 1)
      (let ((best-next (best-match choices (+ fit-ratio step))))
        (unless (null best-next)
          (when (< (candidate-fitness best) (candidate-fitness best-next))
            (setf best best-next)))))
    best))

(defun create-candidate (representation &optional (fitness 0))
  "Create a candidate struct.

  Parameters
    representation : The value used to represent a candidate. The type is
                     determined by the user's choice of rtype.
    fitness        : float : 0.0 - 1.0. The fitness value of the candidate.
  Return
    candidate struct"
  (make-candidate :representation representation :fitness fitness))

(defun determine-children (parents)
  "Take a list and determine new set of children from the list.

  Parameters
    parents : list
  Return
    list"
  (iter (for i from 0 to (1- (length parents)) by 2)
    (collect (cross-over (nth i parents) (nth (1+ i) parents)))))

(defun generate-candidate (r-type rsize)
  "Generate a new candidate.

  Parameters
    r-type : symbol
    rsize  : int
  Return
    candidate struct
  Error
    implementation-error"
  "Create a new candidate structure of given R-TYPE and RSIZE."
  (cond ((eq :ascii-caps r-type)
         (make-candidate :representation (generate-random-ascii-caps rsize) :fitness 0))
        (t (ierror "Feature currently not implemented."))))

(defun generate-random-ascii-caps (length)
  "Generate list of random capital ascii chars.

  Parameters
    length : int : Length of the list of random characters.
  Return
    list"
  (let ((lst ()))
    (dotimes (i length)
      (setf lst (append (list (nth (random 26) +ascii-caps+)) lst))) lst))

(defun get-next-ascii-cap (current)
  "Find the next ascii character.

  Parameters
    current : standrd-char
  Return
    standard-char"
  (cond ((typep current 'standard-char)
         (let ((x -1))
            (iter
              (with y = 0)
              (while (and (< x 0) (< y (length +ascii-caps+))))
              (when (char= current (nth y +ascii-caps+)) (setf x y))
              (incf y))
            (nth (mod (1+ x) 26) +ascii-caps+)))
        (t #\A)))

(defun matchp (c)
  "Check if candidate matches the target.

  Parameters
    c : candidate struct
  Return
    t if a match;
    nil if not a match."
  (if (typep c 'candidate)
      (when (= (candidate-fitness c) 1.0) t)
      nil))
