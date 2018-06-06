;;;; evolution.lisp
;;;;
;;;; The main file for various evolutionary algorithms.
;;;;
(in-package #:evolution)

#|
  The chromosome-length is the length of the genotype.
|#
(defvar *chromosome-length* 0)

#|
  Flag indicating whether search should be considered global or local. If the
  search is defined at local (0) then fitness is normalized on the chromosomes;
  if search is defined as global (1) then fitness is normalized by overall
  fitness.
|#
(defvar *global-local-search* 0)

#|
  Potential match for the target criteria.
  genotype : Each part of the candidate is stored in a list.
  fitness  : The normalized value of the fitness of the genotype.
|#
(defstruct candidate
  (genotype nil :read-only t)
  (fitness 0))

(defun create-candidate (genotype &optional (fitness 0))
  "Create a candidate struct.

   Parameters
    genotype : : Dependent on user but should be coercable into a list.
    fitness  : int
   Return
    candidate struct"
  (if (typep genotype 'list)
      (make-candidate :genotype genotype :fitness fitness)
      (make-candidate :genotype (coerce genotype 'list) :fitness fitness)))

(defun cross-over (parent-1 parent-2)
  "Create new candidates using two old ones.

   Parameters
    parent-1 : candidate struct
    parent-2 : candidate struct
   Return
    list
   Error
    data-error : parent-1 and/or parent-2 are invalid."
  (derror (when (not (and parent-1 parent-2))) "Invalid parent given.")

  (let* ((point (+ 1 (random *chromosome-length*)))
         (gene-1 (split (candidate-genotype parent-1) point))
         (gene-2 (split (candidate-genotype parent-2) point)))
    (list (create-candidate (concatenate 'list (first gene-1)
                                               (first (last gene-2))))
          (create-candidate (concatenate 'list (first gene-2)
                                               (first (last gene-1)))))))

(defun cull-population (population fit-val)
 "Find all matches of candidates that have a given fit-val.

  Parameters
   population : list
   fit-val    : int
  Return
   list
  Error
   data-error if no population."
 (derror (when (null population)) "Nil encountered.")
 (remove-if-not #'(lambda (x) (> (candidate-fitness x) fit-val))
               population))

(defun determine-children (parents)
  "Take a list and determine new set of children from the list.

   Parameters
    parents : list
   Return
    list"
  (iter
    (for i from 0 to (1- (length parents)) by 2)
    (collect (mutation (cross-over (nth i parents)
                                   (nth (1+ i) parents))))))

(defun evolution (method rtype fitness-func chromosomes psize
                  &optional (limit 1000))
  "Main function.

   Parameters
    method       : keyword : Evolutionary algorithm
    rtype        : keyword : genotype type
    fitness-func : function
    chromosomes  : int : Number of individual pieces.
    psize  : int : Initial population size for candidate pool.
    limit  : int : Max number of iterations before giving up.
   Return
    candidate struct
   Modified
    *chromosome-length* is set.
   Error
    implementation-error is thrown if the feature is not implemented."
  (format *standard-output* "Search set to: ")
  (if (= *global-local-search* 0)
      (format *standard-output* "local~%")
      (format *standard-output* "global~%"))

  (format *standard-output* "Setting chromosome length to...~a~%" chromosomes)
  (set-chromosome-length chromosomes)

  (let ((init (genesis rtype psize)))
    (case method
      (:truncation (truncation init fitness-func limit))
      ;(:roulette (evolution-roulette init target limit fit-ratio))
      (t (ierror "Feature not implemented")))))

(defun generate-candidate (genotype)
  "Generate a new candidate.

   Parameters
    genotype : keyword
   Return
    candidate struct
   Error
    implementation-error"
  (case genotype
    (:ascii-caps
      (make-candidate :genotype
                        (generate-random-ascii-caps *chromosome-length*)
                      :fitness 0))
    (t (ierror "Feature currently not implemented."))))

(defun genesis (genotype population)
  "Creates the initial list of candidates.

   Parameters
    genotype   : keyword
    population : int
   Return
    list of candidate structs
   Error
    data-error : population < 2
                 *chromosome-length* <= 1
    implementation-error : Feature not implemented."
  (derror (when (< population 2)) "Population should be at least 2.")
  (derror (when (<= *chromosome-length* 1)) "Length of chromosomes not configured.")

  (let ((population-list ()))
    (case genotype
      (:ascii-caps (dotimes (i population)
                      (setf population-list
                            (append (list (generate-candidate genotype))
                             population-list))))
      (t (ierror "Feature currently not implemented.")))
    population-list))

#|
  Current method:
  1.  Iterate through each piece of the genotype.
  2.  At each piece check if mutation occurs.
 3.  If mutation occurs then changes to next available genotype.
|#
(defun mutation (c)
  "Randomly transform a candidate.

   Parameters
    c : candidate struct
        list
   Return
    candidate struct : If a candidate struct was passed in.
    list             : If a list was passed in."
    (cond ((null c) nil)
          ((typep c 'candidate)
            (let ((cr (candidate-genotype c)) (ncr ())
                  (mutation-rate (/ 1 *chromosome-length*)))
                 (iter
                   (for x from 0 to (1- (length cr)))
                   (cond ((< (- 50 (/ (random 20) 0.2)) 0)
                          (push (get-next-ascii-cap (nth x cr)) ncr))
                         (t (push (nth x cr) ncr))))

                 (cond ((eql cr ncr) cr)
                       (t (create-candidate (reverse ncr))))))
          (t (list (mutation (first c)) (mutation (rest c))))))

(defun ncalculate-population-fitness (population fitness)
  "Calculate the fitness of the entire population.

   Parameters
    population   : list
    fitness      : function : fitness-func
   Modified
    population : fitness slot is modified with the fitness values.
   Return
    list : Population list is returned."
  (iter
    (with total = 0)
    (for c in population)
    (let ((fit (funcall fitness c)))
      (if (= *global-local-search* 0)
          (progn
            (setf (candidate-fitness c) (/ fit *chromosome-length*)))
          (progn
            (setf (candidate-fitness c) fit)
            (setf total (+ total fit)))))
    (finally
      (when (= *global-local-search* 1)
        (map nil #'(lambda (x)
                    (setf (candidate-fitness x)
                          (if (> total 0)
                              (/ (candidate-fitness x) total)
                              0)))
            population))))
  population)
