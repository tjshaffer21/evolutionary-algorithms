;;;; helpers.lisp
;;;;
;;;; Collection of macros and fuctions used to help in handling the evolutionary
;;;; algorithms, but they are not related to the algorithms themselves.
;;;;
(in-package #:evolution)

(defconstant +ascii-caps+ (list #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L
                                #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X
                                #\Y #\Z))

(defun char- (cha chb)
  "The difference between two chars.

   Parameters
    cha : standard-char
    chb : standard-char
   Return
    int"
  (- (char-int cha) (char-int chb)))

(defun flatten-list (l)
  "Flatten a nested list into a list of structs.

   Parameters
    l : list
   Returns
    list
    nil if the list is null."
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (concatenate 'list (flatten-list (first l)) (flatten-list (rest l))))))

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
    current : standard-char
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

(defun max-candidate (population)
  (iter
    (for i in population)
    (finding i maximizing (candidate-fitness i))))

(defun print-candidate (c)
  "Print the candidate information.

   Parameters
    c : candidate struct
   Output
    standard-output"
  (unless (null c)
    (format *standard-output* "Genotype: ~a~T~TFitness: ~a~%"
      (candidate-genotype c) (candidate-fitness c))))

(defun print-population (p)
  "Print each candidate in the population.

   Parameters
    p : list
   Output
    standard-output"
  (unless (null p)
    (iter
      (for c in p)
      (print-candidate c))))

(defun split (input at)
  "Split list at given point.

   List is split such that the new lists are: [0, at], [at+1, end]

   Parameters
    l  : list
    at : int
   Return
    list : A list containing the two new lists.
            If at == (length list) then second value is nil.
           Nil
            An empty list is passed.
            If at <= 0 || at  > length l."
  (cond ((null input) '())
        ((null (rest input)) (list (car input)))
        ((> at (length input)) '())
        ((<= at 0) '())
        (t (cons (subseq input 0 at)
                 (cons (subseq input at (length input)) nil)))))
