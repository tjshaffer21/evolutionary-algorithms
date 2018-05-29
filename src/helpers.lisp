;;;; helpers.lisp
;;;;
;;;; Collection of macros and fuctions used to help in handling the evolutionary
;;;; algorithms, but they are not related to the algorithms themselves.
;;;;

(in-package #:evolution)

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

(defun flatten-list (l)
  "Flatten a nested list into a list of structs.

   Parameters
    l : list : A nested list of candidate structs
   Returns
    A list of candidate structures.
    nil if the list is null."
  (cond ((null l) nil)
        ((typep l 'candidate) (list l))
        (t (concatenate 'list (flatten-list (first l)) (flatten-list (rest l))))))

(defun split-list (l at)
  "Split list at given point.

   List is split such that the new lists are: [0, at], [at+1, end]

   Parameters
    l  : list
    at : int  : Split the list at given index.
   Return
    list : A list containing the two new lists.
            If at == (length list) then second value is nil.
           Nil
            An empty list is passed.
            If at <= 0 || at  > length l."
  (cond ((null l) '())
        ((null (rest l)) (list (car l)))
        ((> at (length l)) '())
        ((<= at 0) '())
        (t (cons (subseq l 0 at) (cons (subseq l at (length l)) nil)))))
