;;;;configuration.lisp
;;;;
;;;; Functions used to help configure the system.
(in-package #:evolution)

(defun set-chromosome-length (len)
  """Set the chromosome length of the genotype.

   Parameters
    len : int : Non-negative, non-zero.
   Modified
    *chromosome-length*"""
  (if (> len 0)
      (setf *chromosome-length* len)
      (restart-case (error 'data-error :text "*chromosome-length* too small.")
        (use-value (value) value))))

(defun set-global-local-search (search)
  """Modifies the search flag to indicate the search method.

   Parameters
    search : int : 0 or 1. 0 indicates a local search; 1 indicates a global
                   search.
   Modified
    *global-local-search*"""
  (if (or (= search 0) (= search 1))
      (setf *global-local-search* search)
      (restart-case (error 'data-error :text "search must be 0 or 1.")
        (use-value (value) value))))
