;;;; package.lisp

(defpackage #:evolution
  (:use #:cl #:iterate)
  (:export #:evolution
           #:set-global-local-search
           #:print-candidate
           #:print-population))
