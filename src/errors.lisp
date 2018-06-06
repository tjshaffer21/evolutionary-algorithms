;;;;errors.Lisp
;;;;
;;;; Error handling code.
(in-package #:evolution)

(define-condition implementation-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
             (format stream "~@(~A~)" (text condition)))))

(define-condition data-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (condition stream)
             (format stream "~@(~A~)" (text condition)))))

(defmacro derror (check msg) `(,@check (error 'data-error :text ,msg)))

(defmacro ierror (msg) `(error 'implementation-error :text ,msg))
