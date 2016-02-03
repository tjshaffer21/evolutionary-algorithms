;;;; evolution.asd

(asdf:defsystem #:evolution
  :description "A simple evolutionary algorithm frame work."
  :author "Thomas Shaffer <tjshaffer21@gmail.com>"
  :license "Specify license here"
  :serial t
  :components ((:file "package")
               (:file "evolution")
               (:file "roulette")))

