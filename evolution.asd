;;;; evolution.asd

(asdf:defsystem #:evolution
  :description "A simple evolutionary algorithm framework."
  :author "Thomas Shaffer <tjshaffer21@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (:iterate)
  :components ((:file "src/package")
               (:file "src/helpers")
               (:file "src/evolution")
               (:file "src/truncate")
               (:file "src/roulette")))
