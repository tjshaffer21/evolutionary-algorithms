;;;;evolution-examples.asd
(asdf:defsystem #:evolution-examples
  :description "Examples for evolutionary algorithms."
  :author "Thomas Shaffer <tjshaffer21@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (#:evolution)
  :components ((:file "examples/package")
               (:file "examples/examples")))
