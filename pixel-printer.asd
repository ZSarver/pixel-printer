;;;; pixel-printer.asd

(asdf:defsystem #:pixel-printer
  :description "Describe pixel-printer here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:unix-opts)
  :components ((:file "package")
               (:file "pixel-printer")))
