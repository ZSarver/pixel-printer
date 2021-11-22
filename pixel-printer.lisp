;;;; pixel-printer.lisp

(in-package #:pixel-printer)

(opts:define-opts
  ( :name :help
    :description "print this help text"
    :short #\h
    :long "help"
   ))

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(format t "Hello, world!~%")
