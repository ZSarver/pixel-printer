(defpackage :pixel-printer
  (:use :cl :unix-options))

(in-package :pixel-printer)

(defun main (&rest args)
  (format t "Heyyyyy, macarena!"))

(with-cli-options '(() t)
                  ('(invert '(#\i "invert" nil "Inverts the height of each pixel, so that brighter pixels are taller and shorter are longer"))
                   )
  (main)
                  )
