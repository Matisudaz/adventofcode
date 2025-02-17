(in-package :adventcode.2024)

(defparameter *input-file-name* "input")

(defun input-pathname ()
  (make-pathname :name *input-file-name* :type "txt"))