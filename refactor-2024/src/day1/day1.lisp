(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :series)
  (series::install :pkg :series))

(defpackage :adventcode.2024.day1
  (:use :cl :series)
  (:shadowing-import-from :series
                          :defun
                          :collect
                          :multiple-value-bind)
  (:export :part-1 :part-2))

(in-package :adventcode.2024.day1)

(provide :adventcode.2024.day1)

(defparameter *input-file-name* "input")

(defun input-pathname ()
  (make-pathname :name *input-file-name* :type "txt"))

(defun read-input (input-pathname)
  (multiple-value-bind (left-column right-column)
      (chunk 2 2 (scan-file input-pathname))
    (values (collect 'list left-column)
            (collect 'list right-column))))

(defun part-1 ()
  (multiple-value-bind (left-column right-column)
      (read-input (input-pathname))
    (collect-sum
     (#Mabs
      (#M-
       (scan (sort left-column #'<))
       (scan (sort right-column #'<)))))))

(defun part-2 ()
  (multiple-value-bind (left-column right-column)
      (read-input (input-pathname))
    (collect-sum
     (#M(lambda (item) (* item (count item right-column)))
        (scan left-column)))))
