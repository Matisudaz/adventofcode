(defpackage :adventcode.2024.day2
   (:use :cl :series)
   (:shadowing-import-from :series
                           :defun
                           :collect
                           :multiple-value-bind)
   (:export :part-1 :part-2))

(in-package :adventcode.2024.day2)

(defun read-levels (stream eof-error-p eof-value)
  (let ((line (read-line stream eof-error-p eof-value)))
    (if (eq line eof-value)
        eof-value
        (with-input-from-string (stream line)
          (collect 'list (scan-stream stream))))))

(defun read-input (input-pathname)
  (collect 'list (scan-file input-pathname #'read-levels)))
