(in-package :cl-user)

(defpackage :adventcode.2024
  (:use :cl)
  (:export :input-pathname))

(defpackage :adventcode.2024.day1
  (:use :cl :series :adventcode.2024)
  (:shadowing-import-from :series
                          :defun
                          :collect
                          :multiple-value-bind)
  (:export :part-1 :part-2))

(defpackage :adventcode.2024.day2
  (:use :cl :series :adventcode.2024)
  (:shadowing-import-from :series
                          :defun
                          :collect
                          :multiple-value-bind)
  (:export :part-1 :part-2))