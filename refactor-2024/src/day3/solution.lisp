(in-package :adventcode.2024.day3)

(defparameter *CURRENT-DIR* "day3")

(defun read-input (input-pathname)
   (read-file-into-string input-pathname))

(defun part-1 ()
  (let ((answer 0))
    (cl-ppcre:do-register-groups (whole (#'parse-integer left) (#'parse-integer right))
        (*mul-instruction* (read-input (input-pathname)))
      (declare (ignore whole))
      (incf answer (* left right)))
    answer))

(defun part-2 ()
  (let ((input (read-input (input-pathname))))
    
    ))