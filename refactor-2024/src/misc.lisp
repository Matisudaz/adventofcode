(in-package :adventcode.2024)

(defparameter *input-file-name* "input")

(defun current-dir ()
  (let ((symbol (find-symbol "*CURRENT-DIR*" *package*)))
    (if symbol
        (symbol-value symbol)
        "")))

(defun input-pathname ()
  (let  ((system-dir (asdf:system-source-directory "adventcode.2024"))
         (input-file (make-pathname :directory `(:relative "src" ,(current-dir))
                                    :name *input-file-name*
                                    :type "txt")))
    (merge-pathnames input-file system-dir)))

(defun remove-one-element (lst)
  (loop for i from 0 below (length lst)
        collect (append (subseq lst 0 i)
                       (subseq lst (1+ i)))))
