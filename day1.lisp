;; helper function
(defun read-numbers-from-string (str)
  "Returns a list of number from a string splitted by empty space."
  (let ((result nil)
        (start 0))
    (loop named read-numbers
          do
          (multiple-value-bind (number index) (read-from-string str nil nil :start start)
            (when (eq number nil)
              (return-from read-numbers (nreverse result)))
            (push number result)
            (setf start index)))))

(defun insert-list (number lst)
  "Insert a number into a sorted number lst, keep sorted"
  (cond
    ((null lst) (list number))
    ((> (car lst) number) (cons number lst))
    (t (cons (car lst) (insert-list number (cdr lst))))))

(defun read-two-number-list-from-stream (stream)
  "Returns two list from a stream."
  (let ((lst1 nil)
        (lst2 nil))
    (loop for line = (read-line stream nil 'eof)
          until (eq line 'eof)
          do
             (let* ((two-number-lst (read-numbers-from-string line))
                    (first-number (car two-number-lst))
                    (second-number (cadr two-number-lst)))
               (setf lst1 (insert-list first-number lst1))
               (setf lst2 (insert-list second-number lst2))))
    (values lst1 lst2)))

(defun read-two-number-list-from-file (file)
  (with-open-file (stream file :direction :input)
    (read-two-number-list-from-stream stream)))

(defun calc-total-distance-between-two-list (file)
  (multiple-value-bind (lst1 lst2) (read-two-number-list-from-file file)
    (do* ((n1 lst1 (cdr n1))
          (n2 lst2 (cdr n2))
          (result 0))
         ((or (null n1) (null n2)) result)
      (incf result (abs (- (car n1) (car n2)))))))


(defun calc-similarity-score-between-two-list (file)
  (multiple-value-bind (lst1 lst2) (read-two-number-list-from-file file)
    (let ((hash-table (make-hash-table))
          (result 0))
      (loop for item in lst2
            do (incf (gethash item hash-table 0)))
      (loop for item in lst1
            do (incf result (* item (gethash item hash-table 0))))
      result)))
