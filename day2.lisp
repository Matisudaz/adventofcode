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

(defmacro create-check-safe-report (&rest args &key increase decrease)
  (declare (ignore args))
  `(lambda (prev cur)
     (and ,(cond
             (increase '(< prev cur))
             (decrease '(> prev cur)))
          (< (abs (- prev cur)) 4))))

(defun safe-report-p (report)
  "Check a report is safe or not."
  (labels ((local-helper (r prev p)
             (if (null r)
                 t
                 (and (funcall p prev (car r)) (local-helper (cdr r) (car r) p)))))
    (cond
      ((or (null report) (null (cdr report))) t)
      ((> (car report) (cadr report))
       (local-helper (cdr report) (car report) (create-check-safe-report :decrease t)))
      (t (local-helper (cdr report) (car report) (create-check-safe-report :increase t))))))


(defun read-numbers-list-from-stream (stream)
  "Returns two list from a stream."
  (loop for line = (read-line stream nil 'eof)
        until (eq line 'eof)
        collect (read-numbers-from-string line)))

(defun read-numbers-list-from-file (file)
  (with-open-file (stream file :direction :input)
    (read-numbers-list-from-stream stream)))

(defun calc-safe-report-from-file (file)
  (let ((result 0))
    (loop for lst in (read-numbers-list-from-file file)
          when (safe-report-p lst)
            do (incf result))
    result))
