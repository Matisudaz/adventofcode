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


(defun safe-report-p (report &optional (tolerate-bad-level 0))
  "Check a report is safe or not."
  (labels
      ((local-helper-1 (report process-list prev p tolerate-bad-level)
         (cond
           ((null report) t)
           ((funcall p prev (car report))
            (local-helper-1
             (cdr report)
             (cons prev process-list)
             (car report)
             p
             tolerate-bad-level))
           ((> tolerate-bad-level 0)
            (cond
              ((null process-list)
               (or
                (local-helper-2 report (- tolerate-bad-level 1))
                (local-helper-2 (cons prev (cdr report)) (- tolerate-bad-level 1))))
              ((null (cdr process-list))
               (or
                (local-helper-2 (cons prev report) (- tolerate-bad-level 1))
                (local-helper-2 (cons (car process-list) report) (- tolerate-bad-level 1))
                (local-helper-2 (append process-list (list prev) (cdr report)) (- tolerate-bad-level 1))))
              (t
               (or
                (local-helper-1
                 report
                 (cdr process-list)
                 (car process-list)
                 p
                 (- tolerate-bad-level 1))
                (local-helper-1
                 (cdr report)
                 process-list
                 prev
                 p
                 (- tolerate-bad-level 1))))))
           (t nil)))
       (local-helper-2 (report tolerate-bad-level)
         (cond
           ((or (null report) (null (cdr report))) t)
           (t (local-helper-1
               (cdr report)
               nil
               (car report)
               (if (> (car report) (cadr report))
                   (create-check-safe-report :decrease t)
                   (create-check-safe-report :increase t))
               tolerate-bad-level)))))
    (local-helper-2 report tolerate-bad-level)))

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

(defun calc-safe-report-with-tolerate-single-bad-level-from-file (file)
  (let ((result 0))
    (loop for lst in (read-numbers-list-from-file file)
          when (safe-report-p lst 1)
            do (incf result))
    result))
