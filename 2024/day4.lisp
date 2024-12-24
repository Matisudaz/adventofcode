(defvar *words-map* nil)

(defvar *search-word* "XMAS")

(defun get-pos-by-direction (row col &key (step 1) direction)
  (case direction
    (:left       (list row (- col step)))
    (:right      (list row (+ col step)))
    (:up         (list (- row step) col))
    (:down       (list (+ row step) col))
    (:left-up    (list (- row step) (- col step)))
    (:left-down  (list (+ row step) (- col step)))
    (:right-up   (list (- row step) (+ col step)))
    (:right-down (list (+ row step) (+ col step)))))

(defun get-possible-pos (row col len &key direction)
  (loop for i from 0 to (- len 1)
        collect (get-pos-by-direction row col :step i :direction direction)))

(defun get-row (pos)
  (car pos))

(defun get-col (pos)
  (cadr pos))

(defun my-format (msg)
  (format t msg)
  t)

(defun match (row col)
  (and
   (char= (aref *words-map* row col) #\A)
   (or
    (and
     (char= (apply #'aref *words-map* (get-pos-by-direction row col :direction :left-up)) #\M)
     (char= (apply #'aref *words-map* (get-pos-by-direction row col :direction :right-down)) #\S))
    (and
     (char= (apply #'aref *words-map* (get-pos-by-direction row col :direction :left-up)) #\S)
     (char= (apply #'aref *words-map* (get-pos-by-direction row col :direction :right-down)) #\M)))
   (or
    (and
     (char= (apply #'aref *words-map* (get-pos-by-direction row col :direction :left-down)) #\M)
     (char= (apply #'aref *words-map* (get-pos-by-direction row col :direction :right-up)) #\S))
    (and
     (char= (apply #'aref *words-map* (get-pos-by-direction row col :direction :left-down)) #\S)
     (char= (apply #'aref *words-map* (get-pos-by-direction row col :direction :right-up)) #\M)))))

(defun safe-match (row col)
  (handler-case (match row col)
    (sb-int:invalid-array-index-error (condition)
      (declare (ignore condition))
      nil)))

(defun find-word (possible-pos)
  (do*
   ((i 0 (+ i 1))
    (pos possible-pos (cdr pos)))
   ((null pos) 1)
    (if (not (char= (aref *words-map* (get-row (car pos)) (get-col (car pos))) (aref *search-word* i)))
        (return-from find-word 0))))

(defun safe-find-word (possible-pos)
  (handler-case (find-word possible-pos)
    (sb-int:invalid-array-index-error (condition)
      (declare (ignore condition))
      0)))

(defun try-find-word (row col)
  (let ((len (length *search-word*)))
    (reduce #'+
            (mapcar #'safe-find-word
                    (mapcar (lambda (direction)
                              (get-possible-pos row col len :direction direction))
                            (list :left :right :up :down :left-up :left-down :right-up :right-down))))))

(defun file-to-2d-char-array (file)
  (with-open-file (stream file :direction :input)
    (let* ((lines (loop for line = (read-line stream nil)
                        while line
                        collect line))
           (rows (length lines))
           (cols (if (null lines) 0 (length (first lines))))
           (2d-array (make-array (list rows cols) :element-type 'character)))
      (dotimes (i rows)
        (dotimes (j cols)
          (setf (aref 2d-array i j) (char (nth i lines) j))))
      2d-array)))


(defun find-all (file)
  (let ((result 0)
        (*words-map* (file-to-2d-char-array file)))
    (loop for i below (array-dimension *words-map* 0) do 
      (loop for j below (array-dimension *words-map* 1) do 
        (incf result (try-find-word i j))))
    result))

(defun find-all-xmas (file)
  (let ((result 0)
        (*words-map* (file-to-2d-char-array file)))
    (loop for i below (array-dimension *words-map* 0) do
      (loop for j below (array-dimension *words-map* 1)
        when (safe-match i j)
          do (incf result)))
    result))
