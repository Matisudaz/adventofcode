(defvar *stream* nil)
 
(defstruct token "A token"
   (token-type nil)
   (token-value nil))

(defstruct mul-op "A mul operation"
   (left-value nil)
   (right-value nil))

(defun read-rubbish ()
  "ignore rubbish"
  (loop for char = (read-char *stream* nil :eof)
        until (eq char :eof)
        when (or (char= char #\m)
                 (char= char #\()
                 (char= char #\))
                 (char= char #\,)
                 (digit-char-p char 10))
          do
             (unread-char char *stream*)
             (return-from read-rubbish (make-token :token-type :rubbish :token-value nil))
        ))

(defun read-comma ()
  "read comma"
  (make-token :token-type :comma :token-value (read-char *stream*)))

(defun read-lpara ()
  "read lpara"
  (make-token :token-type :lpara :token-value (read-char *stream*)))

(defun read-rpara ()
  "read rpara"
  (make-token :token-type :rpara :token-value (read-char *stream*)))

(defun read-mul ()
  (and
   (and (char= (peek-char nil *stream* nil :eof) #\u) (read-char *stream*))
   (and (char= (peek-char nil *stream* nil :eof) #\l) (read-char *stream*))))

(defun parse-number (char-list)
  (parse-integer (coerce char-list 'string)))

(defun read-number ()
  (let ((numbers nil))
    (loop for char = (read-char *stream* nil :eof)
          until (eq char :eof)
          when (digit-char-p char 10)
            do
               (setf numbers (cons char numbers))
          when (not (digit-char-p char 10))
            do
               (unread-char char *stream*)
               (return-from read-number (parse-number (reverse numbers)))
          finally (return-from read-number (parse-number (reverse numbers)))
          )))

(defun next-token-raw ()
  "Get next token"
  (loop for char = (peek-char nil *stream* nil :eof)
        until (eq char :eof)
        do
           (cond
             ((char= char #\,) (return-from next-token-raw (read-comma)))
             ((char= char #\() (return-from next-token-raw (read-lpara)))
             ((char= char #\)) (return-from next-token-raw (read-rpara)))
             ((digit-char-p char 10)
              (return-from next-token-raw (make-token :token-type :number :token-value (read-number))))
             ((and (char= char #\m) (read-char *stream*) (read-mul))
              (return-from next-token-raw (make-token :token-type :mul :token-value "mul")))
             (t (return-from next-token-raw (read-rubbish))))))

(defvar *temp-token* nil)

(defun peek-token ()
  (if *temp-token*
      *temp-token*
      (setf *temp-token* (next-token-raw))))

(defun next-token ()
  (setf *temp-token* (next-token-raw)))


(defun get-mul-op ()
  (let ((token (peek-token)))
    (cond
      ((eq token nil) (throw 'restart nil))
      ((eq (token-token-type token) :mul) (next-token))
      (t
       (next-token)
       (throw 'restart (parse-mul-op))))))

(defun get-lpara ()
  (let ((token (peek-token)))
    (cond
      ((eq token nil) (throw 'restart nil))
      ((eq (token-token-type token) :lpara) (next-token))
      (t (throw 'restart (parse-mul-op))))))

(defun get-number ()
  (let ((token (peek-token)))
    (cond
      ((eq token nil) (throw 'restart nil))
      ((eq (token-token-type token) :number) (next-token) (token-token-value token))
      (t (throw 'restart (parse-mul-op))))))

(defun get-comma ()
  (let ((token (peek-token)))
    (cond
      ((eq token nil) (throw 'restart nil))
      ((eq (token-token-type token) :comma) (next-token))
      (t (throw 'restart (parse-mul-op))))))

(defun get-rpara ()
  (let ((token (peek-token)))
    (cond
      ((eq token nil) (throw 'restart nil))
      ((eq (token-token-type token) :rpara) (next-token))
      (t (throw 'restart (parse-mul-op))))))

(defun parse-mul-op ()
  "parse mul operation"
  (let ((left-value 0)
        (right-value 0))
    (catch 'restart
      (get-mul-op)
      (get-lpara)
      (setf left-value (get-number))
      (get-comma)
      (setf right-value (get-number))
      (get-rpara)
      (make-mul-op :left-value left-value :right-value right-value))))

(defun get-result (mul-op)
  (* (mul-op-left-value mul-op)
     (mul-op-right-value mul-op)))

(defun add-result-of-multi-op (file)
  (with-open-file (stream file :direction :input
                               :if-does-not-exist :error)
    (let ((*stream* stream))
      (reduce #'+
              (loop for op = (parse-mul-op)
                    until (eq op nil)
                    collect (get-result op))))))
