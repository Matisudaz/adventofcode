;; token
;;
;;
;;
;;

;; parse

(defvar *stream* nil)

; 
(defstruct token "A token"
   (token-type nil)
   (token-value nil))

(defstruct mul-op "A mul operation"
   (left-value nil)
   (right-value nil))

;;
(defun ignore-rubbish ()
  "ignore rubbish"
  (loop for char = (read-char *stream* nil :eof)
        until (eq char :eof)
        when (or (char= char #\m)
                 (char= char #\u)
                 (char= char #\l)
                 (char= char #\()
                 (char= char #\))
                 (char= char #\,)
                 (digit-char-p char 10))
          do
             (unread-char char *stream*)
             (return-from ignore-rubbish nil)
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
  (read-char *stream*)
  (or
   (or (char= (peek-char nil *stream* nil :eof) #\u) (read-char *stream*))
   (or (char= (peek-char nil *stream* nil :eof) #\l) (read-char *stream*))))

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

(defun next-token ()
  "Get next token"
  (loop for char = (peek-char nil *stream* nil :eof)
        until (eq char :eof)
        do
           (cond
             ((char= char #\,) (return-from next-token (read-comma)))
             ((char= char #\() (return-from next-token (read-lpara)))
             ((char= char #\)) (return-from next-token (read-rpara)))
             ((digit-char-p char 10)
              (return-from next-token (make-token :token-type :number :token-value (read-number))))
             ((and (char= char #\m) (read-mul))
              (return-from next-token (make-token :token-type :mul :token-value "mul"))))
           (ignore-rubbish)))

(defun parse
  )

(defun parse-mul-op ()
  "parse mul operation"
  (let ((left-value 0)
        (right-value 0))
    (catch 'restart
      (get-mul-op)
      (catch 'continue
        (get-lpara)
        (setf left-value (get-number))
        (get-comma)
        (setf right-value (get-number))
        (get-rpara)))
    (make-mul-op :left-value left-value :right-value right-value)))

(defun get-mul-op ()
  (let ((token (next-token)))
    (unless (eq (token-token-type token) :mul)
      (throw 'restart))))

(defun get-lpara ()
  (let ((token (next-token)))
    (cond
      ((eq (token-token-type token) :lpara) nil)
      ((eq (token-token-type token) :mul)
       (throw 'continue))
      (t (throw 'restart)))))

(defun get-number ()
  (let ((token (next-token)))
    (cond
      ((eq (token-token-type token) :number) (token-token-value token))
      ((eq (token-token-type token) :mul)
       (throw 'continue))
      (t (throw 'restart)))))

(defun get-comma ()
  (let ((token (next-token)))
    (cond
      ((eq (token-token-type token) :comma) nil)
      ((eq (token-token-type token) :mul)
       (throw 'continue))
      (t (throw 'restart)))))

(defun get-rpara
  (let ((token (next-token)))
    (cond
      ((eq (token-token-type token) :rpara) nil)
      ((eq (token-token-type token) :mul)
       (throw 'continue))
      (t (throw 'restart)))))

(defun test-file (file)
  (with-open-file (stream file :direction :input
                               :if-does-not-exist :error)
    (let ((*stream* stream))
      (next-token))
    ))
