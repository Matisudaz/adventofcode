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


(defun next-token ()
  "Get next token"
  (loop for char = (read-char *stream* nil :eof)
        until (eq char :eof)
        do
           (cond
             ((char= char #\,) (return-from next-token (make-token :token-type :comma :token-value #\,)))
             ((char= char #\() (return-from next-token (make-token :token-type :lpara :token-value #\()))
             ((char= char #\)) (return-from next-token (make-token :token-type :rpara :token-value #\))))
             ((digit-char-p char 10)
              (return-from next-token (make-token :token-type :number :token-value (read-number char))))
             ((and (char= char #\m)
                   (or (char= (read-char *stream* nil :eof) #\u)
                       (un)) (char= (read-char *stream*) #\u)
                   (char= (read-char *stream*) #\l))
              (return-from next-token (make-token :token-type :mul :token-value "mul"))))
           (ignore-rubbish)))


(defun read-mul ()
  (let ((char-1 (read-char )))))

(defun parse-number (char-list)
  (parse-integer (coerce char-list 'string)))

(defun number-to-list (number)
  (if (null number)
      number
      (list number)))

(defun read-number (&optional (initial-number nil))
  (let ((numbers (number-to-list initial-number)))
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

(defun test-file (file)
  (with-open-file (stream file :direction :input
                          :if-does-not-exist :error)
    (let ((*stream* stream))
      (next-token))
    ))
