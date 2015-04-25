(in-package #:haptic-parser)

(defun call-with-input-stream (input function)
  (etypecase input
    (stream (funcall function input))
    (string (with-input-from-string (stream input)
              (funcall function stream)))
    (pathname (with-open-file (stream input :direction :input)
                (funcall function stream)))))

(defmacro with-input-stream ((streamvar input) &body body)
  `(call-with-input-stream ,input (lambda (,streamvar) ,@body)))

(defun eol-p (char)
  (and char
       (or (char= char #\Newline)
           (char= char #\Linefeed))))

(defun field-char-p (char)
  (and char
       (not (eol-p char))
       (not (char= char #\;))))

(defun peek (stream)
  (peek-char NIL stream NIL NIL))

(defun consume (stream)
  (read-char stream NIL NIL))

(defun skip-line (stream)
  (loop for char = (consume stream)
        until (eol-p char))
  (loop while (eol-p (peek stream))
        do (consume stream)))

(defun skip-empty (stream)
  (loop for char = (peek stream)
        while (find char #(#\Space #\Tab))
        do (consume stream)))

(defun read-field (input)
  (with-output-to-string (output)
    (loop for char = (peek input)
          while (field-char-p char)
          do (write-char (consume input) output))
    (when (eql #\; (peek input))
      (consume input))))

(defun map-line-fields (input function)
  (with-input-stream (input input)
    (loop for next = (peek input)
          while (and next (not (eol-p next)))
          do (funcall function (read-field input)))
    (loop while (eol-p (peek input))
          do (consume input))))

(defun map-file-lines (input function)
  (with-input-stream (input input)
    (loop while (peek input)
          do (skip-empty input)
             (case (peek input)
               (#\# (skip-line input))
               ((#\Newline #\Linefeed) (skip-line input))
               (T (funcall function input))))))

(defclass haptic-file ()
  ((file :initarg :file :accessor file)
   (records :initarg :records :accessor records))
  (:default-initargs
   :file (error "FILE required.")
   :records (make-array 0 :adjustable T :fill-pointer 0)))

(defmethod print-object ((file haptic-file) stream)
  (print-unreadable-object (file stream :type T)
    (format stream "~s ~d records"
            (enough-namestring (file file)) (length (records file)))))

(defclass record ()
  ((timestamp :initarg :timestamp :accessor timestamp)
   (channels :initarg :channels :accessor channels))
  (:default-initargs
   :timestamp (error "TIMESTAMP required.")
   :channels (make-array 0 :adjustable T :fill-pointer 0)))

(defmethod print-object ((record record) stream)
  (print-unreadable-object (record stream :type T)
    (format stream "~a ~d channels"
            (timestamp record) (length (channels record))))
  record)

(defun pusher (to &key (key #'identity))
  (lambda (item)
    (vector-push-extend (funcall key item) to)))

(defun timestamp-to-universal (timestamp)
  (+ (/ timestamp 1000)
     (load-time-value (encode-universal-time 0 0 0 1 1 1970 0))))

(defun process-records (input record-processor)
  (map-file-lines
   input
   (lambda (input)
     (let ((record (make-instance 'record :timestamp (timestamp-to-universal (parse-integer (read-field input))))))
       (map-line-fields input (pusher (channels record) :key #'parse-float:parse-float))
       (funcall record-processor record)))))

(defmacro do-records ((record input) &body body)
  `(process-records ,input (lambda (,record) ,@body)))

(defun parse-file (input)
  (let ((file (make-instance 'haptic-file :file (pathname input))))
    (process-records input (pusher (records file)))
    file))
