(in-package #:haptic-bag-translator)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load-proto-file (asdf:system-relative-pathname :haptic-bag-translator "Haptic.proto")))

(defmethod to-vicon-object ((record record))
  (make-instance
   'rst.devices.haptic:haptic
   :channels (map 'vector #'round (channels record))))

(defun convert (input output)
  (with-default-bag (bag output)
    (with-bag-channel (channel bag "/haptic/data" "rst.devices.Haptic")
      (with-file-descriptor (stream file input)
        (declare (ignore file))
        (do-records (record stream)
          (make-entry channel record (make-precise-timestamp (timestamp record))))))))

(defun print-help ()
  (let ((system (asdf:find-system :haptic-bag-translator)))
    (format T "~a v~a

~a

Usage:
haptic-bag-translator output input [input ...]

  output        Path to the resulting TIDE file
  input         Path to a Haptic CSV input file
                (can be a Zip-file containing the CSV file)

If multiple input files are specified, they are
processed into the bag in sequence on the same
channel.

Project URL:   ~a
Maintained by: ~a
Compiled against
  ~@<~{~{~36a ~a~}~^~@:_~}~@:>
"
            (asdf:component-name system)
            (asdf:component-version system)
            (asdf:system-description system)
            (asdf:system-homepage system)
            (asdf:system-maintainer system)
            (mapcar (lambda (dep)
                      (let ((system (asdf:find-system dep)))
                        (list (asdf:component-name system)
                              (asdf:component-version system))))
                    (asdf:system-depends-on system)))))

(defun main (&rest noop)
  (declare (ignore noop))
  (let ((args (uiop:command-line-arguments)))
    (case (length args)
      ((0 1)
       (print-help))
      (T
       (let ((args (mapcar #'uiop:parse-native-namestring args)))
         (convert (rest args) (first args)))))))
