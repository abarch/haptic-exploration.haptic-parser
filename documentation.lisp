(in-package #:haptic-parser)

(defmacro setdocs (&body pairs)
  `(progn
     ,@(loop for (var doc) in pairs
             collect (destructuring-bind (var &optional (type 'function))
                         (if (listp var) var (list var))
                       `(setf (documentation ',var ',type) ,doc)))))

(setdocs
  (call-with-input-stream
   "Call FUNCTION with INPUT opened appropriately.

INPUT can be a STREAM, STRING, or PATHNAME.")
  
  (with-input-stream
    "Convenience macro around CALL-WITH-INPUT-STREAM.")
  
  (eol-p
   "Returns true if CHAR is a NEWLINE or LINEFEED.")
  
  (field-char-p
   "Returns true if CHAR is neither EOL-P, nor a semicolon.")
  
  (peek
   "Shorthand for (PEEK-CHAR NIL STREAM NIL NIL)")
  
  (consume
   "Shorthand for (READ-CHAR STREAM NIL NIL)")
  
  (skip-line
   "Consume characters until EOL-P turns true and subsequently false again.")
  
  (skip-empty
   "Consume SPACEs and TABs.")
  
  (read-field
   "Read a singular field from INPUT and return the read field.
Consumes the closing semicolon if it exists.")
  
  (map-line-fields
   "Map all fields on the current line in STREAM to FUNCTION.
Consumes the closing newline/s if they exist.")
  
  (map-file-lines
   "Map all valid lines from INPUT to FUNCTION.
This skips: Empty lines, lines prefixed by a #\#.")
  
  ((haptic-file type)
   "Container for a haptic csv data file.")
  
  (file
   "Returns the file that was read to construct this container.")
  
  (records
   "Returns the vector of RECORDs in the file.")
  
  ((record type)
   "Container for a record in a vicon file at a particular time.")
  
  (timestamp
   "Returns the UNIVERSAL-TIME timestamp at which this RECORD was taken.")
  
  (channels
   "Returns a vector of the actual data in the RECORD. Each index is a separate CHANNEL.")
  
  (pusher
   "Returns a function that accepts a single item to potentially transform by KEY and then VECTOR-PUSH-EXTEND onto TO.")
  
  (timestamp-to-universal
   "Translate the unix-TIMESTAMP integer in millisecond precision into a UNIVERSAL-TIME float in second precision.")
  
  (process-records
   "Read RECORDs from INPUT and map them to RECORD-PROCESSOR.")
  
  (do-records
    "Convenience macro around PROCESS-RECORDS.")
  
  (parse-file
   "Completely parse the INPUT pathname to a HAPTIC-FILE containing all records."))
