(in-package #:cl-user)
(defpackage #:haptic-parser
  (:use #:cl)
  (:export
   #:read-field
   #:map-line-fields
   #:map-file-lines
   
   #:haptic-file
   #:file
   #:records
   
   #:record
   #:timestamp
   #:channels
   
   #:process-records
   #:parse-file))
