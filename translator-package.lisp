(in-package #:cl-user)
(defpackage #:haptic-bag-translator
  (:use #:cl #:haptic-parser #:rsbag-helper)
  (:export
   #:with-haptic-channel
   #:convert
   #:main))
