(in-package #:cl-user)
(defpackage #:haptic-bag-translator
  (:use #:cl #:haptic-parser #:rsbag-helper)
  (:export
   #:convert
   #:main))
