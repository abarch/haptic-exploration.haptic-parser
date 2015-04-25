(in-package #:cl-user)
(asdf:defsystem haptic-bag-translator
  :name "Haptic-Bag-Translator"
  :version "1.1.0"
  :license "LGPL2.1"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Translating from the parsed HAPTIC file into the RSBag format."
  :homepage "https://github.com/Shinmera/haptic-parser/"
  :serial T
  :components ((:file "translator-package")
               (:file "bag-translator"))
  :depends-on (:rsbag-helper
               :cl-rsbag
               :cl-rsb-common
               :rsbag-tidelog
               :rsb-converter-protocol-buffer
               :haptic-parser)
  :build-operation asdf:program-op
  :build-pathname "haptic-bag-translator"
  :entry-point "haptic-bag-translator:main")
