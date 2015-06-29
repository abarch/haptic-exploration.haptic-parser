(in-package #:cl-user)
(asdf:defsystem haptic-parser
  :name "Haptic-Parser"
  :version "1.0.0"
  :license "LGPL2.1"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A parser for HAPTIC CSV files."
  :homepage "https://github.com/Shinmera/haptic-parser/"
  :serial T
  :components ((:file "parser-package")
               (:file "haptic-parser")
               (:file "documentation"))
  :depends-on (:parse-float
               :local-time))
