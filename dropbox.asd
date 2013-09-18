(defpackage :dropbox-asd
  (:use :cl :asdf))

(in-package :dropbox-asd)

(defsystem :dropbox
  :name "dropbox"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "Dropbox REST API for LispWorks."
  :serial t
  :components ((:file "dropbox"))
  :depends-on ("http" "json"))
