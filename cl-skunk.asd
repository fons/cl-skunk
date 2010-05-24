(in-package #:cl-user)

(defpackage #:cl-skunk-system (:use #:cl #:asdf))

(in-package #:cl-skunk-system)

(asdf:defsystem cl-skunk
  :name   "cl-skunk"
  :author "Fons Haffmans; fons.haffmans@gmail.com"
  :version "0.0.1"
  :licence "MIT"
  :description "skunk works with mongo a non-sql db"
  :depends-on (:cl-mongo)
  :serial t
  :components 
  ((:module "src"
    :serial t
    :components ((:file "package")
		 (:file "skunk")))
   (:static-file "README.md")
   (:static-file "COPYING")))











