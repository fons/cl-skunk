(in-package #:cl-user)

(defpackage #:cl-skunk
  (:use #:common-lisp #:cl-mongo )
  (:export
   ;;
   :collection
   :teardown
   :with-collection
   :map-skunk
   ))
