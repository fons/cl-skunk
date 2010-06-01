(in-package :cl-skunk-test)

(with-collection "examples" () 
		 ("title" "The Title")
		 ("body"   "The Body of the document") 
		 ("def"    "some sort of definition ?"))


(with-collection "examples" (:all) 
		 ("title" "title 45")
		 ("body"  "body 45") 
		 ("def"   "def 45")
		 ("ts2"   4567889) )

(with-collection "examples" ( (1) (:all) )
		 ("title" "title 83")
		 ("body"  "new body of text. ") 
		 ("def"   "a new definition")
		 ("ts2"   nil) )

(with-collection "examples" (:key "title")
		 ("ts2"   4567889) )

(with-collection "example" ("title" "65000")
		 ("title" "title 65")
		 ("body"  "body 5") 
		 ("def"   "def 500")
		 ("ts2"   4567889) )


(defun plus+ (v)
  (let ((value v))
    (lambda ()
      (progn (setf value (+ value 1))
	     value))))

(with-collection "examples" ( (6)  )
		 ("doc" "closure") )
		 ("ts2"   (plus+ 0)) )

(defun rb(l)
  (let ((lst l))
    (lambda()
      (prog1 
	  (car lst)
	(setf lst (cdr lst))))))

(defun mklst(n &optional (accum nil))
  (if (zerop n) 
      accum
      (mklst (decf n) (cons n accum) )))



(defun doc-gen()
  (list (list (list "field1" "value1") (list "field2" 34) (list "field3" 78.89))
	(list (list "field1x" "value1x") (list "field2x" 3489) (list "field3x" 7890.89))))

(with-collection (collection "test") () (car (doc-gen)))


;; create one unique document where the user supplies the _id.
;; obvioulsy you can create only one document; this is a very strict specification..
;;
(with-collection "example" (:id)
		 ("title" "title 65")
		 ("body"  "body 5") 
		 ("def"   "def 500")
		 ("_id"   9000))