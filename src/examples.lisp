(in-package :cl-skunk-test)

(with-collection (collection "test") () 
		 ("title" 'title)
		 ("body"  'body) 
		 ("def"   'def)
		 ("ts"       ) 
		 ("ts2"    'nil) )

(with-collection (collection "test") ("_id" id) 
		 ("title" title)
		 ("body"  body) 
		 ("def"   def)
		 ("ts"       ) 
		 ("ts2"    nil) )

(with-collection (collection "test") (:all) 
		 ("title" "title 45")
		 ("body"  "body 45") 
		 ("def"   "def 45")
		 ("ts2"   4567889) )

(with-collection (collection "test") ( () (:all) )
		 ("title" "title 45")
		 ("body"  "body 45") 
		 ("def"   "def 45")
		 ("ts2"   4567889) )

(with-collection (collection "test") (:key "title" "body" "def")
		 ("title" "title 65")
		 ("body"  "body 5") 
		 ("def"   "def 500")
		 ("ts2"   4567889) )

(with-collection (collection "test") ("title" "65000")
		 ("title" "title 65")
		 ("body"  "body 5") 
		 ("def"   "def 500")
		 ("ts2"   4567889) )


(with-collection (collection "test") ( (:all) (:key "title") )
		 ("title" "title 65")
		 ("body"  "body 5") 
		 ("def"   "def 500")
		 ("ts2"   4567889) )


(defun plus+ (v)
  (let ((value v))
    (lambda ()
      (progn (setf value (+ value 1))
	     value))))

(with-collection (collection "test") ( (6)  )
		 ("title" "title 65")
		 ("body"  "body 5") 
		 ("def"   "def 500")
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

(with-collection (collection "test") ( (9)  )
		 ("title" "title 65")
		 ("body"  "body 5") 
		 ("def"   "def 500")
		 ("ts2"   (rb (mklst 10))))
