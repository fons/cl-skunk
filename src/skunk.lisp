(in-package :cl-skunk)

(defclass skunk-collection ()
  ((name      :initarg  :name    :reader name)
   (mongodb   :initarg  :mongodb :reader mongodb)
   (documents :initform (make-hash-table :test #'equal) :accessor documents)))

(defun read-documents(collection)
  (progn
    (dolist (d (docs (db.find (name collection) :all :mongo (mongodb collection))))
      (setf (gethash (doc-id d) (documents collection)) d)))
  collection)

(defgeneric collection (name &key) )

(defmethod  collection ( (name string) &key (host "localhost") (port cl-mongo::+MONGO-PORT+))
  (handler-case 
      (labels ((connection (name)
		 (let ((name (format nil "skunk.~A" name)))
		   (if (mongo-registered name)
		       (mongo :name name)
		       (cl-mongo::make-mongo :host host :db "skunk" :port port :name name)))))
	(read-documents (make-instance 'skunk-collection :name name :mongodb  (connection name))))
    (error(c)
      (format t "error : ~A" c)
      (mongo-close (format nil "skunk.~A" name)))))

(defgeneric close-collection (collection) )

(defmethod close-collection ( (name string ))
  (mongo-close (format nil "skunk.~A" name)))

(defclass skunk-document ()
  ((key :initarg :key :reader key)
   (dirty-flag :initform nil :accessor dirty-flag)
   (mongo-document :initarg :mongo-document :accessor mongo-document)))

(defgeneric document ( collection document) )
;;-> string is by value
;;-> skunk-collection is by reference

;;--> if the key is _id, use the _id obviously (which will always be unique..
;;--> if NOT, on add you need to check the db for existence of this key value
;;--> document adds to collection automatically..

(defmethod document+ ( (collection skunk-collection) (document document) )
  (setf (gethash (doc-id document) (documents collection)) document)
  (db.save (name collection) document :mongo (mongodb collection)))
  
(defmethod document+ ( (collection string) (document document) )
  (document+ (collection collection) document))

(defmethod document+ ( (collection t)  (documents cons) )
  (mapcar (lambda (document) (document+ collection  document )) documents))

(defmethod document+ ( (collection t) (document (eql nil))))

(defun process-document (docs collection args)
  (labels ((mutate-doc (doc)
	     (labels ((exec-value (value)
		      (cond ((functionp value) (funcall value))
			    (t value)))
		      (mutate-fields (d)
			(let ((key   (car d))
			      (value (cadr d)))
			  (if value
			      (add-element (exec-value key) (exec-value value) doc)
			      (rm-element  (exec-value key) doc)))))
	       (progn
		 (mapcar #'mutate-fields args)
		 doc))))
    (document+ collection (mapcar #'mutate-doc docs))))

;    (mapcar #'mutate-doc docs)))

(defun doc-list (ht &optional (fn (lambda (k v) t)))
  (let ((lst))
    (with-hash-table-iterator (iterator ht)
      (dotimes (repeat (hash-table-count ht))
	(multiple-value-bind (exists-p key value) (iterator)
	  (when (and exists-p (funcall fn key value) ) (push value lst)))))
    lst))

(defun match-keys (keys) 
  (lambda (key doc) 
    (declare (ignore key) )
    (reduce (lambda (l r) (and l r)) (mapcar  (lambda (k) (not (null (get-element  k doc)))) keys))))

(defun match-key-values (kv-lst) 
  (if (atom (car kv-lst) )
      (lambda (key doc) 
	(declare (ignore key) )
	(equal (cadr kv-lst) (get-element (car kv-lst) doc)))
      (lambda (key doc) 
	(declare (ignore key) )
	(reduce (lambda (l r) (and l r)) 
		(mapcar  (lambda (kv) (equal (cadr kv) (get-element (car kv) doc))) kv-lst)))))

(defun docs->ht (docs)
  (let ((ht (make-hash-table :test 'equalp)))
    (mapcan (lambda (d) (setf (gethash (doc-id d) ht) d)) docs)
    ht))

(defun ht->lst (ht)
  (let ((lst))
    (labels ((gen (k v)
	       (declare (ignore k))
	       (push v lst)))
      (maphash #'gen ht)
      (nreverse lst))))

(defun unique (docs)
  (ht->lst (docs->ht docs)))

(defun make-docs (n &optional (docs nil)) 
  (if (< n 1)
      docs
      (make-docs (decf n) (cons (make-document) docs))))

(defmacro $exp+ (&rest args)
  (cond (  (zerop (length args) )  '())
	(  (symbolp (car args) ) (cond ( (fboundp (car args)) `(progn ,args))
				       ( (boundp (car args)) `(cons ,(car args) ($exp+ ,@(cdr args))))
				       ( t                   `(list ,(car args) ,@(cdr args)))))
	(  (atom  (car args) ) `(cons ,(car args) ($exp+ ,@(cdr args))))
	(  (consp (car args) ) `(cons ($exp+ ,@(car args) ) ($exp+ ,@(cdr args))))
	(t (format t "can only handle atoms and cons")))) 

(defmacro with-collection (collection docdef &rest args)
    `(labels ((plist(dd)
		(cond ( (null dd)           (make-docs 1))
		      ( (numberp (car dd))  (make-docs (car dd)))
		      ( (eql (car dd) :all) (doc-list (documents ,collection)))
		      ( (eql (car dd) :key) (doc-list (documents ,collection) (match-keys (cdr dd))))
		      ( (stringp (car dd) ) (doc-list (documents ,collection) (match-key-values dd)))
		      (t                      (format t "case ~A not handled~%" dd)))))
     (cond ((atom (car ',docdef)) (process-document (plist ',docdef) ,collection  ($exp+ ,@args)))
	     ( t (process-document (unique (mapcan #'plist ',docdef)) ,collection ($exp+ ,@args))))))

;       ($exp+ ,@args)))




