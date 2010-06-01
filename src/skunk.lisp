(in-package :cl-skunk)

(defclass skunk-collection ()
  ((name      :initarg  :name    :reader name)
   (documents :initform (make-hash-table :test #'equal) :accessor documents)))

(defun skunk-name (collection)
  (format nil "skunk.~A" collection))

(defun connection (collection &key (host "localhost") (port cl-mongo::+MONGO-PORT+))
  (let ((name (skunk-name collection)))
    (if (mongo-registered name)
	(mongo :name name)
	(cl-mongo::make-mongo :host host :db "skunk" :port port :name name))))

(defgeneric collection (name &key) )

(defmethod  collection ( (name string) &key (host "localhost") (port cl-mongo::+MONGO-PORT+))
  (labels ((read-docs(collection)
	     (progn
	       (let ((mongodb (connection (name collection) :port port :host host)))
		 (dolist (d (docs (iter (db.find (name collection) :all :mongo mongodb) :mongo mongodb)))
		   (setf (gethash (doc-id d) (documents collection)) d)))
	       collection)))
    (handler-case 
	(read-docs (make-instance 'skunk-collection :name name))
      (error(c)
	(format t "error : ~A" c)
	(mongo-close (format nil "skunk.~A" name))))))

(defgeneric teardown (collection) )

(defmethod teardown ( (collection string ))
  (mongo-close (skunk-name collection)))

(defclass skunk-document ()
  ((key :initarg :key :reader key)
   (dirty-flag :initform nil :accessor dirty-flag)
   (mongo-document :initarg :mongo-document :accessor mongo-document)))

(defgeneric document+ ( collection document) )
;;-> string is by value
;;-> skunk-collection is by reference

;;--> if the key is _id, use the _id obviously (which will always be unique..
;;--> if NOT, on add you need to check the db for existence of this key value
;;--> document adds to collection automatically..

(defmethod document+ ( (collection string) (document document) )
  (db.save collection document :mongo (connection collection)))

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
		 (if (atom (car args) )
		     (mutate-fields args)
		     (mapcar #'mutate-fields args))
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

;
; creates a (list (list (list k v) (list k v)) (list (list k1 v2) (list k3 v2))) 
; i.e.  a list of lists of doc key-value pairs...
; input can be such 1) a list
;                   2) a single element of such a list (list (list k v) (list k2 v2)) 
;                   3) (list k1 v1) (list k2 v2) ..
;
(defun normalize (&rest lst)
    (cond ( (and (consp lst) (consp (car lst) ) (consp (car (car lst))) 
		 (consp (car (car (car lst)))) (atom (car(car(car(car lst)))))) (car lst))
	  ( (and (consp lst) (consp (car lst) ) (consp (car (car lst))) (atom (car (car(car lst))))) lst)
	  ( (and (consp lst) (consp (car lst) ) (atom (car(car lst)))) (list lst))
	  ( t (format t "not recognized : ~A ~%" lst))))

(defun unwrap (lst)
  (if (or (atom lst) (cdr lst) )
      lst
      (unwrap (car lst))))

(defun find-id(lst)
  (cond ( (null lst) lst)
	( (string= "_id" (car (car lst) )) (cadr (car lst)))
	( t (find-id (cdr lst)))))

      
(defmacro $exp+ (&rest args)
  (cond (  (zerop (length args) )  '())
	(  (symbolp (car args) ) (cond ( (fboundp (car args)) `(progn ,args))
				       ( (boundp (car args)) `(cons ,(car args) ($exp+ ,@(cdr args))))
				       ( t                   `(list ,(car args) ,@(cdr args)))))
	(  (atom  (car args) ) `(cons ,(car args) ($exp+ ,@(cdr args))))
	(  (consp (car args) ) `(cons ($exp+ ,@(car args) ) ($exp+ ,@(cdr args))))
	(t (format t "can only handle atoms and cons")))) 


(defmacro with-collection* (collection docdef &rest args)
  `(labels ((make-with-oid ()
	      (list (make-document :oid (find-id (unwrap ($exp+ ,@args))))))
	    (read-documents ()
	      (documents (collection ,collection)))
	    (plist(dd)
	      (cond ( (null dd)           (make-docs 1))
		    ( (numberp (car dd))  (make-docs (car dd)))
		    ( (eql (car dd) :all) (doc-list (read-documents)))
		    ( (eql (car dd) :key) (doc-list (read-documents) (match-keys (cdr dd))))
		    ( (stringp (car dd) ) (doc-list (read-documents) (match-key-values dd)))
		    (t                      (format t "case ~A not handled~%" dd)))))
     (cond ( (eql (car ',docdef) :id) 
	    (process-document (make-with-oid) ,collection (unwrap ($exp+ ,@args))))
	   ( (atom (car ',docdef))  
	    (process-document (plist ',docdef) ,collection  (unwrap ($exp+ ,@args))))
	   ( t 
	    (process-document (unique (mapcan #'plist ',docdef)) ,collection (unwrap ($exp+ ,@args)))))))

(defmacro with-collection (collection docdef &rest args)
    `(dolist (el (normalize (unwrap ($exp+ ,@args))))
       (with-collection* ,collection ,docdef el)))

(defgeneric map-skunk (fn coll) )

(defmethod map-skunk ( (fn function) (coll string) )
  (map-skunk fn (collection coll)))

(defmethod map-skunk ( (fn function) (coll skunk-collection) )
  (let ((ht (documents coll)))
    (with-hash-table-iterator (iterator ht) 
      (dotimes (repeat (hash-table-count ht))
	(multiple-value-bind (exists-p id doc) (iterator)
	  (declare (ignore id) (ignore exists-p) )
	  (funcall fn "_id" (doc-id doc) )
	  (maphash fn (cl-mongo::elements doc)))))))



