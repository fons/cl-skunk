# cl-skunk

## Description

cl-skunk is a very simple document management 'system' build on top of cl-mongo.
It's underlying peristsence framework is mongodb.
It's goal is to allow you to manage documents relatively painlessly.


## Installation

cl-skunk is asdf installable and it's been tested on sbcl.


## Interface

### (defmethod  collection ( (name string) &key (host "localhost") (port cl-mongo::+MONGO-PORT+))

This initializes a collection with the specified name.
The collection is stored in a mongodb instance specified by the port and host parameters. 
A seperate connection is established for each collection you create. The name of the connetion
is 'skunk.<name>'. You can view the connections using (cl-mongo::show :connections).
The collection object is relatively opaque; Use 'with-collection' macro or 'map-skunk' 
to work with a collection.

### (defgeneric teardown (collection) )

Terminates the connection to the mongodb instance.

### (defmacro with-collection (collection docdef &rest args)

* collection : name of the collection.
* docdef     : defines the operations on the collection.
* args       : list of the document specifications.

#### collection

'with-collection' updates a *collection* in the 'skunk' database. It will use the session parameters 
established when the 'collection' constructor was invoked. Otherwise it'll use a server running on 
the local host connected to the local port. 
    
This collection is accessible using the cl-mongo api calls as well.

    

#### docdef 

*docdef* specifies how the documents should be managed.

##### initializing collections

* ()    : creates one new document.
* (n)   : creates n new documents; () == (1)
* (:id) : creates one new document and uses the "_id" supplied by the user as it's unique id.

The document specification is applied to each new instance, if there's more than one.
 
##### updating collections

There are various ways to select existing documents in the collection to apply the document specifiers to.

* (:all)          : all documents.
* (:key *key*)    : all documents with this key.
* (*key* *value*) : all documents with this key and value.

##### combing docdefs

*docdefs* can be combined in a list. So ( (1) (:all) ) would create  new document and process 
all existing ones. Note that ( () :all) will not work.

#### document definitions : updates and deletes

A document definition specifies keys and value pairs as consecutive lists.
Multiple documents are specified as conses of single document specifiers.

The value for each matching key is updated with the value found in the document specification.
If the value is nil the key is deleted from the document.

### (defgeneric map-skunk (fn coll) )

*map-skunk* will iterate _fn_ over all (key,value) pairs of the documents in the collection. _fn_ is 
a function taking key and value as arguments. The "_id" key is always the first to be passed in and 
hence signals the start of a new document. 
 
## Examples


### single new document

    (with-collection "examples" () 
        ("title" "The Title")
        ("body"   "The Body of the document") 
        ("def"    "some sort of definition ?"))

    (db.use "skunk")
    "skunk"
    CL-USER> (pp (db.find "examples" :all))

     {
      "_id" -> objectid(2DC8664CC9A74AA7A4C98839)
      "def"  ->  some sort of definition ?
      "body"  ->  The Body of the document
      "title"  ->  The Title
     }
 
### change all documents 
    (with-collection "examples" (:all) 
          ("title" "title 45")
          ("body"  "body 45") 
          ("def"   "def 45")
          ("ts2"   4567889) )


          CL-USER> (pp (db.find "examples" :all))

           {
           "_id" -> objectid(2DC8664CC9A74AA7A4C98839)
           "ts2"  ->  4567889
           "title"  ->  title 45
           "body"  ->  body 45
           "def"  ->  def 45
           }


#### combine docdef operations 

Note that "ts2" has a nil key value and will be deleted from the existing document (and obviously won't
be part of the new document either. Note that ( () :all) will not work.

    (with-collection "examples" ( (1) (:all) )
        ("title" "title 83")
        ("body"  "new body of text. ") 
        ("def"   "a new definition")
        ("ts2"   nil) )


     CL-USER> (pp (db.find "examples" :all))

    {
     "_id" -> objectid(2DC8664CC9A74AA7A4C98839)
     "def"  ->  a new definition
     "body"  ->  new body of text. 
     "title"  ->  title 83
    }

     {
     "_id" -> objectid(6ADE6C25468F4B9B83D9D252)
     "def"  ->  a new definition
     "body"  ->  new body of text. 
     "title"  ->  title 83
     }

#### selecting documents with the :key keyword.

This adds the key "ts2" back into all documents whih have the keyword "title".

    CL-USER> (with-collection "examples" (:key "title") ("ts2"   4567889) )

    CL-USER> (pp (db.find "examples" :all))

    {
     "_id" -> objectid(2DC8664CC9A74AA7A4C98839)
     "ts2"  ->  4567889
     "title"  ->  title 83
     "body"  ->  new body of text. 
    "def"  ->  a new definition
     }

    {
     "_id" -> objectid(6ADE6C25468F4B9B83D9D252)
     "ts2"  ->  4567889
     "title"  ->  title 83
     "body"  ->  new body of text. 
     "def"  ->  a new definition
    }

