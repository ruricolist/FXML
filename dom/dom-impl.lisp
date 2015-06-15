;;;; dom-impl.lisp -- Implementation of DOM 1 Core -*- package: fxml.rune-dom -*-
;;;;
;;;; This file is part of the FXML parser, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;; Author: David Lichteblau <david@lichteblau.com>
;;;; Author: knowledgeTools Int. GmbH

#-fxml-system::utf8dom-file
(defpackage :fxml.rune-dom
  (:use :cl :runes)
  #+rune-is-character (:nicknames :fxml-dom)
  (:export #:implementation #:make-dom-builder #:create-document))

#+fxml-system::utf8dom-file
(defpackage :fxml.utf8-dom
  (:use :cl :utf8-runes)
  (:nicknames :fxml-dom)
  (:export #:implementation #:make-dom-builder #:create-document))

#-fxml-system::utf8dom-file
(in-package :fxml.rune-dom)

#+fxml-system::utf8dom-file
(in-package :fxml.utf8-dom)


;; Classes

(define-condition dom-exception (error)
  ((key       :initarg :key       :reader dom-exception-key)
   (string    :initarg :string    :reader dom-exception-string)
   (arguments :initarg :arguments :reader dom-exception-arguments))
  (:report
   (lambda (c s)
     (format s "~A (~D):~%~?"
             (dom-exception-key c)
             (fxml.dom:code c)
             (dom-exception-string c)
             (dom-exception-arguments c)))))

(defclass node (fxml.dom:node)
  ((parent      :initarg :parent        :initform nil)
   (children    :initarg :children      :initform (make-node-list))
   (owner       :initarg :owner         :initform nil)
   (read-only-p :initform nil           :reader read-only-p)
   (map         :initform nil)))

(defmethod fxml.dom:prefix ((node node)) nil)
(defmethod fxml.dom:local-name ((node node)) nil)
(defmethod fxml.dom:namespace-uri ((node node)) nil)

(defclass namespace-mixin ()
  ((prefix        :initarg :prefix        :reader fxml.dom:prefix)
   (local-name    :initarg :local-name    :reader fxml.dom:local-name)
   (namespace-uri :initarg :namespace-uri :reader fxml.dom:namespace-uri)))

(defmethod (setf fxml.dom:prefix) (newval (node namespace-mixin))
  (assert-writeable node)
  (when newval
    (safe-split-qname (concatenate 'rod newval #":foo")
		      (fxml.dom:namespace-uri node)))
  (setf (slot-value node 'prefix) newval))

(defclass document (node fxml.dom:document)
  ((doc-type    :initarg :doc-type     :reader fxml.dom:doctype)
   (dtd         :initform nil          :reader dtd)
   (entity-resolver :initform nil)))

(defclass document-fragment (node fxml.dom:document-fragment)
  ())

(defclass character-data (node fxml.dom:character-data)
  ((value       :initarg :data          :reader fxml.dom:data)))

(defclass attribute (namespace-mixin node fxml.dom:attr)
  ((name        :initarg :name          :reader fxml.dom:name)
   (owner-element :initarg :owner-element :reader fxml.dom:owner-element)
   (specified-p :initarg :specified-p   :reader fxml.dom:specified)))

(defmethod (setf fxml.dom:prefix) :before (newval (node attribute))
  (when (rod= (fxml.dom:node-name node) #"xmlns")
    (dom-error :NAMESPACE_ERR "must not change xmlns attribute prefix")))

(defmethod (setf fxml.dom:prefix) :after (newval (node attribute))
  (setf (slot-value node 'name)
	(concatenate 'rod newval #":" (fxml.dom:local-name node))))

(defmethod print-object ((object attribute) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A=~S"
            (rod-string (fxml.dom:name object))
            (rod-string (fxml.dom:value object)))))

(defclass element (namespace-mixin node fxml.dom:element)
  ((tag-name    :initarg :tag-name      :reader fxml.dom:tag-name)
   (attributes  :initarg :attributes    :reader fxml.dom:attributes)))

(defmethod (setf fxml.dom:prefix) :after (newval (node element))
  (setf (slot-value node 'tag-name)
	(concatenate 'rod newval #":" (fxml.dom:local-name node))))

(defmethod print-object ((object element) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (rod-string (fxml.dom:tag-name object)) stream)))

(defclass text (character-data fxml.dom:text)
  ())

(defclass comment (character-data fxml.dom:comment)
  ())

(defclass cdata-section (text fxml.dom:cdata-section)
  ())

(defclass document-type (node fxml.dom:document-type)
  ((name          :initarg :name          :reader fxml.dom:name)
   (public-id     :initarg :public-id     :reader fxml.dom:public-id)
   (system-id     :initarg :system-id     :reader fxml.dom:system-id)
   (entities      :initarg :entities      :reader fxml.dom:entities)
   (notations     :initarg :notations     :reader fxml.dom:notations)
   (fxml.dom::%internal-subset                 :accessor fxml.dom::%internal-subset)))

(defclass notation (node fxml.dom:notation)
  ((name          :initarg :name          :reader fxml.dom:name)
   (public-id     :initarg :public-id     :reader fxml.dom:public-id)
   (system-id     :initarg :system-id     :reader fxml.dom:system-id)))

(defclass entity (node fxml.dom:entity)
  ((name          :initarg :name          :reader fxml.dom:name)
   (public-id     :initarg :public-id     :reader fxml.dom:public-id)
   (system-id     :initarg :system-id     :reader fxml.dom:system-id)
   (notation-name :initarg :notation-name :reader fxml.dom:notation-name)))

(defclass entity-reference (node fxml.dom:entity-reference)
  ((name          :initarg :name          :reader fxml.dom:name)))

(defclass processing-instruction (node fxml.dom:processing-instruction)
  ((target        :initarg :target        :reader fxml.dom:target)
   (data          :initarg :data          :reader fxml.dom:data)))

(defclass named-node-map (fxml.dom:named-node-map)
  ((items         :initarg :items         :reader fxml.dom:items
                  :initform nil)
   (owner         :initarg :owner         :reader fxml.dom:owner-document)
   (read-only-p   :initform nil           :reader read-only-p)
   (element-type  :initarg :element-type)))

(defclass attribute-node-map (named-node-map)
  ((element       :initarg :element)))


;;; Implementation

(defun %rod (x)
  (etypecase x
    (null x)
    (rod x)
    #+fxml-system::utf8dom-file (runes::rod (fxml::rod-to-utf8-string x))
    (string (string-rod x))
    (vector x)))

#-fxml-system::utf8dom-file
(defun real-rod (x)
  (%rod x))

#+fxml-system::utf8dom-file
(defun real-rod (x)
  (etypecase x
    (null x)
    (runes::rod x)
    (string (fxml::utf8-string-to-rod x))))

(defun valid-name-p (x)
  (fxml::valid-name-p (real-rod x)))

(defun assert-writeable (node)
  (when (read-only-p node)
    (dom-error :NO_MODIFICATION_ALLOWED_ERR "~S is marked read-only." node)))

(defun fxml.dom:map-node-list (fn nodelist)
  (dotimes (i (fxml.dom:length nodelist))
    (funcall fn (fxml.dom:item nodelist i))))

(defmacro fxml.dom:do-node-list ((var nodelist &optional resultform) &body body)
  `(block nil
     (fxml.dom:map-node-list (lambda (,var) ,@body) ,nodelist)
     ,resultform))

(defun fxml.dom:map-node-map (fn node-map)
  (with-slots (items) node-map
    (mapc fn items)))

(defmacro fxml.dom:do-node-map ((var node-map &optional resultform) &body body)
  `(block nil
     (fxml.dom:map-node-map (lambda (,var) ,@body) ,node-map)
     ,resultform))

(defmacro dovector ((var vector &optional resultform) &body body)
  `(loop
       for ,var across ,vector do (progn ,@body)
       ,@(when resultform `(finally (return ,resultform)))))

(defun move (from to from-start to-start length)
  ;; like (setf (subseq to to-start (+ to-start length))
  ;;            (subseq from from-start (+ from-start length)))
  ;; but without creating the garbage.
  ;; Also, this is using AREF not ELT so that fill-pointers are ignored.
  (if (< to-start from-start)
      (loop
          repeat length
          for i from from-start
          for j from to-start
          do (setf (aref to j) (aref from i)))
      (loop
          repeat length
          for i downfrom (+ from-start length -1)
          for j downfrom (+ to-start length -1)
          do (setf (aref to j) (aref from i)))))

(defun adjust-vector-exponentially (vector new-dimension set-fill-pointer-p)
  (let ((d (array-dimension vector 0)))
    (when (< d new-dimension)
      (loop do (setf d (max 1 (* 2 d)))
         while (< d new-dimension))
      (adjust-array vector d))
    (when set-fill-pointer-p
      (setf (fill-pointer vector) new-dimension))))

(defun make-space (vector &optional (n 1))
  (adjust-vector-exponentially vector (+ (length vector) n) nil))

(defun extension (vector)
  (max (array-dimension vector 0) 1))

;; dom-exception

(defun dom-error (key fmt &rest args)
  (error 'dom-exception :key key :string fmt :arguments args))

(defmethod fxml.dom:code ((self dom-exception))
  (ecase (dom-exception-key self)
    (:INDEX_SIZE_ERR                    1)
    (:DOMSTRING_SIZE_ERR                2)
    (:HIERARCHY_REQUEST_ERR             3)
    (:WRONG_DOCUMENT_ERR                4)
    (:INVALID_CHARACTER_ERR             5)
    (:NO_DATA_ALLOWED_ERR               6)
    (:NO_MODIFICATION_ALLOWED_ERR       7)
    (:NOT_FOUND_ERR                     8)
    (:NOT_SUPPORTED_ERR                 9)
    (:INUSE_ATTRIBUTE_ERR               10)
    (:INVALID_STATE_ERR                 11)
    (:SYNTAX_ERR                        12)
    (:INVALID_MODIFICATION_ERR          13)
    (:NAMESPACE_ERR                     14)
    (:INVALID_ACCESS_ERR                15)))

;; dom-implementation protocol

(defmethod fxml.dom:has-feature ((factory (eql 'implementation)) feature version)
  (and (or (string-equal (rod-string feature) "xml")
	   (string-equal (rod-string feature) "core"))
       (or (zerop (length version))
	   (string-equal (rod-string version) "1.0")
	   (string-equal (rod-string version) "2.0"))))

(defun %create-document-type (name publicid systemid)
  (make-instance 'document-type
    :name name
    :notations (make-instance 'named-node-map
		 :element-type :notation
		 :owner nil)
    :entities (make-instance 'named-node-map
		:element-type :entity
		:owner nil)
    :public-id publicid
    :system-id systemid))

(defmethod fxml.dom:create-document-type
    ((factory (eql 'implementation)) name publicid systemid)
  (safe-split-qname name #"")
  (let ((result (%create-document-type name publicid systemid)))
    (setf (slot-value (fxml.dom:entities result) 'read-only-p) t)
    (setf (slot-value (fxml.dom:notations result) 'read-only-p) t)
    result))

(defmethod fxml.dom:create-document
    ((factory (eql 'implementation)) uri qname doctype)
  (let ((document (make-instance 'document)))
    (setf (slot-value document 'owner) nil
	  (slot-value document 'doc-type) doctype)
    (when doctype
      (unless (typep doctype 'document-type)
	(dom-error :WRONG_DOCUMENT_ERR
		   "doctype was created by a different dom implementation"))
      (when (fxml.dom:owner-document doctype)
	(dom-error :WRONG_DOCUMENT_ERR "doctype already in use"))
      (setf (slot-value doctype 'owner) document
	    (slot-value (fxml.dom:notations doctype) 'owner) document
	    (slot-value (fxml.dom:entities doctype) 'owner) document))
    (when (or uri qname)
      (fxml.dom:append-child document (fxml.dom:create-element-ns document uri qname)))
    document))

;; document-fragment protocol
;; document protocol

(defmethod fxml.dom:implementation ((document document))
  'implementation)

(defmethod fxml.dom:document-element ((document document))
  (dovector (k (fxml.dom:child-nodes document))
    (cond ((typep k 'element)
           (return k)))))

(defmethod fxml.dom:create-element ((document document) tag-name)
  (setf tag-name (%rod tag-name))
  (unless (valid-name-p tag-name)
    (dom-error :INVALID_CHARACTER_ERR "not a name: ~A" (rod-string tag-name)))
  (let ((result (make-instance 'element
                  :tag-name tag-name
		  :namespace-uri nil
		  :local-name nil
		  :prefix nil
                  :owner document)))
    (setf (slot-value result 'attributes)
          (make-instance 'attribute-node-map
            :element-type :attribute
            :owner document
            :element result))
    (add-default-attributes result)
    result))

(defun safe-split-qname (qname uri)
  (unless (valid-name-p qname)
    (dom-error :INVALID_CHARACTER_ERR "not a name: ~A" (rod-string qname)))
  (multiple-value-bind (prefix local-name)
      (handler-case
	  (fxml::split-qname (real-rod qname))
	(fxml:well-formedness-violation (c)
	  (dom-error :NAMESPACE_ERR "~A" c)))
    (setf local-name (%rod local-name))
    (when prefix
      (setf prefix (%rod prefix))
      (unless uri
	(dom-error :NAMESPACE_ERR "prefix specified but no namespace URI"))
      (when (and (rod= prefix #"xml")
		 (not (rod= uri #"http://www.w3.org/XML/1998/namespace")))
	(dom-error :NAMESPACE_ERR "invalid uri for prefix `xml'"))
      (when (and (rod= prefix #"xmlns")
		 (not (rod= uri #"http://www.w3.org/2000/xmlns/")))
	(dom-error :NAMESPACE_ERR "invalid uri for prefix `xmlns'")))
    (values prefix local-name)))

(defmethod fxml.dom:create-element-ns ((document document) uri qname)
  (setf qname (%rod qname))
  (multiple-value-bind (prefix local-name)
      (safe-split-qname qname uri)
    (let ((result (make-instance 'element
		    :tag-name qname
		    :namespace-uri uri
		    :local-name local-name
		    :prefix prefix
		    :owner document)))
      (setf (slot-value result 'attributes)
	    (make-instance 'attribute-node-map
	      :element-type :attribute
	      :owner document
	      :element result))
      (add-default-attributes result)
      result)))

(defmethod fxml.dom:create-document-fragment ((document document))
  (make-instance 'document-fragment
    :owner document))

(defmethod fxml.dom:create-text-node ((document document) data)
  (setf data (%rod data))
  (make-instance 'text
    :data data
    :owner document))

(defmethod fxml.dom:create-comment ((document document) data)
  (setf data (%rod data))
  (make-instance 'comment
    :data data
    :owner document))

(defmethod fxml.dom:create-cdata-section ((document document) data)
  (setf data (%rod data))
  (make-instance 'cdata-section
    :data data
    :owner document))

(defmethod fxml.dom:create-processing-instruction ((document document) target data)
  (setf target (%rod target))
  (setf data (%rod data))
  (unless (valid-name-p target)
    (dom-error :INVALID_CHARACTER_ERR "not a name: ~A" (rod-string target)))
  (make-instance 'processing-instruction
    :owner document
    :target target
    :data data))

(defmethod fxml.dom:create-attribute ((document document) name)
  (setf name (%rod name))
  (unless (valid-name-p name)
    (dom-error :INVALID_CHARACTER_ERR "not a name: ~A" (rod-string name)))
  (make-instance 'attribute
    :name name
    :local-name nil
    :prefix nil
    :namespace-uri nil
    :specified-p t
    :owner-element nil
    :owner document))

(defmethod fxml.dom:create-attribute-ns ((document document) uri qname)
  (setf uri (%rod uri))
  (setf qname (%rod qname))
  (when (and (rod= qname #"xmlns")
	     (not (rod= uri #"http://www.w3.org/2000/xmlns/")))
    (dom-error :NAMESPACE_ERR "invalid uri for qname `xmlns'"))
  (multiple-value-bind (prefix local-name)
      (safe-split-qname qname uri)
    (make-instance 'attribute
      :name qname
      :namespace-uri uri
      :local-name local-name
      :prefix prefix
      :specified-p t
      :owner-element nil
      :owner document)))

(defmethod fxml.dom:create-entity-reference ((document document) name)
  (setf name (%rod name))
  (unless (valid-name-p name)
    (dom-error :INVALID_CHARACTER_ERR "not a name: ~A" (rod-string name)))
  (make-instance 'entity-reference
    :name name
    :owner document))

(defmethod get-elements-by-tag-name-internal (node tag-name)
  (setf tag-name (%rod tag-name))
  (let ((result (make-node-list))
	(wild-p (rod= tag-name #"*")))
    (labels ((walk (n)
	       (dovector (c (fxml.dom:child-nodes n))
		 (when (fxml.dom:element-p c)
		   (when (or wild-p (rod= tag-name (fxml.dom:node-name c)))
		     (vector-push-extend c result (extension result)))
		   (walk c)))))
      (walk node))
    result))

(defmethod get-elements-by-tag-name-internal-ns (node uri lname)
  (setf uri (%rod uri))
  (setf lname (%rod lname))
  (let ((result (make-node-list))
	(wild-uri-p (rod= uri #"*"))
	(wild-lname-p (rod= lname #"*")))
    (labels ((walk (n)
	       (dovector (c (fxml.dom:child-nodes n))
		 (when (fxml.dom:element-p c)
		   (when (and (or wild-lname-p (rod= lname (fxml.dom:local-name c)))
			      (or wild-uri-p (rod= uri (fxml.dom:namespace-uri c))))
		     (vector-push-extend c result (extension result)))
		   (walk c)))))
      (walk node))
    result))

(defmethod fxml.dom:get-elements-by-tag-name ((document document) tag-name)
  (get-elements-by-tag-name-internal document tag-name))

(defmethod fxml.dom:get-elements-by-tag-name-ns ((document document) uri lname)
  (get-elements-by-tag-name-internal-ns document uri lname))

(defmethod fxml.dom:get-element-by-id ((document document) id)
  (block t
    (unless (dtd document)
      (return-from t nil))
    (setf id (%rod id))
    (labels ((walk (n)
	       (dovector (c (fxml.dom:child-nodes n))
		 (when (fxml.dom:element-p c)
		   (let ((e (fxml::find-element
			     (real-rod (fxml.dom:tag-name c))
			     (dtd document))))
		     (when e
		       (dolist (a (fxml::elmdef-attributes e))
			 (when (eq :ID (fxml::attdef-type a))
			   (let* ((name (%rod (fxml::attdef-name a)))
				  (value (fxml.dom:get-attribute c name)))
			     (when (and value (rod= value id))
			       (return-from t c)))))))
		   (walk c)))))
      (walk document))))


;;; Node

(defmethod fxml.dom:has-attributes ((element node))
  nil)

(defmethod fxml.dom:is-supported ((node node) feature version)
  (fxml.dom:has-feature 'implementation feature version))

(defmethod fxml.dom:parent-node ((node node))
  (slot-value node 'parent))

(defmethod fxml.dom:child-nodes ((node node))
  (slot-value node 'children))

(defmethod fxml.dom:first-child ((node node))
  (fxml.dom:item (slot-value node 'children) 0))

(defmethod fxml.dom:last-child ((node node))
  (with-slots (children) node
    (if (plusp (length children))
        (elt children (1- (length children)))
        nil)))

(defmethod fxml.dom:previous-sibling ((node node))
  (with-slots (parent) node
    (when parent
      (with-slots (children) parent
        (let ((index (1- (position node children))))
          (if (eql index -1)
              nil
              (elt children index)))))))

(defmethod fxml.dom:next-sibling ((node node))
  (with-slots (parent) node
    (when parent
      (with-slots (children) parent
        (let ((index (1+ (position node children))))
          (if (eql index (length children))
              nil
              (elt children index)))))))

(defmethod fxml.dom:owner-document ((node node))
  (slot-value node 'owner))

(defun ensure-valid-insertion-request (node new-child)
  (assert-writeable node)
  (unless (can-adopt-p node new-child)
    (dom-error :HIERARCHY_REQUEST_ERR "~S cannot adopt ~S." node new-child))
  #+(or)                                ;XXX needs to be moved elsewhere
  (when (eq (fxml.dom:node-type node) :document)
    (let ((child-type (fxml.dom:node-type new-child)))
      (when (and (member child-type '(:element :document-type))
                 (find child-type (fxml.dom:child-nodes node) :key #'fxml.dom:node-type))
        (dom-error :HIERARCHY_REQUEST_ERR
                   "~S cannot adopt a second child of type ~S."
                   node child-type))))
  (unless (eq (if (eq (fxml.dom:node-type node) :document)
                  node
                  (fxml.dom:owner-document node))
              (fxml.dom:owner-document new-child))
    (dom-error :WRONG_DOCUMENT_ERR
               "~S cannot adopt ~S, since it was created by a different document."
               node new-child))
  (do ((n node (fxml.dom:parent-node n)))
      ((null n))
    (when (eq n new-child)
      (dom-error :HIERARCHY_REQUEST_ERR
                 "~S cannot adopt ~S, since that would create a cycle"
                 node new-child)))
  (unless (null (slot-value new-child 'parent))
    (fxml.dom:remove-child (slot-value new-child 'parent) new-child)))

(defmethod fxml.dom:insert-before ((node node) (new-child node) ref-child)
  (ensure-valid-insertion-request node new-child)
  (with-slots (children) node
    (if ref-child
        (let ((i (position ref-child children)))
          (unless i
            (dom-error :NOT_FOUND_ERR "~S is no child of ~S." ref-child node))
          (make-space children 1)
          (move children children i (1+ i) (- (length children) i))
          (incf (fill-pointer children))
          (setf (elt children i) new-child))
        (vector-push-extend new-child children (extension children)))
    (setf (slot-value new-child 'parent) node)
    new-child))

(defmethod fxml.dom:insert-before
    ((node node) (fragment document-fragment) ref-child)
  (let ((children (fxml.dom:child-nodes fragment)))
    (fxml::while (plusp (length children))
      (fxml.dom:insert-before node (elt children 0) ref-child)))
  fragment)

(defmethod fxml.dom:replace-child ((node node) (new-child node) (old-child node))
  (ensure-valid-insertion-request node new-child)
  (with-slots (children) node
    (let ((i (position old-child children)))
      (unless i
        (dom-error :NOT_FOUND_ERR "~S is no child of ~S." old-child node))
      (setf (elt children i) new-child)) 
    (setf (slot-value new-child 'parent) node)
    (setf (slot-value old-child 'parent) nil)
    old-child))

(defmethod fxml.dom:replace-child
    ((node node) (new-child document-fragment) (old-child node))
  (fxml.dom:insert-before node new-child old-child)
  (fxml.dom:remove-child node old-child))

(defmethod fxml.dom:remove-child ((node node) (old-child node))
  (assert-writeable node)
  (with-slots (children) node
    (let ((i (position old-child children)))
      (unless i
        (dom-error :NOT_FOUND_ERR "~A not found in ~A" old-child node))
      (move children children (1+ i) i (- (length children) i 1))
      (decf (fill-pointer children)))
    (setf (slot-value old-child 'parent) nil)
    old-child))

(defmethod fxml.dom:append-child ((node node) (new-child node))
  (ensure-valid-insertion-request node new-child)
  (with-slots (children) node
    (vector-push-extend new-child children (extension children))
    (setf (slot-value new-child 'parent) node)
    new-child))

(defmethod fxml.dom:has-child-nodes ((node node))
  (plusp (length (slot-value node 'children))))

(defmethod fxml.dom:append-child ((node node) (new-child document-fragment))
  (assert-writeable node)
  (let ((children (fxml.dom:child-nodes new-child)))
    (fxml::while (plusp (length children))
      (fxml.dom:append-child node (elt children 0))))
  new-child)

;; was auf node noch implemetiert werden muss:
;; - node-type
;; - can-adopt-p
;; - ggf attributes 
;; - node-name
;; - node-value

;; node-name 

(defmethod fxml.dom:node-name ((self document))
  #"#document")

(defmethod fxml.dom:node-name ((self document-fragment))
  #"#document-fragment")

(defmethod fxml.dom:node-name ((self text))
  #"#text")

(defmethod fxml.dom:node-name ((self cdata-section))
  #"#cdata-section")

(defmethod fxml.dom:node-name ((self comment))
  #"#comment")

(defmethod fxml.dom:node-name ((self attribute))
  (fxml.dom:name self))

(defmethod fxml.dom:node-name ((self element))
  (fxml.dom:tag-name self))

(defmethod fxml.dom:node-name ((self document-type))
  (fxml.dom:name self))

(defmethod fxml.dom:node-name ((self notation))
  (fxml.dom:name self))

(defmethod fxml.dom:node-name ((self entity))
  (fxml.dom:name self))

(defmethod fxml.dom:node-name ((self entity-reference))
  (fxml.dom:name self))

(defmethod fxml.dom:node-name ((self processing-instruction))
  (fxml.dom:target self))

;; node-type

(defmethod fxml.dom:node-type ((self document)) :document)
(defmethod fxml.dom:node-type ((self document-fragment)) :document-fragment)
(defmethod fxml.dom:node-type ((self text)) :text)
(defmethod fxml.dom:node-type ((self comment)) :comment)
(defmethod fxml.dom:node-type ((self cdata-section)) :cdata-section)
(defmethod fxml.dom:node-type ((self attribute)) :attribute)
(defmethod fxml.dom:node-type ((self element)) :element)
(defmethod fxml.dom:node-type ((self document-type)) :document-type)
(defmethod fxml.dom:node-type ((self notation)) :notation)
(defmethod fxml.dom:node-type ((self entity)) :entity)
(defmethod fxml.dom:node-type ((self entity-reference)) :entity-reference)
(defmethod fxml.dom:node-type ((self processing-instruction)) :processing-instruction)

;; node-value

(defmethod fxml.dom:node-value ((self document)) nil)
(defmethod fxml.dom:node-value ((self document-fragment)) nil)
(defmethod fxml.dom:node-value ((self character-data)) (fxml.dom:data self))
(defmethod fxml.dom:node-value ((self attribute)) (fxml.dom:value self))
(defmethod fxml.dom:node-value ((self element)) nil)
(defmethod fxml.dom:node-value ((self document-type)) nil)
(defmethod fxml.dom:node-value ((self notation)) nil)
(defmethod fxml.dom:node-value ((self entity)) nil)
(defmethod fxml.dom:node-value ((self entity-reference)) nil)
(defmethod fxml.dom:node-value ((self processing-instruction)) (fxml.dom:data self))

;; (setf node-value), first the meaningful cases...

(defmethod (setf fxml.dom:node-value) (newval (self character-data))
  (assert-writeable self)
  (setf (fxml.dom:data self) newval))

(defmethod (setf fxml.dom:node-value) (newval (self attribute))
  (assert-writeable self)
  (setf (fxml.dom:value self) newval))

(defmethod (setf fxml.dom:node-value) (newval (self processing-instruction))
  (assert-writeable self)
  (setf (fxml.dom:data self) newval))

;; ... and (setf node-value), part II.  The DOM Level 1 spec fails to explain
;; this case, but it is covered by the (Level 1) test suite and clarified
;; in Level 2:
;;         nodeValue of type DOMString
;;                 The value of this node, depending on its type; see the
;;                 table above.  When it is defined to be null, setting
;;                 it has no effect.

(defmethod (setf fxml.dom:node-value) (newval (self element))
  (declare (ignore newval)))

(defmethod (setf fxml.dom:node-value) (newval (self entity-reference))
  (declare (ignore newval)))

(defmethod (setf fxml.dom:node-value) (newval (self entity))
  (declare (ignore newval)))

(defmethod (setf fxml.dom:node-value) (newval (self document))
  (declare (ignore newval)))

(defmethod (setf fxml.dom:node-value) (newval (self document-type))
  (declare (ignore newval)))

(defmethod (setf fxml.dom:node-value) (newval (self document-fragment))
  (declare (ignore newval)))

(defmethod (setf fxml.dom:node-value) (newval (self notation))
  (declare (ignore newval)))

;; attributes

;; (gibt es nur auf element)

(defmethod fxml.dom:attributes ((self node))
  nil)

;; dann fehlt noch can-adopt und attribute conventions fuer adoption

;;; NodeList

(defun make-node-list (&optional initial-contents)
  (make-array (length initial-contents)
              :adjustable t
              :fill-pointer (length initial-contents)
              :initial-contents initial-contents))

(defmethod fxml.dom:item ((self vector) index)
  (if (< index (length self))
      (elt self index)
      nil))

(defmethod fxml.dom:length ((self vector))
  (length self))

;;; NAMED-NODE-MAP

(defmethod fxml.dom:get-named-item ((self named-node-map) name)
  (setf name (%rod name))
  (with-slots (items) self
    (dolist (k items nil)
      (when (rod= name (fxml.dom:node-name k))
	(return k)))))

(defmethod fxml.dom:get-named-item-ns ((self named-node-map) uri lname)
  (setf uri (%rod uri))
  (setf lname (%rod lname))
  (with-slots (items) self
    (dolist (k items nil)
      (when (and (rod= uri (fxml.dom:namespace-uri k))
		 (rod= lname (fxml.dom:local-name k)))
	(return k)))))

(defun %set-named-item (map arg test)
  (assert-writeable map)
  (unless (eq (fxml.dom:node-type arg) (slot-value map 'element-type))
    (dom-error :HIERARCHY_REQUEST_ERR
               "~S cannot adopt ~S, since it is not of type ~S."
               map arg (slot-value map 'element-type)))
  (unless (eq (fxml.dom:owner-document map) (fxml.dom:owner-document arg))
    (dom-error :WRONG_DOCUMENT_ERR
               "~S cannot adopt ~S, since it was created by a different document."
               map arg))
  (let ((old-map (slot-value arg 'map)))
    (when (and old-map (not (eq old-map map)))
      (dom-error :INUSE_ATTRIBUTE_ERR "Attribute node already mapped" arg)))
  (setf (slot-value arg 'map) map)
  (with-slots (items) map
    (dolist (k items (progn (setf items (cons arg items)) nil))
      (when (funcall test k)
	(setf items (cons arg (delete k items)))
	(return k)))))

(defmethod fxml.dom:set-named-item ((self named-node-map) arg)
  (let ((name (fxml.dom:node-name arg)))
    (%set-named-item self arg (lambda (k) (rod= name (fxml.dom:node-name k))))))

(defmethod fxml.dom:set-named-item-ns ((self named-node-map) arg)
  (let ((uri (fxml.dom:namespace-uri arg))
	(lname (fxml.dom:local-name arg)))
    (%set-named-item self
		     arg
		     (lambda (k)
		       (and (rod= lname (fxml.dom:local-name k))
			    (rod= uri (fxml.dom:namespace-uri k)))))))

(defmethod fxml.dom:remove-named-item ((self named-node-map) name)
  (assert-writeable self)
  (setf name (%rod name))
  (with-slots (items) self
    (dolist (k items (dom-error :NOT_FOUND_ERR "~A not found in ~A" name self))
      (cond ((rod= name (fxml.dom:node-name k))
             (setf items (delete k items))
             (return k))))))

(defmethod fxml.dom:remove-named-item-ns ((self named-node-map) uri lname)
  (assert-writeable self)
  (setf uri (%rod uri))
  (setf lname (%rod lname))
  (with-slots (items) self
    (dolist (k items
	      (dom-error :NOT_FOUND_ERR "~A not found in ~A" lname self))
      (when (and (rod= lname (fxml.dom:local-name k))
		 (rod= uri (fxml.dom:namespace-uri k)))
	(setf items (delete k items))
	(return k)))))

(defmethod fxml.dom:length ((self named-node-map))
  (with-slots (items) self
    (length items)))

(defmethod fxml.dom:item ((self named-node-map) index)
  (with-slots (items) self
    (do ((nthcdr items (cdr nthcdr))
         (i index (1- i)))
        ((zerop i) (car nthcdr)))))

;;; CHARACTER-DATA

(defmethod (setf fxml.dom:data) (newval (self character-data))
  (assert-writeable self)
  (setf newval (%rod newval))
  (setf (slot-value self 'value) newval))

(defmethod fxml.dom:length ((node character-data))
  (length (slot-value node 'value)))

(defmethod fxml.dom:substring-data ((node character-data) offset count)
  (with-slots (value) node
    (unless (<= 0 offset (length value))
      (dom-error :INDEX_SIZE_ERR "offset is invalid"))
    (let ((end (min (length value) (+ offset count))))
      (subseq value offset end))))

(defmethod fxml.dom:append-data ((node character-data) arg)
  (assert-writeable node)
  (setq arg (%rod arg))
  (with-slots (value) node
    (setf value (concatenate 'rod value arg)))
  (values))

(defmethod fxml.dom:delete-data ((node character-data) offset count)
  (assert-writeable node)
  (with-slots (value) node
    (unless (<= 0 offset (length value))
      (dom-error :INDEX_SIZE_ERR "offset is invalid"))
    (when (minusp count)
      (dom-error :INDEX_SIZE_ERR "count is negative"))
    (setf count (min count (- (length value) offset)))
    (let ((new (make-array (- (length value) count)
                           :element-type (array-element-type value))))
      (replace new value 
               :start1 0 :end1 offset
               :start2 0 :end2 offset)
      (replace new value 
               :start1 offset :end1 (length new)
               :start2 (+ offset count) :end2 (length value))
      (setf value new)))
  (values))

(defmethod fxml.dom:replace-data ((node character-data) offset count arg)
  ;; Although we could implement this by calling DELETE-DATA, then INSERT-DATA,
  ;; we implement this function directly to avoid creating temporary garbage.
  (assert-writeable node)
  (setf arg (%rod arg))
  (with-slots (value) node
    (unless (<= 0 offset (length value))
      (dom-error :INDEX_SIZE_ERR "offset is invalid"))
    (when (minusp count)
      (dom-error :INDEX_SIZE_ERR "count is negative"))
    (setf count (min count (- (length value) offset)))
    (if (= count (length arg))
        (replace value arg
                 :start1 offset :end1 (+ offset count)
                 :start2 0 :end2 count)
        (let ((new (make-array (+ (length value) (length arg) (- count))
                               :element-type (array-element-type value))))
          (replace new value :end1 offset)
          (replace new arg :start1 offset)
          (replace new value
                   :start1 (+ offset (length arg))
                   :start2 (+ offset count))
          (setf value new))))
  (values))

(defmethod fxml.dom:insert-data ((node character-data) offset arg)
  (assert-writeable node)
  (setf arg (%rod arg))
  (with-slots (value) node
    (unless (<= 0 offset (length value))
      (dom-error :INDEX_SIZE_ERR "offset is invalid"))
    (let ((new (make-array (+ (length value) (length arg))
                           :element-type (array-element-type value)))
          (arglen (length arg)))
      (replace new value :end1 offset)
      (replace new arg :start1 offset)
      (replace new value :start1 (+ offset arglen) :start2 offset)
      (setf value new)))
  (values))

;;; ATTR
;;;
;;; An attribute value can be read and set as a string using FXML.DOM:VALUE
;;; or frobbed by changing the attribute's children!
;;;
;;; We store the value in a TEXT node and read this node's DATA slot
;;; when asked for our VALUE -- until the user changes the child nodes,
;;; in which case we have to compute VALUE by traversing the children.

(defmethod fxml.dom:value ((node attribute))
  (with-slots (children) node
    (cond
      ((zerop (length children))
        #.(rod-string ""))
      ((and (eql (length children) 1)
            (eq (fxml.dom:node-type (elt children 0)) :text))
        ;; we have as single TEXT-NODE child, just return its DATA
        (fxml.dom:data (elt children 0)))
      (t
        ;; traverse children to compute value
        (attribute-to-string node)))))

(defmethod (setf fxml.dom:value) (new-value (node attribute))
  (assert-writeable node)
  (let ((rod (%rod new-value)))
    (with-slots (children owner) node
      ;; remove children, add new TEXT-NODE child
      ;; (alas, we must not reuse an old TEXT-NODE)
      (fxml::while (plusp (length children))
        (fxml.dom:remove-child node (fxml.dom:last-child node)))
      (fxml.dom:append-child node (fxml.dom:create-text-node owner rod))))
  new-value)

(defun attribute-to-string (attribute)
  (let ((stream (make-rod-stream)))
    (flet ((doit ()
             (dovector (child (fxml.dom:child-nodes attribute))
               (write-attribute-child child stream))))
      (doit)
      (initialize-rod-stream stream)
      (doit))
    (rod-stream-buf stream)))

(defmethod write-attribute-child ((node node) stream)
  (put-rod (fxml.dom:node-value node) stream))

(defmethod write-attribute-child ((node entity-reference) stream)
  (dovector (child (fxml.dom:child-nodes node))
    (write-attribute-child child stream)))

;;; ROD-STREAM als Ersatz fuer MAKE-STRING-OUTPUT-STREAM zu verwenden,
;;; nur dass der Buffer statische Groesse hat.  Solange er NIL ist,
;;; zaehlt der Stream nur die Runen.  Dann ruft man INITIALIZE-ROD-STREAM
;;; auf, um den Buffer zu erzeugen und die Position zurueckzusetzen, und
;;; schreibt alles abermals.  Dann ist der Buffer gefuellt.
(defstruct rod-stream
  (buf nil)
  (position 0))

(defun put-rod (rod rod-stream)
  (let ((buf (rod-stream-buf rod-stream)))
    (when buf
      (move rod buf 0 (rod-stream-position rod-stream) (length rod)))
    (incf (rod-stream-position rod-stream) (length rod)))
  rod)

(defun initialize-rod-stream (stream)
  (setf (rod-stream-buf stream) (make-rod (rod-stream-position stream)))
  (setf (rod-stream-position stream) 0)
  stream)

;;; ELEMENT

(defmethod fxml.dom:has-attributes ((element element))
  (plusp (length (fxml.dom:items (fxml.dom:attributes element)))))

(defmethod fxml.dom:has-attribute ((element element) name)
  (and (fxml.dom:get-named-item (fxml.dom:attributes element) name) t))

(defmethod fxml.dom:has-attribute-ns ((element element) uri lname)
  (and (fxml.dom:get-named-item-ns (fxml.dom:attributes element) uri lname) t))

(defmethod fxml.dom:get-attribute-node ((element element) name)
  (fxml.dom:get-named-item (fxml.dom:attributes element) name))

(defmethod fxml.dom:set-attribute-node ((element element) (new-attr attribute))
  (assert-writeable element)
  (fxml.dom:set-named-item (fxml.dom:attributes element) new-attr))

(defmethod fxml.dom:get-attribute-node-ns ((element element) uri lname)
  (fxml.dom:get-named-item-ns (fxml.dom:attributes element) uri lname))

(defmethod fxml.dom:set-attribute-node-ns ((element element) (new-attr attribute))
  (assert-writeable element)
  (fxml.dom:set-named-item-ns (fxml.dom:attributes element) new-attr))

(defmethod fxml.dom:get-attribute ((element element) name)
  (let ((a (fxml.dom:get-attribute-node element name)))
    (if a
        (fxml.dom:value a)
        #"")))

(defmethod fxml.dom:get-attribute-ns ((element element) uri lname)
  (let ((a (fxml.dom:get-attribute-node-ns element uri lname)))
    (if a
        (fxml.dom:value a)
        #"")))

(defmethod fxml.dom:set-attribute ((element element) name value)
  (assert-writeable element)
  (with-slots (owner) element
    (let ((attr (fxml.dom:create-attribute owner name)))
      (setf (slot-value attr 'owner-element) element)
      (setf (fxml.dom:value attr) value)
      (fxml.dom:set-attribute-node element attr))
    (values)))

(defmethod fxml.dom:set-attribute-ns ((element element) uri lname value)
  (assert-writeable element)
  (with-slots (owner) element
    (let ((attr (fxml.dom:create-attribute-ns owner uri lname)))
      (setf (slot-value attr 'owner-element) element)
      (setf (fxml.dom:value attr) value)
      (fxml.dom:set-attribute-node-ns element attr))
    (values)))

(defmethod fxml.dom:remove-attribute ((element element) name)
  (assert-writeable element)
  (fxml.dom:remove-attribute-node element (fxml.dom:get-attribute-node element name)))

(defmethod fxml.dom:remove-attribute-ns ((elt element) uri lname)
  (assert-writeable elt)
  (fxml.dom:remove-attribute-node elt (fxml.dom:get-attribute-node-ns elt uri lname)))

(defmethod fxml.dom:remove-attribute-node ((element element) (old-attr attribute))
  (assert-writeable element)
  (with-slots (items) (fxml.dom:attributes element)
    (unless (find old-attr items)
      (dom-error :NOT_FOUND_ERR "Attribute not found."))
    (setf items (remove old-attr items))
    (maybe-add-default-attribute element old-attr)
    old-attr))

;; eek, defaulting:

(defun maybe-add-default-attribute (element old-attr)
  (let* ((qname (fxml.dom:name old-attr))
	 (dtd (dtd (slot-value element 'owner)))
         (e (when dtd (fxml::find-element
		       (real-rod (fxml.dom:tag-name element))
		       dtd)))
         (a (when e (fxml::find-attribute e (real-rod qname)))))
    (when (and a (listp (fxml::attdef-default a)))
      (let ((new (add-default-attribute element a)))
	(setf (slot-value new 'namespace-uri) (fxml.dom:namespace-uri old-attr))
	(setf (slot-value new 'prefix) (fxml.dom:prefix old-attr))
	(setf (slot-value new 'local-name) (fxml.dom:local-name old-attr))))))

(defun add-default-attributes (element)
  (let* ((dtd (dtd (slot-value element 'owner)))
         (e (when dtd (fxml::find-element
		       (real-rod (fxml.dom:tag-name element))
		       dtd))))
    (when e
      (dolist (a (fxml::elmdef-attributes e))
        (when (and a
		   (listp (fxml::attdef-default a))
		   (not (fxml.dom:get-attribute-node
			 element
			 (%rod (fxml::attdef-name a)))))
          (let ((anode (add-default-attribute element a)))
	    (multiple-value-bind (prefix local-name)
		(handler-case
		    (fxml::split-qname (fxml::attdef-name a))
		  (fxml:well-formedness-violation (c)
		    (dom-error :NAMESPACE_ERR "~A" c)))
	      (when prefix (setf prefix (%rod prefix)))
	      (setf local-name (%rod local-name))
	      ;; das ist fuer importnode07.
	      ;; so richtig ueberzeugend finde ich das ja nicht.
	      (setf (slot-value anode 'prefix) prefix)
	      (setf (slot-value anode 'local-name) local-name))))))))

(defun add-default-attribute (element adef)
  (let* ((value (second (fxml::attdef-default adef)))
         (owner (slot-value element 'owner))
         (anode (fxml.dom:create-attribute owner (fxml::attdef-name adef)))
         (text (fxml.dom:create-text-node owner value)))
    (setf (slot-value anode 'specified-p) nil)
    (setf (slot-value anode 'owner-element) element)
    (fxml.dom:append-child anode text)
    (push anode (slot-value (fxml.dom:attributes element) 'items))
    anode))

(defmethod fxml.dom:remove-named-item ((self attribute-node-map) name)
  name
  (let ((k (call-next-method)))
    (maybe-add-default-attribute (slot-value self 'element) k)
    k))

(defmethod fxml.dom:remove-named-item-ns ((self attribute-node-map) uri lname)
  uri lname
  (let ((k (call-next-method)))
    (maybe-add-default-attribute (slot-value self 'element) k)
    k))

(defmethod fxml.dom:get-elements-by-tag-name ((element element) name)
  (assert-writeable element)
  (get-elements-by-tag-name-internal element name))

(defmethod fxml.dom:get-elements-by-tag-name-ns ((element element) uri lname)
  (assert-writeable element)
  (get-elements-by-tag-name-internal-ns element uri lname))

(defmethod fxml.dom:set-named-item :after ((self attribute-node-map) arg)
  (setf (slot-value arg 'owner-element)
	(slot-value self 'element)))

(defmethod fxml.dom:set-named-item-ns :after ((self attribute-node-map) arg)
  (setf (slot-value arg 'owner-element)
	(slot-value self 'element)))

(defmethod fxml.dom:normalize ((node node))
  (assert-writeable node)
  (labels ((walk (n)
             (when (eq (fxml.dom:node-type n) :element)
               (map nil #'walk (fxml.dom:items (fxml.dom:attributes n))))
             (let ((children (fxml.dom:child-nodes n))
                   (i 0)
                   (previous nil))
               ;; careful here, we're modifying the array we are iterating over
               (fxml::while (< i (length children))
                 (let ((child (elt children i)))
                   (cond
                     ((not (eq (fxml.dom:node-type child) :text))
                       (setf previous nil)
                       (incf i))
                     ((and previous (eq (fxml.dom:node-type previous) :text))
                       (setf (slot-value previous 'value)
                             (concatenate 'rod
                               (fxml.dom:data previous)
                               (fxml.dom:data child)))
                       (fxml.dom:remove-child n child)
                       ;; not (incf i)
                       )
		     ((zerop (length (fxml.dom:data child)))
                       (fxml.dom:remove-child n child)
                       ;; not (incf i)
		       )
                     (t
                       (setf previous child)
                       (incf i)))))) 
             (map nil #'walk (fxml.dom:child-nodes n))))
    (walk node))
  (values))

;;; TEXT

(defmethod fxml.dom:split-text ((text text) offset)
  (assert-writeable text)
  (with-slots (owner parent value) text
    (unless (<= 0 offset (length value))
      (dom-error :INDEX_SIZE_ERR "offset is invalid"))
    (prog1
        (fxml.dom:insert-before parent
                           (fxml.dom:create-text-node owner (subseq value offset))
                           (fxml.dom:next-sibling text))
      (setf value (subseq value 0 offset)))))

;;; COMMENT -- nix
;;; CDATA-SECTION -- nix

;;; DOCUMENT-TYPE

(defmethod fxml.dom:internal-subset ((node document-type))
  ;; FIXME: encoding ist falsch, anderen sink nehmen!
  (if (and (slot-boundp node 'fxml.dom::%internal-subset)
	   ;; die damen und herren von der test suite sind wohl der meinung,
	   ;; dass ein leeres internal subset nicht vorhanden ist und
	   ;; wir daher nil liefern sollen.  bittesehr!
	   (fxml.dom::%internal-subset node))
      (let ((sink
	     #+rune-is-character (fxml:make-string-sink)
	     #-rune-is-character (fxml:make-string-sink/utf8)))
	(dolist (def (fxml.dom::%internal-subset node))
	  (apply (car def) sink (cdr def)))
	(fxml.sax:end-document sink))
      nil))

;;; NOTATION -- nix
;;; ENTITY -- nix

;;; ENTITY-REFERENCE

(defmethod initialize-instance :after ((instance entity-reference) &key)
  (let* ((owner (fxml.dom:owner-document instance))
         (handler (make-dom-builder))
         (resolver (slot-value owner 'entity-resolver)))
    (when resolver
      (setf (document handler) owner)
      (push instance (element-stack handler))
      #+fxml-system::utf8dom-file
      (setf handler (fxml:make-recoder handler #'fxml:rod-to-utf8-string))
      (funcall resolver (real-rod (fxml.dom:name instance)) handler)
      (flush-characters handler)))
  (labels ((walk (n)
             (setf (slot-value n 'read-only-p) t)
             (when (fxml.dom:element-p n)
	       (setf (slot-value (fxml.dom:attributes n) 'read-only-p) t)
               (map nil #'walk (fxml.dom:items (fxml.dom:attributes n))))
             (map nil #'walk (fxml.dom:child-nodes n))))
    (walk instance)))

;;; PROCESSING-INSTRUCTION

(defmethod (setf fxml.dom:data) (newval (self processing-instruction))
  (assert-writeable self)
  (setf newval (%rod newval))
  (setf (slot-value self 'data) newval))

;; das koennte man auch mit einer GF machen
(defun can-adopt-p (parent child)
  (member (fxml.dom:node-type child)
          (let ((default '(:element :processing-instruction :comment :text
                           :cdata-section :entity-reference)))
            (etypecase parent
              (document
                '(:element :processing-instruction :comment :document-type))
              (document-fragment default)
              (document-type nil)
              (entity-reference default)
              (element default)
              (attribute '(:text :entity-reference))
              (processing-instruction nil)
              (comment nil)
              (text nil)
              (cdata-section nil)
              (entity default)
              (notation nil)))))


;;; predicates

(defmethod fxml.dom:node-p ((object node)) t)
(defmethod fxml.dom:node-p ((object t)) nil)

(defmethod fxml.dom:document-p ((object document)) t)
(defmethod fxml.dom:document-p ((object t)) nil)

(defmethod fxml.dom:document-fragment-p ((object document-fragment)) t)
(defmethod fxml.dom:document-fragment-p ((object t)) nil)

(defmethod fxml.dom:character-data-p ((object character-data)) t)
(defmethod fxml.dom:character-data-p ((object t)) nil)

(defmethod fxml.dom:attribute-p ((object attribute)) t)
(defmethod fxml.dom:attribute-p ((object t)) nil)

(defmethod fxml.dom:element-p ((object element)) t)
(defmethod fxml.dom:element-p ((object t)) nil)

(defmethod fxml.dom:text-node-p ((object text)) t)
(defmethod fxml.dom:text-node-p ((object t)) nil)

(defmethod fxml.dom:comment-p ((object comment)) t)
(defmethod fxml.dom:comment-p ((object t)) nil)

(defmethod fxml.dom:cdata-section-p ((object cdata-section)) t)
(defmethod fxml.dom:cdata-section-p ((object t)) nil)

(defmethod fxml.dom:document-type-p ((object document-type)) t)
(defmethod fxml.dom:document-type-p ((object t)) nil)

(defmethod fxml.dom:notation-p ((object notation)) t)
(defmethod fxml.dom:notation-p ((object t)) nil)

(defmethod fxml.dom:entity-p ((object entity)) t)
(defmethod fxml.dom:entity-p ((object t)) nil)

(defmethod fxml.dom:entity-reference-p ((object entity-reference)) t)
(defmethod fxml.dom:entity-reference-p ((object t)) nil)

(defmethod fxml.dom:processing-instruction-p ((object processing-instruction)) t)
(defmethod fxml.dom:processing-instruction-p ((object t)) nil)

(defmethod fxml.dom:named-node-map-p ((object named-node-map)) t)
(defmethod fxml.dom:named-node-map-p ((object t)) nil)


;;; IMPORT-NODE

(defvar *clone-not-import* nil)         ;not beautiful, I know.  See below.

(defmethod import-node-internal (class document node deep &rest initargs)
  (let ((result (apply #'make-instance class :owner document initargs)))
    (when deep
      (dovector (child (fxml.dom:child-nodes node))
        (fxml.dom:append-child result (fxml.dom:import-node document child t))))
    result))

(defmethod fxml.dom:import-node ((document document) (node t) deep)
  (declare (ignore deep))
  (dom-error :NOT_SUPPORTED_ERR "not implemented"))

(defmethod fxml.dom:import-node ((document document) (node attribute) deep)
  (declare (ignore deep))
  (import-node-internal 'attribute
			document node
			t
                        :specified-p (fxml.dom:specified node)
			:name (fxml.dom:name node)
			:namespace-uri (fxml.dom:namespace-uri node)
			:local-name (fxml.dom:local-name node)
			:prefix (fxml.dom:prefix node)
			:owner-element nil))

(defmethod fxml.dom:import-node ((document document) (node document-fragment) deep)
  (import-node-internal 'document-fragment document node deep))

(defmethod fxml.dom:import-node ((document document) (node element) deep)
  (let* ((attributes (make-instance 'attribute-node-map
                       :element-type :attribute
                       :owner document))
         (result (import-node-internal 'element document node deep
                                       :attributes attributes
				       :namespace-uri (fxml.dom:namespace-uri node)
				       :local-name (fxml.dom:local-name node)
				       :prefix (fxml.dom:prefix node)
                                       :tag-name (fxml.dom:tag-name node))))
    (setf (slot-value attributes 'element) result)
    (dolist (attribute (fxml.dom:items (fxml.dom:attributes node)))
      (when (or (fxml.dom:specified attribute) *clone-not-import*)
        (let ((attr (fxml.dom:import-node document attribute t)))
          (if (fxml.dom:namespace-uri attribute)
              (fxml.dom:set-attribute-node-ns result attr)
              (fxml.dom:set-attribute-node result attr)))))
    (add-default-attributes result)
    result))

(defmethod fxml.dom:import-node ((document document) (node entity) deep)
  (import-node-internal 'entity document node deep
			:name (fxml.dom:name node)
                        :public-id (fxml.dom:public-id node)
                        :system-id (fxml.dom:system-id node)
                        :notation-name (fxml.dom:notation-name node)))

(defmethod fxml.dom:import-node ((document document) (node entity-reference) deep)
  (declare (ignore deep))
  (import-node-internal 'entity-reference document node nil
                        :name (fxml.dom:name node)))

(defmethod fxml.dom:import-node ((document document) (node notation) deep)
  (import-node-internal 'notation document node deep
                        :name (fxml.dom:name node)
                        :public-id (fxml.dom:public-id node)
                        :system-id (fxml.dom:system-id node)))

(defmethod fxml.dom:import-node
    ((document document) (node processing-instruction) deep)
  (import-node-internal 'processing-instruction document node deep
                        :target (fxml.dom:target node)
                        :data (fxml.dom:data node)))

;; TEXT_NODE, CDATA_SECTION_NODE, COMMENT_NODE
(defmethod fxml.dom:import-node
    ((document document) (node character-data) deep)
  (import-node-internal (class-of node) document node deep
                        :data (copy-seq (fxml.dom:data node))))

;;; CLONE-NODE
;;;
;;; As far as I can tell, cloneNode is the same as importNode, except
;;; for one difference involving element attributes: importNode imports
;;; only specified attributes, cloneNode copies even default values.
;;;
;;; Since I don't want to reimplement all of importNode here, we run
;;; importNode with a special flag...

(defmethod fxml.dom:clone-node ((node node) deep)
  (let ((*clone-not-import* t))
    (fxml.dom:import-node (fxml.dom:owner-document node) node deep)))

;; extension:
(defmethod fxml.dom:clone-node ((node document) deep)
  (let* ((document (make-instance 'document))
	 (original-doctype (fxml.dom:doctype node))
	 (doctype 
	  (when original-doctype
	    (make-instance 'document-type
	      :owner document
	      :name (fxml.dom:name original-doctype)
	      :public-id (fxml.dom:public-id original-doctype)
	      :system-id (fxml.dom:system-id original-doctype)
	      :notations (make-instance 'named-node-map
			   :element-type :notation
			   :owner document
			   :items (fxml.dom:items (fxml.dom:notations original-doctype)))
	      :entities (make-instance 'named-node-map
			  :element-type :entity
			  :owner document
			  :items (fxml.dom:items
				  (fxml.dom:entities original-doctype)))))))
    (setf (slot-value document 'owner) nil)
    (setf (slot-value document 'doc-type) doctype)
    (setf (slot-value document 'dtd) (dtd node))
    (setf (slot-value document 'entity-resolver)
	  (slot-value node 'entity-resolver))
    (setf (slot-value (fxml.dom:entities doctype) 'read-only-p) t)
    (setf (slot-value (fxml.dom:notations doctype) 'read-only-p) t)
    (when (and doctype (slot-boundp doctype 'fxml.dom::%internal-subset))
      (setf (fxml.dom::%internal-subset doctype)
	    (fxml.dom::%internal-subset original-doctype)))
    (when (and (fxml.dom:document-element node) deep)
      (let* ((*clone-not-import* t)
	     (clone (fxml.dom:import-node document (fxml.dom:document-element node) t)))
	(fxml.dom:append-child document clone)))
    document))


;;; Erweiterung

(defun create-document (&optional document-element)
  ;; Um ein neues Dokumentenobject zu erzeugen, parsen wir einfach ein
  ;; Dummydokument.
  (let* ((handler (make-dom-builder))
         (fxml::*ctx* (fxml::make-context :handler handler))
         (result
          (progn
            (fxml.sax:start-document handler)
            (fxml.sax:end-document handler))))
    (when document-element
      (fxml.dom:append-child result (fxml.dom:import-node result document-element t)))
    result))
