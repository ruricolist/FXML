;;;; dom-builder.lisp -- DOM-building SAX handler
;;;;
;;;; This file is part of the FXML parser, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;; Author: Henrik Motakef <hmot@henrik-motakef.de>
;;;; Author: David Lichteblau <david@lichteblau.com>
;;;; Author: knowledgeTools Int. GmbH

#-rune-is-integer
(in-package :fxml.rune-dom)

#+rune-is-integer
(in-package :fxml.utf8-dom)

#-rune-is-integer
(in-readtable :runes)
#+rune-is-integer
(in-readtable :utf8-runes)


(defclass dom-builder (fxml.sax:content-handler)
  ((document      :initform nil :accessor document)
   (element-stack :initform '() :accessor element-stack)
   (internal-subset             :accessor internal-subset)
   (text-buffer   :initform nil :accessor text-buffer)))

(defun make-dom-builder ()
  (make-instance 'dom-builder))

(defun fast-push (new-element vector)
  (vector-push-extend new-element vector (max 1 (array-dimension vector 0))))

(defmethod fxml.sax:start-document ((handler dom-builder))
  (when (and fxml.sax:*namespace-processing*
	     (not (and fxml.sax:*include-xmlns-attributes*
		       fxml.sax:*use-xmlns-namespace*)))
    (error "SAX configuration is incompatible with FXML.DOM: *namespace-processing* is activated, but *include-xmlns-attributes* or *use-xmlns-namespace* are not"))
  (let ((document (make-instance 'document)))
    (setf (slot-value document 'owner) nil
	  (slot-value document 'doc-type) nil)
    (setf (document handler) document)
    (push document (element-stack handler))))

;; fixme
(defmethod fxml.sax::dtd ((handler dom-builder) dtd)
  (setf (slot-value (document handler) 'dtd) dtd))

(defmethod fxml.sax:end-document ((handler dom-builder))
  (let ((doctype (fxml.dom:doctype (document handler))))
    (when doctype
      (setf (slot-value (fxml.dom:entities doctype) 'read-only-p) t)
      (setf (slot-value (fxml.dom:notations doctype) 'read-only-p) t)))
  (document handler))

(defmethod fxml.sax:entity-resolver ((handler dom-builder) resolver)
  (setf (slot-value (document handler) 'entity-resolver) resolver))

(defmethod fxml.sax:start-dtd ((handler dom-builder) name publicid systemid)
  (let* ((document (document handler))
         (doctype (%create-document-type name publicid systemid)))
    (setf (slot-value doctype 'owner) document
	  (slot-value (fxml.dom:notations doctype) 'owner) document
	  (slot-value (fxml.dom:entities doctype) 'owner) document
	  (slot-value document 'doc-type) doctype)))

(defmethod fxml.sax:start-internal-subset ((handler dom-builder))
  (setf (internal-subset handler) nil))

(defmethod fxml.sax:end-internal-subset ((handler dom-builder))
  (setf (fxml.dom::%internal-subset (slot-value (document handler) 'doc-type))
	(nreverse (internal-subset handler)))
  (slot-makunbound handler 'internal-subset))

(macrolet ((defhandler (name &rest args)
	     `(defmethod ,name ((handler dom-builder) ,@args)
		(when (slot-boundp handler 'internal-subset)
		  (push (list ',name ,@args) (internal-subset handler))))))
  (defhandler fxml.sax:unparsed-entity-declaration
      name public-id system-id notation-name)
  (defhandler fxml.sax:external-entity-declaration
      kind name public-id system-id)
  (defhandler fxml.sax:internal-entity-declaration
      kind name value)
  (defhandler fxml.sax:notation-declaration
      name public-id system-id)
  (defhandler fxml.sax:element-declaration
      name model)
  (defhandler fxml.sax:attribute-declaration
      element-name attribute-name type default))

(defmethod fxml.sax:start-element
    ((handler dom-builder) namespace-uri local-name qname attributes)
  (check-type qname rod)		;catch recoder/builder mismatch
  (flush-characters handler)
  (with-slots (document element-stack) handler
    (let* ((nsp fxml.sax:*namespace-processing*)
	   (element (make-instance 'element 
                     :tag-name qname
                     :owner document
		     :namespace-uri (when nsp namespace-uri)
		     :local-name (when nsp local-name)
		     :prefix (%rod (when nsp (fxml::split-qname (real-rod qname))))))
	  (parent (car element-stack))
          (anodes '()))
      (dolist (attr attributes)
	(let ((anode
               (if nsp
		   (fxml.dom:create-attribute-ns document
					    (fxml.sax:attribute-namespace-uri attr)
					    (fxml.sax:attribute-qname attr))
		   (fxml.dom:create-attribute document (fxml.sax:attribute-qname attr))))
              (text
               (fxml.dom:create-text-node document (fxml.sax:attribute-value attr))))
          (setf (slot-value anode 'specified-p)
                (fxml.sax:attribute-specified-p attr))
	  (setf (slot-value anode 'owner-element) element)
          (fxml.dom:append-child anode text)
          (push anode anodes)))
      (setf (slot-value element 'parent) parent)
      (fast-push element (slot-value parent 'children))
      (let ((map
	      (make-instance 'attribute-node-map
		:items anodes
		:element-type :attribute
		:element element
		:owner document)))
	(setf (slot-value element 'attributes) map)
	(dolist (anode anodes)
	  (setf (slot-value anode 'map) map)))
      (push element element-stack))))

(defmethod fxml.sax:end-element ((handler dom-builder) namespace-uri local-name qname)
  (declare (ignore namespace-uri local-name qname))
  (flush-characters handler)
  (pop (element-stack handler)))

(defmethod fxml.sax:characters ((handler dom-builder) data)
  (with-slots (text-buffer) handler
    (cond
      ((null text-buffer)
       (setf text-buffer data))
      (t
       (unless (array-has-fill-pointer-p text-buffer)
	 (setf text-buffer (make-array (length text-buffer)
				       :element-type 'rune
				       :adjustable t
				       :fill-pointer t
				       :initial-contents text-buffer)))
       (let ((n (length text-buffer))
	     (m (length data)))
	 (adjust-vector-exponentially text-buffer (+ n m) t)
	 (move data text-buffer 0 n m))))))

(defun flush-characters (handler)
  (with-slots (document element-stack text-buffer) handler
    (let ((data text-buffer))
      (when data
	(when (array-has-fill-pointer-p data)
	  (setf data
		(make-array (length data)
			    :element-type 'rune
			    :initial-contents data)))
	(let ((parent (car element-stack)))
	  (if (eq (fxml.dom:node-type parent) :cdata-section)
	      (setf (fxml.dom:data parent) data)
	      (let ((node (fxml.dom:create-text-node document data)))
		(setf (slot-value node 'parent) parent)
		(fast-push node (slot-value (car element-stack) 'children)))))
	(setf text-buffer nil)))))

(defmethod fxml.sax:start-cdata ((handler dom-builder))
  (flush-characters handler)
  (with-slots (document element-stack) handler
    (let ((node (fxml.dom:create-cdata-section document #""))
          (parent (car element-stack)))
      (setf (slot-value node 'parent) parent)
      (fast-push node (slot-value parent 'children))
      (push node element-stack))))

(defmethod fxml.sax:end-cdata ((handler dom-builder))
  (flush-characters handler)
  (let ((node (pop (slot-value handler 'element-stack))))
    (assert (eq (fxml.dom:node-type node) :cdata-section))))

(defmethod fxml.sax:processing-instruction ((handler dom-builder) target data)
  (flush-characters handler)
  (with-slots (document element-stack) handler
    (let ((node (fxml.dom:create-processing-instruction document target data))
          (parent (car element-stack)))
      (setf (slot-value node 'parent) parent)
      (fast-push node (slot-value (car element-stack) 'children)))))

(defmethod fxml.sax:comment ((handler dom-builder) data)
  (flush-characters handler)
  (with-slots (document element-stack) handler
    (let ((node (fxml.dom:create-comment document data))
          (parent (car element-stack)))
      (setf (slot-value node 'parent) parent)
      (fast-push node (slot-value (car element-stack) 'children)))))

(defmethod fxml.sax:unparsed-entity-declaration
    ((handler dom-builder) name public-id system-id notation-name)
  (set-entity handler name public-id system-id notation-name))

(defmethod fxml.sax:external-entity-declaration
    ((handler dom-builder) kind name public-id system-id)
  (ecase kind
    (:general (set-entity handler name public-id system-id nil))
    (:parameter)))

(defmethod fxml.sax:internal-entity-declaration
    ((handler dom-builder) kind name value)
  (declare (ignore value))
  (ecase kind
    (:general (set-entity handler name nil nil nil))
    (:parameter)))

(defun set-entity (handler name pid sid notation)
  (fxml.dom:set-named-item (fxml.dom:entities (fxml.dom:doctype (document handler)))
                      (make-instance 'entity
                        :owner (document handler)
                        :name name
                        :public-id pid
                        :system-id sid
                        :notation-name notation)))

(defmethod fxml.sax:notation-declaration
    ((handler dom-builder) name public-id system-id)
  (fxml.dom:set-named-item (fxml.dom:notations (fxml.dom:doctype (document handler)))
                      (make-instance 'notation
                        :owner (document handler)
                        :name name
                        :public-id public-id
                        :system-id system-id)))
