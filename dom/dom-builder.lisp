;;;; dom-builder.lisp -- DOM-building SAX handler
;;;;
;;;; This file is part of the CXML parser, released under (L)LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;; Author: Henrik Motakef <hmot@henrik-motakef.de>
;;;; Author: David Lichteblau <david@lichteblau.com>
;;;; Author: knowledgeTools Int. GmbH

(in-package :dom-impl)

(defclass dom-builder ()
  ((document      :initform nil :accessor document)
   (element-stack :initform '() :accessor element-stack)))

(defun dom:make-dom-builder ()
  (make-instance 'dom-builder))

(defun fast-push (new-element vector)
  (vector-push-extend new-element vector (max 1 (array-dimension vector 0))))

(defmethod sax:start-document ((handler dom-builder))
  (let ((document (make-instance 'dom-impl::document)))
    (setf (slot-value document 'dom-impl::owner) nil
	  (slot-value document 'dom-impl::doc-type) nil)
    (setf (document handler) document)
    (push document (element-stack handler))))

(defmethod sax:end-document ((handler dom-builder))
  (setf (slot-value (document handler) 'dtd) (cxml::dtd cxml::*ctx*))
  (let ((doctype (dom:doctype (document handler))))
    (when doctype
      (setf (slot-value (dom:entities doctype) 'read-only-p) t)
      (setf (slot-value (dom:notations doctype) 'read-only-p) t)))
  (document handler))

(defmethod sax:entity-resolver ((handler dom-builder) resolver)
  (setf (slot-value (document handler) 'entity-resolver) resolver))

(defmethod sax:start-dtd ((handler dom-builder) name publicid systemid)
  (declare (ignore publicid systemid))
  (let* ((document (document handler))
         (doctype (make-instance 'dom-impl::document-type
                    :name name
                    :notations (make-instance 'dom-impl::named-node-map
                                 :element-type :notation
                                 :owner document)
                    :entities (make-instance 'dom-impl::named-node-map
                                :element-type :entity
                                :owner document))))
    (setf (slot-value doctype 'dom-impl::owner) document
	  (slot-value document 'dom-impl::doc-type) doctype)))

(defmethod sax:start-element
    ((handler dom-builder) namespace-uri local-name qname attributes)
  (declare (ignore namespace-uri local-name))
  (with-slots (document element-stack) handler
    (let ((element (make-instance 'element 
                     :tag-name qname
                     :owner document))
	  (parent (car element-stack))
          (anodes '()))
      (dolist (attr attributes)
	(let ((anode
               (dom:create-attribute document (sax:attribute-qname attr)))
              (text
               (dom:create-text-node document (sax:attribute-value attr))))
          (setf (slot-value anode 'dom-impl::specified-p)
                (sax:attribute-specified-p attr))
          (dom:append-child anode text)
          (push anode anodes)))
      (setf (slot-value element 'dom-impl::parent) parent)
      (fast-push element (slot-value parent 'dom-impl::children))
      (setf (slot-value element 'dom-impl::attributes)
            (make-instance 'attribute-node-map
              :items anodes
              :element-type :attribute
              :element element
              :owner document))
      (push element element-stack))))

(defmethod sax:end-element ((handler dom-builder) namespace-uri local-name qname)
  (declare (ignore namespace-uri local-name qname))
  (pop (element-stack handler)))

(defmethod sax:characters ((handler dom-builder) data)
  (with-slots (document element-stack) handler
    (let* ((parent (car element-stack))
           (last-child (dom:last-child parent)))
      (cond
        ((eq (dom:node-type parent) :cdata-section)
          (setf (dom:data parent) data))
        ((and last-child (eq (dom:node-type last-child) :text))
          ;; um entities herum wird SAX:CHARACTERS mehrfach aufgerufen fuer
          ;; den gleichen Textknoten.  Hier muessen wir den bestehenden Knoten
          ;; erweitern, sonst ist das Dokument nicht normalisiert.
          ;; (XXX Oder sollte man besser den Parser entsprechend aendern?)
          (dom:append-data last-child data))
        (t
          (let ((node (dom:create-text-node document data)))
            (setf (slot-value node 'dom-impl::parent) parent)
            (fast-push node (slot-value (car element-stack) 'dom-impl::children))))))))

(defmethod sax:start-cdata ((handler dom-builder))
  (with-slots (document element-stack) handler
    (let ((node (dom:create-cdata-section document #""))
          (parent (car element-stack)))
      (setf (slot-value node 'dom-impl::parent) parent)
      (fast-push node (slot-value parent 'dom-impl::children))
      (push node element-stack))))

(defmethod sax:end-cdata ((handler dom-builder))
  (let ((node (pop (slot-value handler 'element-stack))))
    (assert (eq (dom:node-type node) :cdata-section))))

(defmethod sax:processing-instruction ((handler dom-builder) target data)
  (with-slots (document element-stack) handler
    (let ((node (dom:create-processing-instruction document target data))
          (parent (car element-stack)))
      (setf (slot-value node 'dom-impl::parent) parent)
      (fast-push node (slot-value (car element-stack) 'dom-impl::children)))))

(defmethod sax:comment ((handler dom-builder) data)
  (with-slots (document element-stack) handler
    (let ((node (dom:create-comment document data))
          (parent (car element-stack)))
      (setf (slot-value node 'dom-impl::parent) parent)
      (fast-push node (slot-value (car element-stack) 'dom-impl::children)))))

(defmethod sax:unparsed-entity-declaration
    ((handler dom-builder) name public-id system-id notation-name)
  (set-entity handler name public-id system-id notation-name))

(defmethod sax:external-entity-declaration
    ((handler dom-builder) kind name public-id system-id)
  (ecase kind
    (:general (set-entity handler name public-id system-id nil))
    (:parameter)))

(defmethod sax:internal-entity-declaration
    ((handler dom-builder) kind name value)
  (declare (ignore value))
  (ecase kind
    (:general (set-entity handler name nil nil nil))
    (:parameter)))

(defun set-entity (handler name pid sid notation)
  (dom:set-named-item (dom:entities (dom:doctype (document handler)))
                      (make-instance 'dom-impl::entity
                        :owner (document handler)
                        :name name
                        :public-id pid
                        :system-id sid
                        :notation-name notation)))

(defmethod sax:notation-declaration
    ((handler dom-builder) name public-id system-id)
  (dom:set-named-item (dom:notations (dom:doctype (document handler)))
                      (make-instance 'dom-impl::notation
                        :owner (document handler)
                        :name name
                        :public-id public-id
                        :system-id system-id)))
