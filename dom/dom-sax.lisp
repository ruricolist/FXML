;;;; dom-sax.lisp -- DOM walker
;;;;
;;;; This file is part of the FXML parser, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: David Lichteblau <david@lichteblau.com>
;;;; Copyright (c) 2004 knowledgeTools Int. GmbH

(in-package :fxml)

(defun fxml.dom:map-document
    (handler document
     &key (include-xmlns-attributes fxml.sax:*include-xmlns-attributes*)
	  include-doctype
          include-default-values
	  (recode (and #+rune-is-integer (typep document 'fxml.utf8-dom::node))))
  (declare (ignorable recode))
  #+rune-is-integer
  (when recode
    (setf handler (make-recoder handler #'utf8-string-to-rod)))
  (fxml.sax:start-document handler)
  (when include-doctype
    (let ((doctype (fxml.dom:doctype document)))
      (when doctype
	(fxml.sax:start-dtd handler
		       (fxml.dom:name doctype)
		       (fxml.dom:public-id doctype)
		       (fxml.dom:system-id doctype))
	(ecase include-doctype
	  (:full-internal-subset
	    (when (slot-boundp doctype 'fxml.dom::%internal-subset)
	      (fxml.sax:start-internal-subset handler)
	      (dolist (def (fxml.dom::%internal-subset doctype))
		(apply (car def) handler (cdr def)))
	      (fxml.sax:end-internal-subset handler)))
	  (:canonical-notations
	    ;; need notations for canonical mode 2
	    (let* ((ns (fxml.dom:notations doctype))
		   (a (make-array (fxml.dom:length ns))))
	      (when (plusp (fxml.dom:length ns))
		(fxml.sax:start-internal-subset handler)
		;; get them
		(dotimes (k (fxml.dom:length ns))
		  (setf (elt a k) (fxml.dom:item ns k)))
		;; sort them 
		(setf a (sort a #'rod< :key #'fxml.dom:name))
		(loop for n across a do
		      (fxml.sax:notation-declaration handler
						(fxml.dom:name n)
						(fxml.dom:public-id n)
						(fxml.dom:system-id n)))
		(fxml.sax:end-internal-subset handler)))))
	(fxml.sax:end-dtd handler))))
  (labels ((walk (node)
             (fxml.dom:do-node-list (child (fxml.dom:child-nodes node))
               (ecase (fxml.dom:node-type child)
                 (:element
                   (let ((attlist
                          (compute-attributes child
                                              include-xmlns-attributes
                                              include-default-values))
			 (uri (fxml.dom:namespace-uri child))
                         (lname (fxml.dom:local-name child))
                         (qname (fxml.dom:tag-name child)))
                     (fxml.sax:start-element handler uri lname qname attlist)
                     (walk child)
                     (fxml.sax:end-element handler uri lname qname)))
                 (:cdata-section
                   (fxml.sax:start-cdata handler)
                   (fxml.sax:characters handler (fxml.dom:data child))
                   (fxml.sax:end-cdata handler))
                 (:text
                   (fxml.sax:characters handler (fxml.dom:data child)))
                 (:comment
                   (fxml.sax:comment handler (fxml.dom:data child)))
                 (:processing-instruction
                   (fxml.sax:processing-instruction handler
                                               (fxml.dom:target child)
                                               (fxml.dom:data child)))))))
    (walk document))
  (fxml.sax:end-document handler))

(defun compute-attributes (element xmlnsp defaultp)
  (let ((results '()))
    (fxml.dom:do-node-list (a (fxml.dom:attributes element))
      (when (and (or defaultp (fxml.dom:specified a))
                 (or xmlnsp (not (fxml::xmlns-attr-p (rod (fxml.dom:name a))))))
        (push
         (fxml.sax:make-attribute :qname (fxml.dom:name a)
                             :value (fxml.dom:value a)
			     :local-name (fxml.dom:local-name a)
			     :namespace-uri (fxml.dom:namespace-uri a)
                             :specified-p (fxml.dom:specified a))
         results)))
    (reverse results)))
