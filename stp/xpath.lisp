;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-

;;; Copyright (c) 2007 Ivan Shvedunov. All rights reserved.
;;; Copyright (c) 2007 David Lichteblau. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(in-package :cxml-stp-impl)

(defun vector->pipe (vector &optional (start 0))
  (if (>= start (length vector))
      nil
      (xpath::make-pipe (elt vector start)
			(vector->pipe vector (1+ start)))))


;;;; XPath protocol implementation for STP

;;;; FIXME: xpath-protocol:child-pipe destructively normalizes the STP tree!

(define-default-method xpath-protocol:local-name ((node stp:node))
  (local-name node))

(define-default-method xpath-protocol:namespace-prefix ((node stp:node))
  (namespace-prefix node))

(define-default-method xpath-protocol:parent-node ((node stp:node))
  (stp:parent node))

(define-default-method xpath-protocol:namespace-uri ((node stp:node))
  (namespace-uri node))

(define-default-method xpath-protocol:qualified-name ((node stp:node))
  (qualified-name node))

(define-default-method xpath-protocol:base-uri ((node stp:element))
  (stp:base-uri node))

(define-default-method xpath-protocol:base-uri ((node stp:document))
  (stp:base-uri node))

(define-default-method xpath-protocol:base-uri ((node stp:node))
  nil)

(define-default-method xpath-protocol:child-pipe ((node stp:node))
  nil)

(define-default-method xpath-protocol:child-pipe ((node stp:document))
  (filter-children (alexandria:of-type '(not document-type)) node))

(define-default-method xpath-protocol:child-pipe ((node stp:element))
  (normalize-text-nodes! node)
  (vector->pipe (%children node)))

(define-default-method xpath-protocol:attribute-pipe ((node stp:node))
  nil)

(define-default-method xpath-protocol:attribute-pipe ((node stp:element))
  (list-attributes node))

(define-default-method xpath-protocol:namespace-pipe ((node stp:node))
  (when (stp:parent node)
    (xpath-protocol:namespace-pipe (stp:parent node))))

(defstruct (stp-namespace
	     (:constructor make-stp-namespace (parent prefix uri)))
  parent
  prefix
  uri)

(define-default-method xpath-protocol:node-equal
    ((a stp-namespace) (b stp-namespace))
  (and (eq (stp-namespace-parent a) (stp-namespace-parent b))
       (equal (stp-namespace-prefix a) (stp-namespace-prefix b))))

(define-default-method xpath-protocol:hash-key
    ((node stp-namespace))
  (cons (stp-namespace-parent node) (stp-namespace-prefix node)))

(define-default-method xpath-protocol:base-uri ((node stp-namespace))
  nil)

(define-default-method xpath-protocol:node-p ((node stp-namespace))
  t)

(define-default-method xpath-protocol:child-pipe ((node stp-namespace)) nil)
(define-default-method xpath-protocol:attribute-pipe ((node stp-namespace)) nil)
(define-default-method xpath-protocol:namespace-pipe ((node stp-namespace)) nil)

(define-default-method xpath-protocol:parent-node ((node stp-namespace))
  (stp-namespace-parent node))
(define-default-method xpath-protocol:local-name ((node stp-namespace))
  (stp-namespace-prefix node))
(define-default-method xpath-protocol:qualified-name ((node stp-namespace))
  (stp-namespace-prefix node))
(define-default-method xpath-protocol:namespace-uri ((node stp-namespace))
  "")

(define-default-method xpath-protocol:namespace-pipe
    ((original-node stp:element))
  (let ((node original-node)
	(table (make-hash-table :test 'equal))
	(current '()))
    (labels ((yield (prefix uri)
	       (unless (gethash prefix table)
		 (let ((nsnode (make-stp-namespace original-node prefix uri)))
		   (setf (gethash prefix table) nsnode)
		   (push nsnode current))))
	     (iterate ()
	       (if current
		   (cons (pop current) #'iterate)
		   (recurse)))
	     (recurse ()
	       (etypecase node
		 (null)
		 (stp:element
		   (let ((parent (stp:parent node)))
		     (map-extra-namespaces #'yield node)
		     (unless (and (zerop (length (%namespace-prefix node)))
				  (zerop (length (%namespace-uri node)))
				  (or (typep parent 'stp:document)
				      (zerop
				       (length
					(stp:find-namespace "" parent)))))
		       (yield (%namespace-prefix node)
			      (%namespace-uri node)))
		     (dolist (a (%attributes node))
		       (when (plusp (length (namespace-prefix a)))
			 (yield (namespace-prefix a) (namespace-uri a))))
		     (setf node parent))
		   (iterate))
		 (stp:document
		  (yield "xml" "http://www.w3.org/XML/1998/namespace")
		  #+nil (yield "xmlns" "http://www.w3.org/2000/xmlns/")
		  (setf node nil)
		  (iterate)))))
      (recurse))))

(define-default-method xpath-protocol:node-text ((node node))
  (string-value node))

(define-default-method xpath-protocol:node-text ((node stp-namespace))
  (stp-namespace-uri node))

(define-default-method xpath-protocol:node-p ((node node))
  t)

(define-default-method xpath-protocol:node-type-p ((node node) type)
  (declare (ignore type))
  nil)

(define-default-method xpath-protocol:node-type-p ((node stp-namespace) type)
  (declare (ignore type))
  nil)

(macrolet ((deftypemapping (class keyword)
	     `(define-default-method xpath-protocol:node-type-p
		  ((node ,class) (type (eql ,keyword)))
		t)))
  (deftypemapping document :document)
  (deftypemapping comment :comment)
  (deftypemapping processing-instruction :processing-instruction)
  (deftypemapping text :text)
  (deftypemapping attribute :attribute)
  (deftypemapping element :element)
  (deftypemapping stp-namespace :namespace))

(defun normalize-text-nodes! (node)
  (when (typep node 'stp:parent-node)
    (let ((children (%children node)))
      (when (and children (loop
			     for child across children
			     for a = nil then b
			     for b = (typep child 'text)
			     thereis
			       (and b (or a
					  (zerop (length (stp:data child)))))))
	(let ((previous nil)
	      (results '()))
	  (stp:do-children (child node)
	    (cond
	      ((not (typep child 'stp:text))
	       (when previous
		 (push (stp:make-text
			(apply #'concatenate 'string (nreverse previous)))
		       results)
		 (setf (%parent (car results)) node)
		 (setf previous nil))
	       (push child results))
	      (previous
	       (push (stp:data child) previous))
	      ((zerop (length (stp:data child))))
	      (t
	       (setf previous (list (stp:data child))))))
	  (when previous
	    (push (stp:make-text
		   (apply #'concatenate 'string (nreverse previous)))
		  results)
	    (setf (%parent (car results)) node))
	  (setf (cxml-stp-impl::%children node)
		(let ((n (length results)))
		  (make-array n
			      :fill-pointer n
			      :initial-contents (nreverse results)))))))))

(define-default-method xpath-protocol:get-element-by-id ((node stp:node) id)
  (let* ((document (stp:document node))
	 (dtd (when (stp:document-type document)
		(stp:dtd (stp:document-type document)))))
    (when dtd
      (block nil
	(flet ((test (node)
		 (when (typep node 'stp:element)
		   (let ((elmdef
			  (cxml::find-element (stp:qualified-name node) dtd)))
		     (when elmdef
		       (dolist (attdef (cxml::elmdef-attributes elmdef))
			 (when (eq :ID (cxml::attdef-type attdef))
			   (let* ((name (cxml::attdef-name attdef))
				  (value (stp:attribute-value node name)))
			     (when (and value (equal value id))
			       (return node))))))))))
	  (find-recursively-if #'test document))))))

(define-default-method xpath-protocol:unparsed-entity-uri
    ((node stp:node) name)
  (let ((doctype (stp:document-type (stp:document node))))
    (when doctype
      (let ((dtd (stp:dtd doctype)))
	(when dtd
	  (let ((entdef (cdr (gethash name (cxml::dtd-gentities dtd)))))
	    (when (typep entdef 'cxml::external-entdef)
	      (let ((uri (cxml::extid-system (cxml::entdef-extid entdef))))
		(when uri
		  (puri:render-uri uri nil))))))))))

(define-default-method xpath-protocol:local-name ((node stp:text)) "")

(define-default-method xpath-protocol:namespace-prefix ((node stp:text)) "")

(define-default-method xpath-protocol:namespace-uri ((node stp:text)) "")

(define-default-method xpath-protocol:qualified-name ((node stp:text)) "")

(define-default-method xpath-protocol:local-name ((node stp:comment)) "")

(define-default-method xpath-protocol:namespace-prefix ((node stp:comment)) "")

(define-default-method xpath-protocol:namespace-uri ((node stp:comment)) "")

(define-default-method xpath-protocol:qualified-name
    ((node stp:comment))
  "")

(define-default-method xpath-protocol:namespace-prefix
    ((node stp:processing-instruction))
  "")

(define-default-method xpath-protocol:local-name
    ((node stp:processing-instruction))
  (stp:target node))

(define-default-method xpath-protocol:qualified-name
    ((node stp:processing-instruction))
  (stp:target node))

(define-default-method xpath-protocol:namespace-uri
    ((node stp:processing-instruction))
  "")

(define-default-method xpath-protocol:namespace-uri
    ((node stp:document))
  "")

(define-default-method xpath-protocol:namespace-prefix ((node stp:document))
  "")

(define-default-method xpath-protocol:qualified-name ((node stp:document)) "")

(define-default-method xpath-protocol:local-name ((node stp:document)) "")

(define-default-method xpath-protocol:processing-instruction-target
    ((node stp:node))
  node)

(define-default-method xpath-protocol:processing-instruction-target
    ((node stp:processing-instruction))
  (stp:target node))

(defun run-xpath-tests ()
  (let ((xpath::*dom-builder* (stp:make-builder))
	(xpath::*document-element* #'stp:document-element))
    (xpath::run-all-tests)))
