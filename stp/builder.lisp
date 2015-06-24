;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-

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

(in-package :fxml.stp.impl)

#+sbcl
(declaim (optimize (debug 2)))

(defun make-builder ()
  "@return{STP builder, a SAX handler}
   @short{This function creates SAX handler that constructs an STP document.}

   The builder processes SAX events and can be used with any
   function generating such events, in particular with fxml:parse-file.

   Examples. Parsing a file:
   @begin{pre}(fxml:parse #p\"example.xml\" (fxml.stp:make-builder))@end{pre}
   Parsing a string:
   @begin{pre}(fxml:parse \"<example/>\" (fxml.stp:make-builder))@end{pre}

   @see{serialize}"
  (make-instance 'builder))

(defun make-fragment-builder ()
  "Like `make-builder', but return a document fragment instead of a document."
  (make-instance 'builder :document-class 'document-fragment))

(defclass builder (fxml.sax:content-handler)
  ((nodes :initform nil :accessor builder-nodes)
   (doctype :initform nil :accessor builder-doctype)
   (namespace-declarations :initform nil :accessor namespace-declarations)
   (internal-subset-sink :initform nil
			 :accessor builder-internal-subset-sink)
   (document-class :initarg :document-class
                   :reader builder.document-class))
  (:default-initargs :document-class 'document))

(defmethod fxml.sax:start-document ((builder builder))
  (let ((document (builder.document-class builder)))
    (push (make-instance document) (builder-nodes builder))))

(defun builder-append (builder x)
  (let ((parent (car (builder-nodes builder))))
    (%unchecked-insert-child parent x (length (%children parent)))))

(defmethod fxml.sax:start-dtd ((builder builder) name publicid systemid)
  (setf (builder-doctype builder)
	(make-document-type name systemid publicid ""))
  (builder-append builder (builder-doctype builder)))

(defmethod fxml.sax:start-internal-subset ((builder builder))
  (setf (builder-internal-subset-sink builder) (fxml:make-string-sink)))

(macrolet ((def (name &rest args)
	     `(defmethod ,name ((builder builder) ,@args)
		(let ((sink (builder-internal-subset-sink builder)))
		  (when sink (,name sink ,@args))))))
  (def fxml.sax:unparsed-entity-declaration name public-id system-id notation-name)
  (def fxml.sax:external-entity-declaration kind name public-id system-id)
  (def fxml.sax:internal-entity-declaration kind name value)
  (def fxml.sax:notation-declaration name public-id system-id)
  (def fxml.sax:element-declaration name model)
  (def fxml.sax:attribute-declaration element-name attribute-name type default))

(defmethod fxml.sax:end-internal-subset ((builder builder))
  (setf (internal-subset (builder-doctype builder))
	(string-trim "[]"
		     (fxml.sax:end-document
		      (builder-internal-subset-sink builder))))
  (setf (builder-internal-subset-sink builder) nil))

(defmethod fxml.sax:dtd ((builder builder) dtd)
  (when (builder-doctype builder)
    (setf (dtd (builder-doctype builder)) dtd)))

(defmethod fxml.sax:start-prefix-mapping ((builder builder) prefix uri)
  (push (cons (or prefix "") uri) (namespace-declarations builder)))

(defmethod fxml.sax:start-element ((builder builder) uri lname qname attrs)
  (let ((element (make-element qname uri)))
    (setf (%base-uri element) (fxml.sax:xml-base builder))
    (dolist (a attrs)
      (let ((uri (fxml.sax:attribute-namespace-uri a)))
	(unless (equal uri "http://www.w3.org/2000/xmlns/")
	  (let ((b (make-attribute (fxml.sax:attribute-value a)
				   (fxml.sax:attribute-qname a)
				   uri)))
	    (add-attribute element b)))))
    (builder-append builder element)
    (loop for (prefix . uri) in (namespace-declarations builder) do
	 (unless (find-namespace prefix element)
	   (add-extra-namespace element prefix uri)))
    (setf (namespace-declarations builder) nil)
    (push element (builder-nodes builder))))

(defmethod fxml.sax:end-element ((builder builder) uri lname qname)
  (declare (ignore uri lname qname))
  (pop (builder-nodes builder)))

;; zzz normalisieren?
(defmethod fxml.sax:characters ((builder builder) data)
  (builder-append builder (make-text data)))

(defmethod fxml.sax:processing-instruction ((builder builder) target data)
  (builder-append builder (make-processing-instruction target data)))

(defmethod fxml.sax:comment ((builder builder) data)
  (builder-append builder (make-comment data)))

(defmethod fxml.sax:end-document ((builder builder))
  (pop (builder-nodes builder)))
