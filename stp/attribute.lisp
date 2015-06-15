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

(in-package :cxml-stp-impl)

#+sbcl
(declaim (optimize (debug 2)))


;;;; Class ATTRIBUTE

(defgeneric value (attribute)
  (:documentation
   "@arg[attribute]{an @class{attribute}}
    @return{a string of XML characters}
    @short{Returns the attribute's value.}"))

(defgeneric (setf value) (newval attribute)
  (:documentation
   "@arg[newval]{a string of XML characters}
    @arg[attribute]{an @class{attribute}}
    @return{the value}
    @short{Sets the attribute's value.}"))

(defun make-attribute (value name &optional (uri ""))
  "@arg[value]{a string containing XML characters only}
   @arg[name]{a string, either a QName or an NCName}
   @arg[uri]{a string, the namespace URI}
   @return{an @class{attribute}}
   @short{This function creates an attribute node of the given value and name.}

   @see{element}"
  (let ((result (make-instance 'attribute)))
    (multiple-value-bind (prefix local-name)
	(cxml::split-qname name)
      (setf prefix (or prefix ""))
      (setf (local-name result) "tmp")
      (rename-attribute result prefix uri)
      (setf (local-name result) local-name)
      (setf (value result) value))
    result))

(defmethod copy ((node attribute))
  (let ((result (make-instance 'attribute)))
    (setf (%local-name result) (%local-name node))
    (setf (%namespace-prefix result) (%namespace-prefix node))
    (setf (%namespace-uri result) (%namespace-uri node))
    (setf (value result) (value node))
    result))

(defmethod detach ((node attribute))
  (when (parent node)
    (remove-attribute (parent node) node)))

(defmethod string-value ((node attribute))
  (value node))

(defun xml-characters-p (str)
  (declare (optimize speed (safety 0)))
  (every (lambda (c)
	   (let ((code (char-code c)))
	     (or (eql code 9)
		 (eql code 10)
		 (eql code 13)
		 (<= 32 code #xd7ff)
		 #+rune-is-utf-16 (<= #xD800 code #xDFFF)
		 (<= #xe000 code #xfffd)
		 #-rune-is-utf-16 (<= #x10000 code #x10ffff))))
	 (the string str)))

(defmethod (setf value) :before (newval (node attribute))
  (unless (xml-characters-p newval)
    (stp-error "new attribute value includes characters that cannot be ~
                represented in XML at all: ~S"
	       newval)))

(defmethod (setf local-name) (newval (node attribute))
  (check-nc-name newval)
  (when (and (equal newval "xmlns") (equal (stp:namespace-uri node) ""))
    (stp-error "attempt to represent a namespace declaration as an ATTRIBUTE"))
  (setf (%local-name node) newval))

(defun xor (a b)
  (if a (not b) b))

(defun rename-attribute (attribute prefix uri)
  "@arg[attribute]{the @class{attribute} to be renamed}
   @arg[prefix]{string, an NCName}
   @arg[uri]{a string, the namespace URI}
   @return{the attribute}
   @short{This function changed namespace prefix and URI of an attribute.}

   Since there is no default namespace for attributes, prefix and uri must
   be changed in the same step to rename an attribute with no namespace to
   an attribute with both namespace prefix and URI.

   @see{local-name}"
  (unless prefix (setf prefix ""))
  (unless uri (setf uri ""))
  (check-uri-like uri)
  (when (equal prefix "xmlns")
    (stp-error "attempt to represent a namespace declaration as an ATTRIBUTE"))
  (when (xor (equal uri "http://www.w3.org/XML/1998/namespace")
	     (equal prefix "xml"))
    (stp-error "prefix/URI mismatch for `xml' namespace"))
  (cond
    ((zerop (length prefix))
      (unless (zerop (length uri))
	(stp-error "attribute with URI but no prefix"))
     (when (equal (stp:local-name attribute) "xmlns")
       (stp-error
	"attempt to represent a namespace declaration as an ATTRIBUTE"))
      (values
       (setf (%namespace-prefix attribute) "")
       (setf (%namespace-uri attribute) "")))
    ((zerop (length uri))
      (stp-error "attribute with prefix but no URI"))
    (t
      (let ((parent (parent attribute)))
	(when parent
	  (let ((old (find-local-namespace prefix parent)))
	    (when (and old (not (equal uri old)))
	      (stp-error "conflicting namespaces when renaming attribute")))))
      (check-nc-name prefix)
      (check-namespace-uri uri)
      (values
       (setf (%namespace-prefix attribute) prefix)
       (setf (%namespace-uri attribute) uri))))
  attribute)

(defmethod serialize ((node attribute) handler)
  (stp-error "attempt to serialize an attribute in isolation"))


;;; printing

(defmethod slots-for-print-object append ((node attribute))
  '((:value value)))

(defreader attribute (value)
  (setf (value this) value))
