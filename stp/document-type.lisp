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


;;;; Class DOCUMENT-TYPE

(defgeneric root-element-name (document-type)
  (:documentation
   "@arg[document-type]{@class{document-type}}
    @return{string, a Name}
    @short{Returns the document-type's root-element-name.}"))

(defgeneric (setf root-element-name) (newval document-type)
  (:documentation
   "@arg[newval]{string, a Name}
    @arg[document-type]{@class{document-type}}
    @return{the root-element-name}
    @short{Sets the document-type's root-element-name.}"))

(defgeneric system-id (document-type)
  (:documentation
   "@arg[document-type]{@class{document-type}}
    @return{string suitable as a system ID}
    @short{Returns the document-type's system-id.}"))

(defgeneric (setf system-id) (newval document-type)
  (:documentation
   "@arg[newval]{string, suitable as a system ID}
    @arg[document-type]{@class{document-type}}
    @return{the system-id}
    @short{Sets the document-type's system-id.}"))

(defgeneric public-id (document-type)
  (:documentation
   "@arg[document-type]{@class{document-type}}
    @return{string suitable as a system ID}
    @short{Returns the document-type's public-id.}"))

(defgeneric (setf public-id) (newval document-type)
  (:documentation
   "@arg[newval]{string, suitable as a system ID}
    @arg[document-type]{@class{document-type}}
    @return{the public-id}
    @short{Sets the document-type's public-id.}"))

(defgeneric internal-subset (document-type)
  (:documentation
   "@arg[document-type]{@class{document-type}}
    @return{string, a well-formed internal subset}
    @short{Returns the document-type's internal subset as a string.}"))

(defgeneric (setf internal-subset) (newval document-type)
  (:documentation
   "@arg[newval]{string, a well-formed internal subset}
    @arg[document-type]{@class{document-type}}
    @return{the internal-subset}
    @short{Sets the document-type's internal subset.}"))


(defun make-document-type
    (root-element-name &optional system-id public-id internal-subset)
  "@arg[root-element-name]{string, a Name}
   @arg[system-id]{a string allowed as a system ID}
   @arg[public-id]{a string allowed as a public ID}
   @arg[internal-subset]{a well-formed internal subset as a string}
   @return{an @class{documen-type}}
   @short{This function creates a document-type node.}

   @see{document}"
  (let ((result (make-instance 'cxml-stp:document-type)))
    (setf (root-element-name result) root-element-name)
    (setf (system-id result) system-id)
    (setf (public-id result) public-id)
    (setf (internal-subset result) internal-subset)
    result))

(defmethod copy ((node cxml-stp:document-type))
  (let ((result (make-instance 'cxml-stp:document-type)))
    (setf (root-element-name result) (root-element-name node))
    (setf (system-id result) (system-id node))
    (setf (public-id result) (public-id node))
    (setf (internal-subset result) (internal-subset node))
    result))

(defun namep (str)
  (and (not (zerop (length str)))
       (cxml::name-start-rune-p (elt str 0))
       (every #'cxml::name-rune-p str)))

(defun check-xml-name (str)
  (unless (namep str)
    (stp-error "not a Name: ~S" str)))

(defmethod (setf root-element-name) :before (newval (node cxml-stp:document-type))
  (unless (zerop (length newval))
    (check-xml-name newval)
    (handler-case
	(cxml::split-qname newval)
      (cxml:well-formedness-violation ()
	(stp-error "not a QName: ~A" newval)))))

(defmethod (setf internal-subset) :around (newval (node cxml-stp:document-type))
  (setf newval (or newval ""))
  (unless (zerop (length newval))
    (handler-case
	(cxml:parse-rod
	 (concatenate 'string "<!DOCTYPE dummy [" newval "]><dummy/>")
	 nil)
      (cxml:well-formedness-violation (c)
	(stp-error "attempt to set internal subset to a value that is not ~
                    well-formed: ~A"
		   c))))
  (call-next-method newval node))

(defmethod (setf public-id) :around (newval (node cxml-stp:document-type))
  (when (equal newval "")
    (setf newval nil))
  (when (and newval (null (system-id node)))
    (stp-error "attempt to set public-id, but no system-id is set"))
  ;; zzz hier muss mehr geprueft werden?
  ;; was ist mit ' und " gleichzeitig?
  (unless (every #'cxml::pubid-char-p newval)
    (stp-error "malformed public id: ~S" newval))
  (call-next-method newval node))

(defmethod (setf system-id) :around (newval (node cxml-stp:document-type))
  (when (equal newval "")
    (setf newval nil))
  (when (and (public-id node) (null newval))
    (stp-error "attempt to remove system-id, but public-id is set"))
  (when (position #\# newval)
    (stp-error "attempt to use a system id with a fragment identifier"))
  (when (some (lambda (c) (> (char-code c) 126)) newval)
    (stp-error "non-ASCII characters in system id"))
  (when (and (position #\" newval) (position #\' newval))
    (stp-error "system id contains both single and double quote"))
  (call-next-method newval node))

(defmethod (setf dtd) :before (newval (node cxml-stp:document-type))
  (check-type newval (or cxml::dtd null)))

(defmethod string-value ((node cxml-stp:document-type))
  "")

;; for the XML test suite
;; doesn't actually work, since we don't record those notations anyway
(defvar *serialize-canonical-notations-only-p* nil)

(defclass notation-collector ()
  ((collected-notations :initform nil :accessor collected-notations)))

(defmethod sax:notation-declaration
    ((handler notation-collector) name public system)
  (push (list name public system) (collected-notations handler)))

(defmethod sax:end-document ((handler notation-collector))
  (collected-notations handler))

(defmethod serialize ((node cxml-stp:document-type) handler)
  (sax:start-dtd handler
		 (root-element-name node)
		 (public-id node)
		 (system-id node))
  (unless (zerop (length (internal-subset node)))
    (if *serialize-canonical-notations-only-p*
	(let ((notations
	       (cxml:parse-rod
		(concatenate 'string
			     "<!DOCTYPE dummy ["
			     (internal-subset node)
			     "]><dummy/>")
		(make-instance 'notation-collector))))
	  (when notations
	    (sax:start-internal-subset handler)
	    (loop
	       for (name public system)
	       in (sort notations #'string< :key #'car)
	       do
		 (sax:notation-declaration handler name public system))
	    (sax:end-internal-subset handler)))
	(sax:unparsed-internal-subset handler (internal-subset node))))
  (sax:end-dtd handler))


;;; printing

(defmethod slots-for-print-object append ((node cxml-stp:document-type))
  '((:root-element-name root-element-name)
    (:system-id system-id)
    (:public-id public-id)
    (:internal-subset internal-subset)))

(defreader cxml-stp:document-type
    (root-element-name system-id public-id internal-subset)
  (setf (root-element-name this) root-element-name)
  (setf (system-id this) system-id)
  (setf (public-id this) public-id)
  (setf (internal-subset this) internal-subset))
