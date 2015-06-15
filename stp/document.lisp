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


;;;; Class DOCUMENT

(defun make-document (document-element)
  "@arg[document-element]{an @class{element}}
   @return{an @class{document}}
   @short{This function creates document.}

   The given element is used as the document's only initial child."
  (check-type document-element element)
  (let ((result (make-instance 'document)))
    (insert-child result document-element 0)
    result))

(defmethod copy ((node document))
  (let ((result (make-instance 'document)))
    (insert-child result (copy (document-element node)) 0)
    ;; zzz das ist doch nicht schoen so
    (let ((i 0))
      (do-children (child node)
	(unless (typep child 'element)
	  (insert-child result (copy child) i))
	(incf i)))
    (setf (%base-uri result) (%base-uri node))
    result))

(defun assert-orphan (node)
  (when (parent node)
    (stp-error "node already has a parent: ~A" node)))

(defmethod check-insertion-allowed ((parent document) child i)
  (assert-orphan child)
  (typecase child
    ((or comment processing-instruction))
    (cxml-stp:document-type
     (when (stp:document-type parent)
       (stp-error "attempt to insert multiple document types"))
     (let ((j (child-position-if (alexandria:of-type 'element) parent)))
       (unless (<= i j)
	 (stp-error
	  "attempt to insert document type after document element"))))
    (element
     (when (some (alexandria:of-type 'element) (%children parent))
       (stp-error "attempt to insert multiple document elements")))
    (t
     (stp-error "not a valid child of a document: ~A" child))))

(defmethod check-deletion-allowed ((parent document) (child node) i)
  nil)
(defmethod check-deletion-allowed ((parent document) (child element) i)
  (stp-error "attempt to remove document element"))

(defmethod replace-child ((parent document) old-child new-child)
  (cond
    ((and (eq old-child (document-element parent))
	  (typep new-child 'element))
      (setf (document-element parent) new-child))
    ((and (eq old-child (stp:document-type parent))
	  (typep new-child 'cxml-stp:document-type))
      (setf (stp:document-type parent) new-child))
    (t
      (call-next-method))))

(defun cxml-stp:document-type (document)
  "@arg[document]{a @class{document}}
   @return{a @class{document-type}, or nil}
   This function returns the child node that is a document type, or nil.
   @see{document-element}"
  (find-if (alexandria:of-type 'cxml-stp:document-type) (%children document)))

;; zzz gefaellt mir nicht
(defun (setf cxml-stp:document-type) (newval document)
  (check-type newval cxml-stp:document-type)
  (let ((old (cxml-stp:document-type document)))
    (unless (eq newval old)
      (assert-orphan newval)
      (if old
	  (let ((pos (position old (%children document))))
	    (delete-nth-child pos document)
	    (insert-child document newval pos))
	  (insert-child document newval 0)))))

(defun document-element (document)
  "@arg[document]{a @class{document}}
   @return{an @class{element}}
   This function returns the child node that is an element.
   @see{document-type}"
  (find-if (alexandria:of-type 'element) (%children document)))

;; zzz gefaellt mir nicht
(defun (setf document-element) (newval document)
  (check-type newval element)
  (let ((old (document-element document)))
    (unless (eq newval old)
      (assert-orphan newval)
      (let ((pos (position old (%children document))))
	(%nuke-nth-child document pos)
	(insert-child document newval pos)))))

(defmethod base-uri ((document document))
  (%base-uri document))

(defmethod (setf base-uri) (newval (document document))
  (setf (%base-uri document) newval))

(defmethod string-value ((node document))
  (string-value (document-element node)))

(defmethod serialize ((node document) handler)
  (sax:start-document handler)
  (map nil (lambda (x) (serialize x handler)) (%children node))
  (sax:end-document handler))

(defreader document ())
