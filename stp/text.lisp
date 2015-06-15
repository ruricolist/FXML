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


;;;; Class TEXT

(defun make-text (data)
  "@arg[data]{a string containing XML characters only}
   @return{an @class{text}}
   @short{This function creates a text node.}"
  (let ((result (make-instance 'text)))
    (setf (data result) data)
    result))

(defmethod copy ((node text))
  (make-instance 'text :data (data node)))

(defmethod string-value ((node text))
  (data node))

(defmethod (setf data) :around (newval (node text))
  (unless newval (setf newval ""))
  (unless (xml-characters-p newval)
    (stp-error "text includes characters that cannot be ~
                represented in XML at all: ~S"
	       newval))
  (call-next-method newval node))

(defmethod serialize ((node text) handler)
  (sax:characters handler (data node)))


;;; printing

(defmethod slots-for-print-object append ((node text))
  '((:data data)))

(defreader text (data)
  (setf (data this) data))
