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


;;;; Class PROCESSING-INSTRUCTION

(defgeneric target (processing-instruction)
  (:documentation
   "@arg[processing-instruction]{@class{processing-instruction}}
    @return{string, a Name}
    @short{Returns the processing-instruction's target.}"))

(defgeneric (setf target) (newval processing-instruction)
  (:documentation
   "@arg[newval]{string, a Name}
    @arg[processing-instruction]{@class{processing-instruction}}
    @return{the target}
    @short{Sets the processing-instruction's target.}"))

(defun make-processing-instruction (target data)
  "@arg[target]{string, an NCName}
   @arg[data]{string containing XML characters only}
   @return{an @class{processing-instruction}}
   @short{This function creates a processing instruction.}

   @code{target} must not equal \"xml\".

   @code{data} must not contain the substring \"?>\"."
  (let ((result (make-instance 'processing-instruction)))
    (setf (target result) target)
    (setf (data result) data)
    result))

(defmethod copy ((node processing-instruction))
  (make-instance 'processing-instruction
    :target (target node)
    :data (data node)))

(defmethod string-value ((node processing-instruction))
  (data node))

(defmethod (setf target) :before (newval (node processing-instruction))
  (check-nc-name newval)
  (when (string-equal newval "xml")
    (stp-error "attempt to pretend that a PI is an XMLDecl")))

(defmethod (setf data) :around (newval (node processing-instruction))
  (unless newval (setf newval ""))
  (unless (xml-characters-p newval)
    (stp-error "Processing instruction data includes characters that ~
                cannot be represented in XML at all: ~S"
	       newval))
  (when (search "?>" newval)
    (stp-error "forbidden -- in processing-instruction"))
  (when (or (alexandria:starts-with 9 newval :key #'char-code)
	    (alexandria:starts-with 10 newval :key #'char-code)
	    (alexandria:starts-with 13 newval :key #'char-code)
	    (alexandria:starts-with 32 newval :key #'char-code))
    (stp-error "space at beginning of processing instruction data"))
  (call-next-method newval node))

(defmethod serialize ((node processing-instruction) handler)
  (sax:processing-instruction handler (target node) (data node)))


;;; printing

(defmethod slots-for-print-object append ((node processing-instruction))
  '((:data data)
    (:target target)))

(defreader processing-instruction (data target)
  (setf (data this) data)
  (setf (target this) target))
