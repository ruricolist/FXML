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


;;;; Class COMMENT

(defgeneric data (node)
  (:documentation
   "@arg[node]{a @class{comment}, @class{processing-instruction},
      or @class{text}}
    @return{a string of XML characters}
    @short{Returns the node's data.}"))

(defgeneric (setf data) (newval node)
  (:documentation
   "@arg[newval]{a string of XML characters}
    @arg[node]{a @class{comment}, @class{processing-instruction},
      or @class{text}}
    @return{the data}
    @short{Sets the node's data.}"))

(defun make-comment (data)
  "@arg[data]{a string containing XML characters only}
   @return{an @class{comment}}
   @short{This function creates a comment node.}

   @code{data} must not contain two consecutive dashes, or a dash
   at the end."
  (let ((result (make-instance 'comment)))
    (setf (data result) data)
    result))

(defmethod copy ((node comment))
  (make-instance 'comment :data (data node)))

(defmethod string-value ((node comment))
  (data node))

(defmethod (setf data) :around (newval (node comment))
  (unless newval (setf newval ""))
  (unless (xml-characters-p newval)
    (stp-error "comment data includes characters that cannot be ~
                represented in XML at all: ~S"
	       newval))
  (when (search "--" newval)
    (stp-error "forbidden -- in comment"))
  (when (alexandria:ends-with #\- newval)
    (stp-error "- at end of comment"))
  (call-next-method newval node))

(defmethod serialize ((node comment) handler)
  (sax:comment handler (data node)))


;;; printing

(defmethod slots-for-print-object append ((node comment))
  '((:data data)))

(defreader comment (data)
  (setf (data this) data))
