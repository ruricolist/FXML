;;; -*- Mode: Lisp; readtable: runes; -*-
;;;  (c) copyright 2007 David Lichteblau

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :cxml)

(defclass klacks:source () ())

(defgeneric klacks:close-source (source))

(defgeneric klacks:peek (source))
(defgeneric klacks:peek-value (source))
(defgeneric klacks:consume (source))

(defgeneric klacks:map-attributes (fn source))
(defgeneric klacks:list-attributes (source))
;;;(defgeneric klacks:current-uri (source))
;;;(defgeneric klacks:current-lname (source))
;;;(defgeneric klacks:current-qname (source))
;;;(defgeneric klacks:current-characters (source))
(defgeneric klacks:current-cdata-section-p (source))

(defmacro klacks:with-open-source ((var source) &body body)
  `(let ((,var ,source))
     (unwind-protect
	 (progn ,@body)
       (klacks:close-source ,var))))

(defun klacks:current-uri (source)
  (multiple-value-bind (key uri lname qname) (klacks:peek source)
    (declare (ignore lname qname))
    (check-type key (member :start-element :end-element))
    uri))

(defun klacks:current-lname (source)
  (multiple-value-bind (key uri lname qname) (klacks:peek source)
    (declare (ignore uri qname))
    (check-type key (member :start-element :end-element))
    lname))

(defun klacks:current-qname (source)
  (multiple-value-bind (key uri lname qname) (klacks:peek source)
    (declare (ignore uri lname))
    (check-type key (member :start-element :end-element))
    qname))

(defun klacks:current-characters (source)
  (multiple-value-bind (key characters) (klacks:peek source)
    (check-type key (member :characters))
    characters))

(defun klacks:serialize-source (source handler)
  (loop
    (multiple-value-bind (key a b c) (klacks:peek source)
      (case key
	(:start-document
	  (sax:start-document handler))
	(:characters
	  (cond
	    ((klacks:current-cdata-section-p source)
	      (sax:start-cdata source)
	      (sax:characters handler a)
	      (sax:end-cdata source))
	    (T
	      (sax:characters handler a))))
	(:processing-instruction
	  (sax:processing-instruction handler a b))
	(:comment
	  (sax:comment handler a))
	(:dtd
	  (sax:start-dtd handler a b c)
	  (sax:end-dtd handler))
	(:start-element
	  (sax:start-element handler a b c (klacks:list-attributes source)))
	(:end-element
	  (sax:end-element handler a b c))
	(:end-document
	  (return (sax:end-document handler)))
	(t
	  (error "unexpected klacks key: ~A" key)))
      (klacks:consume source))))
