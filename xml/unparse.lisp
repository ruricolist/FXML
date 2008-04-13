;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CXML; readtable: runes; Encoding: utf-8; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Unparse XML
;;;     Title: (including support for canonic XML according to J.Clark)
;;;   Created: 1999-09-09
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;    Author: David Lichteblau <david@lichteblau.com>
;;;   License: Lisp-LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1999 by Gilbert Baumann
;;;  (c) copyright 2004 by knowledgeTools Int. GmbH
;;;  (c) copyright 2004 by David Lichteblau (for headcraft.de)
;;;  (c) copyright 2005-2008 by David Lichteblau

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

;; 
;; | Canonical XML
;; | =============
;; |                                      
;; | This document defines a subset of XML called canonical XML. The
;; | intended use of canonical XML is in testing XML processors, as a
;; | representation of the result of parsing an XML document.
;; | 
;; | Every well-formed XML document has a unique structurally equivalent
;; | canonical XML document. Two structurally equivalent XML documents have
;; | a byte-for-byte identical canonical XML document. Canonicalizing an
;; | XML document requires only information that an XML processor is
;; | required to make available to an application.
;; | 
;; | A canonical XML document conforms to the following grammar:
;; |
;; |    CanonXML    ::= Pi* element Pi*
;; |    element     ::= Stag (Datachar | Pi | element)* Etag
;; |    Stag        ::= '<'  Name Atts '>'
;; |    Etag        ::= '</' Name '>'
;; |    Pi          ::= '<?' Name ' ' (((Char - S) Char*)? - (Char* '?>' Char*)) '?>'
;; |    Atts        ::= (' ' Name '=' '"' Datachar* '"')*
;; |    Datachar    ::= '&amp;' | '&lt;' | '&gt;' | '&quot;'
;; |                     | '&#9;'| '&#10;'| '&#13;'
;; |                     | (Char - ('&' | '<' | '>' | '"' | #x9 | #xA | #xD))
;; |    Name        ::= (see XML spec)
;; |    Char        ::= (see XML spec)
;; |    S           ::= (see XML spec)
;; |
;; | Attributes are in lexicographical order (in Unicode bit order).
;; |  
;; | A canonical XML document is encoded in UTF-8.
;; |  
;; | Ignorable white space is considered significant and is treated
;; | equivalently to data.
;;
;; -- James Clark (jjc@jclark.com)


;;;; SINK: an xml output sink

(defclass sink (sax:content-handler)
    ((ystream :initarg :ystream :accessor sink-ystream)
     (width :initform 79 :initarg :width :accessor width)
     (canonical :initform nil :initarg :canonical :accessor canonical)
     (indentation :initform nil :initarg :indentation :accessor indentation)
     (current-indentation :initform 0 :accessor current-indentation)
     (notations :initform (make-buffer :element-type t) :accessor notations)
     (name-for-dtd :accessor name-for-dtd)
     (previous-notation :initform nil :accessor previous-notation)
     (have-doctype :initform nil :accessor have-doctype)
     (have-internal-subset :initform nil :accessor have-internal-subset)
     (stack :initform nil :accessor stack)
     (sink-omit-xml-declaration-p :initform nil
				  :initarg :omit-xml-declaration-p
				  :accessor sink-omit-xml-declaration-p)
     (encoding :initarg :encoding :reader sink-encoding)))

#-rune-is-character
(defmethod hax:%want-strings-p ((handler sink))
  nil)

(defmethod initialize-instance :after ((instance sink) &key)
  (when (eq (canonical instance) t)
    (setf (canonical instance) 1))
  (unless (member (canonical instance) '(nil 1 2))
    (error "Invalid canonical form: ~A" (canonical instance)))
  (when (and (canonical instance) (indentation instance))
    (error "Cannot indent XML in canonical mode"))
  (when (and (canonical instance)
	     (not (eq (ystream-encoding (sink-ystream instance)) :utf-8)))
    (error "Cannot use non-UTF-8 encoding in canonical mode"))
  (when (let ((encoding (ystream-encoding (sink-ystream instance))))
	  (and (not (symbolp encoding))
	       (eq (babel-encodings:enc-name encoding) :utf-16)))
    (sink-write-rune #/U+FEFF instance)))

(defun make-buffer (&key (element-type '(unsigned-byte 8)))
  (make-array 1
              :element-type element-type
              :adjustable t
              :fill-pointer 0))

(defun find-output-encoding (name)
  (when (stringp name)
    (setf name (find-symbol (string-upcase name) :keyword)))
  (cond
    ((null name)
     (warn "Unknown encoding ~A, falling back to UTF-8" name)
     :utf-8)
    ((find name '(:utf-8 :utf_8 :utf8))
     :utf-8)
    #-rune-is-character
    (t
     (warn "Unknown encoding ~A, falling back to UTF-8" name)
     :utf-8)
    #+rune-is-character
    (t
     (handler-case
	 (babel-encodings:get-character-encoding name)
       (error ()
	 (warn "Unknown encoding ~A, falling back to UTF-8" name)
	 :utf-8)))))

;; bisschen unschoen hier die ganze api zu duplizieren, aber die
;; ystreams sind noch undokumentiert
(macrolet ((define-maker (make-sink make-ystream &rest args)
	     `(defun ,make-sink (,@args &rest initargs
				        &key encoding &allow-other-keys)
		(let* ((encoding (or encoding "UTF-8"))
		       (ystream (,make-ystream ,@args)))
		  (setf (ystream-encoding ystream)
			(find-output-encoding encoding))
		  (apply #'make-instance
			 'sink
			 :ystream ystream
			 :encoding encoding
			 initargs)))))
  (define-maker make-octet-vector-sink make-octet-vector-ystream)
  (define-maker make-octet-stream-sink make-octet-stream-ystream stream)
  (define-maker make-rod-sink make-rod-ystream)

  #+rune-is-character
  (define-maker make-character-stream-sink make-character-stream-ystream stream)

  #-rune-is-character
  (define-maker make-string-sink/utf8 make-string-ystream/utf8)

  #-rune-is-character
  (define-maker make-character-stream-sink/utf8
      make-character-stream-ystream/utf8
    stream))

#+rune-is-character
(defun make-string-sink (&rest args) (apply #'make-rod-sink args))


(defmethod sax:end-document ((sink sink))
  (close-ystream (sink-ystream sink)))


;;;; doctype and notations

(defmethod sax:start-document ((sink sink))
  (unless (or (canonical sink)
	      (sink-omit-xml-declaration-p sink))
    (sink-write-rod #"<?xml version=\"1.0\" encoding=\"" sink)
    (sink-write-rod (rod (sink-encoding sink)) sink)
    (sink-write-rod #"\"?>" sink)
    (sink-write-rune #/U+000A sink)))

(defmethod sax:start-dtd ((sink sink) name public-id system-id)
  (setf (name-for-dtd sink) name)
  (unless (canonical sink)
    (ensure-doctype sink public-id system-id)))

(defun ensure-doctype (sink &optional public-id system-id)
  (unless (have-doctype sink)
    (setf (have-doctype sink) t)
    (sink-write-rod #"<!DOCTYPE " sink)
    (sink-write-rod (name-for-dtd sink) sink)
    (cond
      ((not (zerop (length public-id)))
        (sink-write-rod #" PUBLIC \"" sink)
        (sink-write-escapable-rod public-id sink)
        (sink-write-rod #"\" \"" sink)
        (sink-write-escapable-rod system-id sink)
        (sink-write-rod #"\"" sink))
      ((not (zerop (length system-id)))
        (sink-write-rod #" SYSTEM \"" sink)
        (sink-write-escapable-rod system-id sink)
        (sink-write-rod #"\"" sink)))))

(defmethod sax:start-internal-subset ((sink sink))
  (when (have-internal-subset sink)
    (error "duplicate internal subset"))
  (setf (have-internal-subset sink) t)
  (ensure-doctype sink)
  (sink-write-rod #" [" sink)
  (sink-write-rune #/U+000A sink))

(defmethod sax:end-internal-subset ((sink sink))
  (ensure-doctype sink)
  (sink-write-rod #"]" sink))

(defmethod sax:unparsed-internal-subset ((sink sink) str)
  (when (have-internal-subset sink)
    (error "duplicate internal subset"))
  (setf (have-internal-subset sink) t)
  (ensure-doctype sink)
  (sink-write-rod #" [" sink)
  (sink-write-rune #/U+000A sink)
  (sink-write-rod str sink)
  (sink-write-rod #"]" sink))

;; for the benefit of the XML test suite, prefer ' over "
(defun write-quoted-rod (x sink)
  (let ((q (if (find #/' x) #/" #/'
               ;; '" (thanks you Emacs indentation, the if ends here)
		     )))
    (sink-write-rune q sink)
    (sink-write-rod x sink)
    (sink-write-rune q sink)))

(defmethod sax:notation-declaration ((sink sink) name public-id system-id)
  (let ((prev (previous-notation sink)))
    (when (and (and (canonical sink) (>= (canonical sink) 2))
	       prev
	       (not (rod< prev name)))
      (error "misordered notations; cannot unparse canonically"))
    (setf (previous-notation sink) name)) 
  (sink-write-rod #"<!NOTATION " sink)
  (sink-write-rod name sink)
  (cond
    ((zerop (length public-id))
      (sink-write-rod #" SYSTEM " sink)
      (write-quoted-rod system-id sink))
    ((zerop (length system-id))
      (sink-write-rod #" PUBLIC " sink)
      (write-quoted-rod public-id sink))
    (t 
      (sink-write-rod #" PUBLIC " sink)
      (write-quoted-rod public-id sink)
      (sink-write-rod #" " sink)
      (write-quoted-rod system-id sink)))
  (sink-write-rune #/> sink)
  (sink-write-rune #/U+000A sink))

(defmethod sax:unparsed-entity-declaration
    ((sink sink) name public-id system-id notation-name)
  (unless (and (canonical sink) (< (canonical sink) 3))
    (sink-write-rod #"<!ENTITY " sink)
    (sink-write-rod name sink)
    (cond
      ((zerop (length public-id))
	(sink-write-rod #" SYSTEM " sink)
	(write-quoted-rod system-id sink))
      ((zerop (length system-id))
	(sink-write-rod #" PUBLIC " sink)
	(write-quoted-rod public-id sink))
      (t 
	(sink-write-rod #" PUBLIC " sink)
	(write-quoted-rod public-id sink)
	(sink-write-rod #" " sink)
	(write-quoted-rod system-id sink)))
    (sink-write-rod #" NDATA " sink)
    (sink-write-rod notation-name sink)
    (sink-write-rune #/> sink)
    (sink-write-rune #/U+000A sink)))

(defmethod sax:external-entity-declaration
    ((sink sink) kind name public-id system-id)
  (when (canonical sink)
    (error "cannot serialize parsed entities in canonical mode"))
  (sink-write-rod #"<!ENTITY " sink)
  (when (eq kind :parameter)
    (sink-write-rod #" % " sink))
  (sink-write-rod name sink)
  (cond
    ((zerop (length public-id))
      (sink-write-rod #" SYSTEM " sink)
      (write-quoted-rod system-id sink))
    ((zerop (length system-id))
      (sink-write-rod #" PUBLIC " sink)
      (write-quoted-rod public-id sink))
    (t 
      (sink-write-rod #" PUBLIC " sink)
      (write-quoted-rod public-id sink)
      (sink-write-rod #" " sink)
      (write-quoted-rod system-id sink)))
  (sink-write-rune #/> sink)
  (sink-write-rune #/U+000A sink))

(defmethod sax:internal-entity-declaration ((sink sink) kind name value)
  (when (canonical sink)
    (error "cannot serialize parsed entities in canonical mode"))
  (sink-write-rod #"<!ENTITY " sink)
  (when (eq kind :parameter)
    (sink-write-rod #" % " sink))
  (sink-write-rod name sink)
  (sink-write-rune #/U+0020 sink)
  (sink-write-rune #/\" sink)
  (sink-write-escapable-rod/dtd value sink)
  (sink-write-rune #/\" sink)
  (sink-write-rune #/> sink)
  (sink-write-rune #/U+000A sink))

(defmethod sax:element-declaration ((sink sink) name model)
  (when (canonical sink)
    (error "cannot serialize element type declarations in canonical mode"))
  (sink-write-rod #"<!ELEMENT " sink)
  (sink-write-rod name sink)
  (sink-write-rune #/U+0020 sink)
  (labels ((walk (m)
	     (cond
	       ((eq m :EMPTY)
		 (sink-write-rod "EMPTY" sink))
	       ((eq m :PCDATA)
		 (sink-write-rod "#PCDATA" sink))
	       ((eq m :ANY)
		 (sink-write-rod "ANY" sink))
	       ((atom m)
		 (sink-write-escapable-rod m sink))
	       (t
		 (ecase (car m)
		   (and
		     (sink-write-rune #/\( sink)
		     (loop for (n . rest) on (cdr m) do
			   (walk n)
			   (when rest
			     (sink-write-rune #\, sink)))
		     (sink-write-rune #/\) sink))
		   (or
		     (sink-write-rune #/\( sink)
		     (loop for (n . rest) on (cdr m) do
			   (walk n)
			   (when rest
			     (sink-write-rune #\| sink)))
		     (sink-write-rune #/\) sink))
		   (*
		     (walk (second m))
		     (sink-write-rune #/* sink))
		   (+
		     (walk (second m))
		     (sink-write-rune #/+ sink))
		   (?
		     (walk (second m))
		     (sink-write-rune #/? sink)))))))
    (walk model))
  (sink-write-rune #/> sink)
  (sink-write-rune #/U+000A sink))

(defmethod sax:attribute-declaration ((sink sink) ename aname type default)
  (when (canonical sink)
    (error "cannot serialize attribute type declarations in canonical mode"))
  (sink-write-rod #"<!ATTLIST " sink)
  (sink-write-rod ename sink)
  (sink-write-rune #/U+0020 sink)
  (sink-write-rod aname sink)
  (sink-write-rune #/U+0020 sink)
  (cond
    ((atom type)
      (sink-write-rod (rod (string-upcase (symbol-name type))) sink))
    (t
      (when (eq :NOTATION (car type))
	(sink-write-rod #"NOTATION " sink))
      (sink-write-rune #/\( sink)
      (loop for (n . rest) on (cdr type) do
	    (sink-write-rod n sink)
	    (when rest
	      (sink-write-rune #\| sink)))
      (sink-write-rune #/\) sink)))
  (sink-write-rune #/U+0020 sink)
  (cond
    ((atom default)
      (sink-write-rune #/# sink)
      (sink-write-rod (rod (string-upcase (symbol-name default))) sink))
    (t
      (when (eq :FIXED (car default))
	(sink-write-rod #"#FIXED " sink))
      (sink-write-rune #/\" sink)
      (sink-write-escapable-rod (second default) sink)
      (sink-write-rune #/\" sink)))
  (sink-write-rune #/> sink)
  (sink-write-rune #/U+000A sink))

(defmethod sax:end-dtd ((sink sink))
  (when (have-doctype sink)
    (sink-write-rod #">" sink)
    (sink-write-rune #/U+000A sink)))


;;;; elements

(defstruct (tag (:constructor make-tag (name)))
  name
  (n-children 0)
  (have-gt nil))

(defun sink-fresh-line (sink)
  (unless (zerop (ystream-column (sink-ystream sink)))
    (sink-write-rune #/U+000A sink)		;newline
    (indent sink)))

(defun maybe-close-tag (sink)
  (let ((tag (car (stack sink))))
    (when (and (tag-p tag) (not (tag-have-gt tag)))
      (setf (tag-have-gt tag) t)
      (sink-write-rune #/> sink))))

(defmethod sax:start-element
    ((sink sink) namespace-uri local-name qname attributes)
  (declare (ignore namespace-uri local-name))
  (maybe-close-tag sink)
  (when (stack sink)
    (incf (tag-n-children (first (stack sink)))))
  (push (make-tag qname) (stack sink))
  (when (indentation sink)
    (sink-fresh-line sink)
    (start-indentation-block sink))
  (sink-write-rune #/< sink)
  (sink-write-rod qname sink)
  (dolist (a (if (canonical sink)
		 (sort (copy-list attributes)
		       #'rod<
		       :key #'sax:attribute-qname)
		 attributes))
    (sink-write-rune #/space sink)
    (sink-write-rod (sax:attribute-qname a) sink)
    (sink-write-rune #/= sink)
    (sink-write-rune #/\" sink)
    (if (canonical sink)
	(sink-write-escapable-rod/canonical (sax:attribute-value a) sink)
	(sink-write-escapable-rod/attribute (sax:attribute-value a) sink))
    (sink-write-rune #/\" sink))
  (when (canonical sink)
    (maybe-close-tag sink)))

(defmethod sax:end-element
    ((sink sink) namespace-uri local-name qname)
  (declare (ignore namespace-uri local-name))
  (let ((tag (pop (stack sink))))
    (unless (tag-p tag)
      (error "output does not nest: not in an element"))
    (unless (rod= (tag-name tag) qname)
      (error "output does not nest: expected ~A but got ~A"
             (rod qname) (rod (tag-name tag))))
    (when (indentation sink)
      (end-indentation-block sink)
      (unless (zerop (tag-n-children tag))
        (sink-fresh-line sink)))
    (cond
      ((tag-have-gt tag)
       (sink-write-rod '#.(string-rod "</") sink)
       (sink-write-rod qname sink)
       (sink-write-rod '#.(string-rod ">") sink))
      (t
       (sink-write-rod #"/>" sink)))))

(defmethod sax:processing-instruction ((sink sink) target data)
  (maybe-close-tag sink)
  (unless (rod-equal target '#.(string-rod "xml"))
    (sink-write-rod '#.(string-rod "<?") sink)
    (sink-write-rod target sink)
    (cond
      ((plusp (length data))
       (sink-write-rune #/space sink)
       (sink-write-rod data sink))
      ((canonical sink)
       (sink-write-rune #/space sink)))
    (sink-write-rod '#.(string-rod "?>") sink)))

(defmethod sax:start-cdata ((sink sink))
  (maybe-close-tag sink)
  (push :cdata (stack sink)))

(defmethod sax:characters ((sink sink) data)
  (maybe-close-tag sink)
  (cond
    ((and (eq (car (stack sink)) :cdata)
          (not (canonical sink))
          (not (search #"]]" data)))
      (when (indentation sink)
        (sink-fresh-line sink))
      (sink-write-rod #"<![CDATA[" sink)
      ;; XXX signal error if body is unprintable?
      ;; zzz no, in that case, split into multiple CDATA sections
      (map nil (lambda (c) (sink-write-rune c sink)) data)
      (sink-write-rod #"]]>" sink))
    (t
      (if (indentation sink)
          (unparse-indented-text data sink)
	  (if (canonical sink)
	      (sink-write-escapable-rod/canonical data sink)
	      (sink-write-escapable-rod data sink))))))

(defmethod sax:unescaped ((sink sink) data)
  (maybe-close-tag sink)
  (sink-write-rod data sink))

(defmethod sax:comment ((sink sink) data)
  (maybe-close-tag sink)
  (unless (canonical sink)
    ;; XXX signal error if body is unprintable?
    (sink-write-rod #"<!--" sink)
    (map nil (lambda (c) (sink-write-rune c sink)) data)
    (sink-write-rod #"-->" sink)))

(defmethod sax:end-cdata ((sink sink))
  (unless (eq (pop (stack sink)) :cdata)
    (error "output does not nest: not in a cdata section")))

(defun indent (sink)
  (dotimes (x (current-indentation sink))
    (sink-write-rune #/U+0020 sink)))

(defun start-indentation-block (sink)
  (incf (current-indentation sink) (indentation sink)))

(defun end-indentation-block (sink)
  (decf (current-indentation sink) (indentation sink)))

(defun unparse-indented-text (data sink)
  (flet ((whitespacep (x)
           (or (rune= x #/U+000A) (rune= x #/U+0020))))
    (let* ((n (length data))
           (pos (position-if-not #'whitespacep data))
           (need-whitespace-p nil))
      (cond
        ((zerop n))
        (pos
          (sink-fresh-line sink)
          (while (< pos n)
            (let* ((w (or (position-if #'whitespacep data :start (1+ pos)) n))
                   (next (or (position-if-not #'whitespacep data :start w) n)))
              (when need-whitespace-p
                (if (< (+ (ystream-column (sink-ystream sink)) w (- pos))
		       (width sink))
                    (sink-write-rune #/U+0020 sink)
                    (sink-fresh-line sink)))
	      (sink-write-escapable-rod data sink :start pos :end w)
              (setf need-whitespace-p (< w n))
              (setf pos next))))
        (t
          (sink-write-rune #/U+0020 sink))))))

(defun sink-write-escapable-rod (rod sink &key (start 0) (end (length rod)))
  ;;
  ;; OPTIMIZE ME
  ;;
  (let ((y (sink-ystream sink)))
    (loop
       for i from start below end
       for c = (rune rod i)
       do
	 (case c
	   (#/& (ystream-write-escapable-rod #.(string-rod "&amp;") y))
	   (#/< (ystream-write-escapable-rod #.(string-rod "&lt;") y))
	   ;; there's no need to escape > per se, but we're supposed to
	   ;; escape -->, which is harder to check for
	   (#/> (ystream-write-escapable-rod #.(string-rod "&gt;") y))
	   (#/U+000D (ystream-write-escapable-rod #.(string-rod "&#13;") y))
	   (t (ystream-write-escapable-rune c y))))))

(defun sink-write-escapable-rod/attribute
    (rod sink &key (start 0) (end (length rod)))
  ;;
  ;; OPTIMIZE ME
  ;;
  (let ((y (sink-ystream sink)))
    (loop
       for i from start below end
       for c = (rune rod i)
       do
	 (case c
	   (#/& (ystream-write-escapable-rod #.(string-rod "&amp;") y))
	   (#/< (ystream-write-escapable-rod #.(string-rod "&lt;") y))
	   ;; there's no need to escape > per se, but we're supposed to
	   ;; escape -->, which is harder to check for
	   (#/> (ystream-write-escapable-rod #.(string-rod "&gt;") y))
	   (#/\" (ystream-write-escapable-rod #.(string-rod "&quot;") y))
	   (#/U+0009 (ystream-write-escapable-rod #.(string-rod "&#9;") y))
	   (#/U+000A (ystream-write-escapable-rod #.(string-rod "&#10;") y))
	   (#/U+000D (ystream-write-escapable-rod #.(string-rod "&#13;") y))
	   (t (ystream-write-escapable-rune c y))))))

(defun sink-write-escapable-rod/canonical
    (rod sink &key (start 0) (end (length rod)))
  ;;
  ;; OPTIMIZE ME
  ;;
  (let ((y (sink-ystream sink)))
    (loop
       for i from start below end
       for c = (rune rod i)
       do
	 (case c
	   (#/& (ystream-write-escapable-rod #.(string-rod "&amp;") y))
	   (#/< (ystream-write-escapable-rod #.(string-rod "&lt;") y))
	   (#/> (ystream-write-escapable-rod #.(string-rod "&gt;") y))
	   (#/\" (ystream-write-escapable-rod #.(string-rod "&quot;") y))
	   (#/U+0009 (ystream-write-escapable-rod #.(string-rod "&#9;") y))
	   (#/U+000A (ystream-write-escapable-rod #.(string-rod "&#10;") y))
	   (#/U+000D (ystream-write-escapable-rod #.(string-rod "&#13;") y))
	   (t (ystream-write-escapable-rune c y))))))

(defun sink-write-escapable-rod/dtd
    (rod sink &key (start 0) (end (length rod)))
  (let ((y (sink-ystream sink)))
    (loop
       for i from start below end
       for c = (rune rod i)
       do
	 (case c
	   (#/% (ystream-write-escapable-rod #.(string-rod "&#37;") y))
	   (#/& (ystream-write-escapable-rod #.(string-rod "&amp;") y))
	   (#/< (ystream-write-escapable-rod #.(string-rod "&lt;") y))
	   (#/> (ystream-write-escapable-rod #.(string-rod "&gt;") y))
	   (#/\" (ystream-write-escapable-rod #.(string-rod "&quot;") y))
	   (#/U+0009 (ystream-write-escapable-rod #.(string-rod "&#9;") y))
	   (#/U+000A (ystream-write-escapable-rod #.(string-rod "&#10;") y))
	   (#/U+000D (ystream-write-escapable-rod #.(string-rod "&#13;") y))
	   (t (ystream-write-escapable-rune c y))))))

(defun sink-write-rune (c sink)
  (ystream-write-rune c (sink-ystream sink)))

(defun sink-write-rod (r sink)
  (ystream-write-rod r (sink-ystream sink)))


;;;; convenience functions for DOMless XML serialization

(defvar *current-element*)
(defvar *sink*)
(defvar *unparse-namespace-bindings*)
(defvar *current-namespace-bindings*)

(defmacro with-xml-output (sink &body body)
  `(invoke-with-xml-output (lambda () ,@body) ,sink))

(defmacro with-output-sink ((var) &body body)
  `(invoke-with-output-sink (lambda (,var) ,@body)))

(defun invoke-with-xml-output (fn sink)
  (let ((*sink* sink)
        (*current-element* nil)
	(*unparse-namespace-bindings* *initial-namespace-bindings*)
	(*current-namespace-bindings* nil))
    (sax:start-document *sink*)
    (funcall fn)
    (sax:end-document *sink*)))

(defun invoke-with-output-sink (fn)
  (maybe-emit-start-tag)
  (funcall fn *sink*))

(defmacro with-element (qname &body body)
  `(invoke-with-element (lambda () ,@body) ,qname))

(defmacro with-element* ((prefix lname) &body body)
  `(invoke-with-element* (lambda () ,@body) ,prefix ,lname))

(defmacro with-namespace ((prefix uri) &body body)
  `(invoke-with-namespace (lambda () ,@body) ,prefix ,uri))

(defun doctype (name public-id system-id &optional internal-subset)
  (sax:start-dtd *sink* name public-id system-id)
  (when internal-subset
    (sax:unparsed-internal-subset *sink* internal-subset))
  (sax:end-dtd *sink*))

(defun maybe-emit-start-tag ()
  (when *current-element*
    ;; starting child node, need to emit opening tag of parent first:
    (destructuring-bind ((uri lname qname) &rest attributes) *current-element*
      (sax:start-element *sink* uri lname qname (reverse attributes)))
    (setf *current-element* nil)))

(defun invoke-with-namespace (fn prefix uri)
  (let ((*unparse-namespace-bindings*
	 (acons prefix uri *unparse-namespace-bindings*))
	(*current-namespace-bindings*
	 (acons prefix uri *current-namespace-bindings*)))
    (sax:start-prefix-mapping *sink* prefix uri)
    (multiple-value-prog1
	(funcall fn)
      (sax:end-prefix-mapping *sink* prefix))))

(defun invoke-with-element (fn qname)
  (setf qname (rod qname))
  (multiple-value-bind (prefix lname)
      (split-qname qname)
    (invoke-with-element* fn prefix lname qname)))

(defun find-unparse-namespace (prefix)
  (cdr (assoc prefix *unparse-namespace-bindings* :test 'equal)))

(defun invoke-with-element* (fn prefix lname &optional qname)
  (setf prefix (when prefix (rod prefix)))
  (setf lname (rod lname))
  (maybe-emit-start-tag)
  (let* ((qname (or qname
		    (if prefix (concatenate 'rod prefix #":" lname) lname)))
	 (uri (find-unparse-namespace (or prefix #"")))
	 (*current-element*
	  (cons (list uri lname qname)
		(mapcar (lambda (x)
			  (destructuring-bind (prefix &rest uri) x
			    (sax:make-attribute
			     :namespace-uri #"http://www.w3.org/2000/xmlns/"
			     :local-name prefix
			     :qname (if (zerop (length prefix))
					#"xmlns"
					(concatenate 'rod #"xmlns:" prefix))
			     :value uri)))
			*current-namespace-bindings*))))
    (multiple-value-prog1
        (let ((*current-namespace-bindings* nil))
	  (funcall fn))
      (maybe-emit-start-tag)
      (sax:end-element *sink* uri lname qname))))

(defgeneric unparse-attribute (value))
(defmethod unparse-attribute ((value string)) value)
(defmethod unparse-attribute ((value null)) nil)
(defmethod unparse-attribute ((value integer)) (write-to-string value))

(defun attribute (qname value)
  (setf qname (rod qname))
  (multiple-value-bind (prefix lname)
      (split-qname qname)
    (attribute* prefix lname value qname)))

(defun attribute* (prefix lname value &optional qname)
  (setf value (unparse-attribute value))
  (when value
    (setf prefix (when prefix (rod prefix)))
    (setf lname (rod lname))
    (push (sax:make-attribute
	   :namespace-uri (find-unparse-namespace prefix)
	   :local-name lname
	   :qname (or qname
		      (if prefix (concatenate 'rod prefix #":" lname) lname))
	   :value (rod value))
	  (cdr *current-element*))))

(defun cdata (data)
  (maybe-emit-start-tag)
  (sax:start-cdata *sink*)
  (sax:characters *sink* (rod data))
  (sax:end-cdata *sink*)
  data)

(defun text (data)
  (maybe-emit-start-tag)
  (sax:characters *sink* (rod data))
  data)

(defun comment (data)
  (maybe-emit-start-tag)
  (sax:comment *sink* (rod data))
  data)

(defun processing-instruction (target data)
  (maybe-emit-start-tag)
  (sax:processing-instruction *sink* (rod target) (rod data))
  data)

(defun unescaped (str)
  (maybe-emit-start-tag)
  (sax:unescaped *sink* (rod str)))
