;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XML; readtable: glisp; Encoding: utf-8; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Dump canonic XML according to J.Clark
;;;   Created: 1999-09-09
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  © copyright 1999 by Gilbert Baumann

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

(in-package :xml)

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

(defvar *quux*)                         ;!!!BIG HACK!!!

(defun unparse-document (doc sink)
  (mapc (rcurry #'unparse-node sink) (dom:child-nodes doc)))

(defun unparse-node (node sink)
  (cond ((dom:element-p node)
         (write-rune #/< sink)
         (write-rod (dom:tag-name node) sink)
         ;; atts
         (let ((atts (sort (copy-list (dom:items (dom:attributes node)))
                           #'rod< :key #'dom:name)))
           (dolist (a atts)
             (write-rune #/space sink)
             (write-rod (dom:name a) sink)
             (write-rune #/= sink)
             (write-rune #/\" sink)
             (let ((*quux* nil))
               (map nil (lambda (c) (unparse-datachar c sink)) (dom:value a)))
             (write-rune #/\" sink)))
         (write-rod '#.(string-rod ">") sink)
         (dolist (k (dom:child-nodes node))
           (unparse-node k sink))
         (write-rod '#.(string-rod "</") sink)
         (write-rod (dom:tag-name node) sink)
         (write-rod '#.(string-rod ">") sink))
        ((dom:processing-instruction-p node)
         (unless (rod-equal (dom:target node) '#.(string-rod "xml"))
           (write-rod '#.(string-rod "<?") sink)
           (write-rod (dom:target node) sink)
           (write-rune #/space sink)
           (write-rod (dom:data node) sink)
           (write-rod '#.(string-rod "?>") sink) ))
        ((dom:text-node-p node)
         (let ((*quux* nil))
           (map nil (lambda (c) (unparse-datachar c sink))
                (dom:data node))))
        (t
         (error "Oops in unparse: ~S." node))))

(defun unparse-datachar (c sink)
  (cond ((rune= c #/&) (write-rod '#.(string-rod "&amp;") sink))
        ((rune= c #/<) (write-rod '#.(string-rod "&lt;") sink))
        ((rune= c #/>) (write-rod '#.(string-rod "&gt;") sink))
        ((rune= c #/\") (write-rod '#.(string-rod "&quot;") sink))
        ((rune= c #/U+0009) (write-rod '#.(string-rod "&#9;") sink))
        ((rune= c #/U+000A) (write-rod '#.(string-rod "&#10;") sink))
        ((rune= c #/U+000D) (write-rod '#.(string-rod "&#13;") sink))
        (t
         (write-rune c sink))))

(defun write-rod (rod sink)
  (let ((*quux* nil))
    (map nil (lambda (c) (write-rune c sink)) rod)))

(defun write-rune (rune sink)
  (cond ((<= #xD800 rune #xDBFF)
         (setf *quux* rune))
        ((<= #xDC00 rune #xDFFF)
         (let ((q (logior (ash (- *quux* #xD7C0) 10) (- rune #xDC00))))
           (write-rune-0 q sink))
         (setf *quux* nil))
        (t
         (write-rune-0 rune sink))))

(defun write-rune-0 (rune sink)
  (labels ((wr (x)
             (write-char (code-char x) sink)))
    (cond ((<= #x00000000 rune #x0000007F) 
           (wr rune))
          ((<= #x00000080 rune #x000007FF)
           (wr (logior #b11000000 (ldb (byte 5 6) rune)))
           (wr (logior #b10000000 (ldb (byte 6 0) rune))))
          ((<= #x00000800 rune #x0000FFFF)
           (wr (logior #b11100000 (ldb (byte 4 12) rune)))
           (wr (logior #b10000000 (ldb (byte 6 6) rune)))
           (wr (logior #b10000000 (ldb (byte 6 0) rune))))
          ((<= #x00010000 rune #x001FFFFF)
           (wr (logior #b11110000 (ldb (byte 3 18) rune)))
           (wr (logior #b10000000 (ldb (byte 6 12) rune)))
           (wr (logior #b10000000 (ldb (byte 6 6) rune)))
           (wr (logior #b10000000 (ldb (byte 6 0) rune))))
          ((<= #x00200000 rune #x03FFFFFF)
           (wr (logior #b11111000 (ldb (byte 2 24) rune)))
           (wr (logior #b10000000 (ldb (byte 6 18) rune)))
           (wr (logior #b10000000 (ldb (byte 6 12) rune)))
           (wr (logior #b10000000 (ldb (byte 6 6) rune)))
           (wr (logior #b10000000 (ldb (byte 6 0) rune))))
          ((<= #x04000000 rune #x7FFFFFFF)
           (wr (logior #b11111100 (ldb (byte 1 30) rune)))
           (wr (logior #b10000000 (ldb (byte 6 24) rune)))
           (wr (logior #b10000000 (ldb (byte 6 18) rune)))
           (wr (logior #b10000000 (ldb (byte 6 12) rune)))
           (wr (logior #b10000000 (ldb (byte 6 6) rune)))
           (wr (logior #b10000000 (ldb (byte 6 0) rune)))))))

(defun rod< (rod1 rod2)
  (do ((i 0 (+ i 1)))
      (nil)
    (cond ((= i (length rod1))
           (return t))
          ((= i (length rod2))
           (return nil))
          ((< (aref rod1 i) (aref rod2 i))
           (return t))
          ((> (aref rod1 i) (aref rod2 i))
           (return nil)))))

