;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SAX; readtable: glisp; Encoding: utf-8; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: A SAX2-like API for the xml parser
;;;   Created: 2003-06-30
;;;    Author: Henrik Motakef <hmot@henrik-motakef.de>
;;;   License: BSD
;;; ---------------------------------------------------------------------------
;;;  © copyright 2003 by Henrik Motakef

;;; Redistribution and use  in source and binary   forms, with or  without
;;; modification, are permitted provided that the following conditions are
;;; met:                                                                  
;;; 								      
;;; 1. Redistributions  of  source  code  must retain  the above copyright
;;;    notice, this list of conditions and the following disclaimer.      
;;; 								      
;;; 2. Redistributions in  binary form must reproduce  the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution
;;; 								      
;;; THIS  SOFTWARE   IS PROVIDED ``AS  IS''   AND ANY  EXPRESS  OR IMPLIED
;;; WARRANTIES, INCLUDING, BUT NOT LIMITED  TO, THE IMPLIED WARRANTIES  OF
;;; MERCHANTABILITY  AND FITNESS FOR A  PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;;; INDIRECT,  INCIDENTAL,  SPECIAL, EXEMPLARY,  OR CONSEQUENTIAL  DAMAGES
;;; (INCLUDING, BUT NOT LIMITED TO,   PROCUREMENT OF SUBSTITUTE GOODS   OR
;;; SERVICES;  LOSS OF  USE,  DATA, OR  PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER  CAUSED AND ON ANY THEORY  OF LIABILITY,  WHETHER IN CONTRACT,
;;; STRICT LIABILITY, OR  TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
;;; IN ANY WAY  OUT OF THE  USE OF THIS SOFTWARE,  EVEN IF ADVISED OF  THE
;;; POSSIBILITY OF SUCH DAMAGE.

;;; TODO/ Open Questions:

;; o Should there be a predefined "handler" class, or even several
;;   (like Java SAX' ContentHandler, DTDHandler, LexicalHandler etc? I
;;   don't really see why.
;; o Missing stuff from Java SAX2:
;;   * ignorable-whitespace
;;   * document-locator/(setf document-locator)
;;     (probably implies a handler class with an appropriate slot)
;;   * skipped-entity
;;   * notation-declaration
;;   * unparsed-entity-declaration
;;   * The whole ErrorHandler class, this is better handled using
;;     conditions (but isn't yet)
;;   * The LexicalHandler (start-cdata etc) would be nice
;;   * The DeclHandler interface (element-decl, attribute-decl...)
;;     is useful, but the Java interface sucks.
;; o Despite all the namespace-uri etc arguments, namespaces are not
;;   really supported yet, the xml parser always passes nil. This will
;;   hopefully change Real Soon Now, and I didn't want to have to
;;   rewrite the interface then

(defpackage :sax
  (:use :common-lisp)
  (:export #:*namespace-processing*
	   #:*include-xmlns-attributes*
	   #:*use-xmlns-namespace*
	   
	   #:start-document
	   #:start-prefix-mapping
	   #:start-element
	   #:characters
	   #:processing-instruction
	   #:end-element
	   #:end-prefix-mapping
	   #:end-document
	   #:comment
	   #:start-cdata
	   #:end-cdata))

(in-package :sax)

;; The http://xml.org/sax/features/namespaces property
(defvar *namespace-processing* t
  "If non-nil (the default), namespace processing is enabled.

See also `start-element' and `end-element' for a detailed description
of the consequences of modifying this variable, and
`*include-xmlns-attributes*' and `*use-xmlns-namespace*' for further
related options.")

;; The http://xml.org/sax/features/namespace-prefixes property.
(defvar *include-xmlns-attributes* nil
  "If non-nil, namespace declarations are reported as normal
attributes.

This variable has no effect unless `*namespace-processing*' is
non-nil.

See also `*use-xmlns-namespace*', and `start-element' for a detailed
description of the consequences of setting this variable.")

(defvar *use-xmlns-namespace* nil
  "If this variable is nil (the default), attributes with a name like
'xmlns:x' are not considered to be in a namespace, following the
'Namespaces in XML' specification.

If it is non-nil, such attributes are considered to be in a namespace
with the URI 'http://www.w3.org/2000/xmlns/', following an
incompatible change silently introduced in the errata to that spec,
and adopted by some W3C standards.

For example, an attribute like xmlns:ex='http://example.com' would be
reported like this:

*use-xmlns-namespace*: nil
namespace-uri:         nil
local-name:            nil
qname:                 #\"xmlns:ex\"

*use-xmlns-namespace*: t
namespace-uri:         #\"http://www.w3.org/2000/xmlns/\"
local-name:            #\"ex\"
qname:                 #\"xmlns:ex\"

Setting this variable has no effect unless both
`*namespace-processing*' and `*include-xmlns-attributes*' are non-nil.")

(defgeneric start-document (handler)
  (:documentation "Called at the beginning of the parsing process,
before any element, processing instruction or comment is reported.

Handlers that need to maintain internal state may use this to perform
any neccessary initializations.")
  (:method ((handler t)) nil))

;; How should attributes be represented?
;; Currently its just a (name . value) alist, but this isn't too
;; useful wrt namespaced attributes. Probably a struct.
(defgeneric start-element (handler namespace-uri local-name qname attributes)
  (:documentation "Called to report the beginning of an element.

There will always be a corresponding call to end-element, even in the
case of an empty element (i.e. <foo/>).

If the value of *namespaces* is non-nil, namespace-uri, local-name and
qname are rods. If it is nil, namespace-uri and local-name are always
nil, and it is not an error if the qname is not a well-formed
qualified element name (for example, if it contains more than one
colon).

The attributes parameter is a list (in arbitrary order) of instances
of the `attribute' structure class. The for their namespace-uri and
local-name properties, the same rules as for the element name
apply. Additionally, namespace-declaring attributes (those whose name
is \"xmlns\" or starts with \"xmlns:\") are only included if
*namespace-prefixes* is non-nil.")
  (:method ((handler t) namespace-uri local-name qname attributes) nil))

(defgeneric start-prefix-mapping (handler prefix uri)
  (:documentation "Called when the scope of a new prefix -> namespace-uri mapping begins.

This will always be called immediatly before the `start-element' event
for the element on which the namespaces are declared.

Clients don't usually have to implement this except under special
circumstances, for example when they have to deal with qualified names
in textual content. The parser will handle namespaces of elements and
attributes on its own.")
  (:method ((handler t) prefix uri) nil))

(defgeneric characters (handler data)
  (:documentation "Called for textual element content.

The data is passed as a rod, with all entity references resolved.
It is possible that the character content of an element is reported
via multiple subsequent calls to this generic function.")
  (:method ((handler t) data) nil))

(defgeneric processing-instruction (handler target data)
  (:documentation "Called when a processing instruction is read.

Both target and data are rods.")
  (:method ((handler t) target data) nil))

(defgeneric end-prefix-mapping (handler prefix)
  (:documentation "Called when a prefix -> namespace-uri mapping goes out of scope.

This will always be called immediatly after the `end-element' event
for the element on which the namespace is declared. The order of the
end-prefix-mapping events is otherwise not guaranteed.

Clients don't usually have to implement this except under special
circumstances, for example when they have to deal with qualified names
in textual content. The parser will handle namespaces of elements and
attributes on its own.")
  (:method ((handler t) prefix) nil))

(defgeneric end-element (handler namespace-uri local-name qname)
  (:documentation "Called to report the end of an element.

See the documentation for `start-element' for a description of the
parameters.")
  (:method ((handler t) namespace-uri local-name qname) nil))

(defgeneric end-document (handler)
  (:documentation "Called at the end of parsing a document.
This is always the last function called in the parsing process.

In contrast to all of the other methods, the return value of this gf
is significant, it will be returned by the parse-file/stream/string function.")
  (:method ((handler t)) nil))

;; LexicalHandler

(defgeneric comment (handler data)
  (:method ((handler t) data) nil))

(defgeneric start-cdata (handler)
  (:documentation "Called at the beginning of parsing a CDATA section.

Handlers only have to implement this if they are interested in the
lexical structure of the parsed document. The content of the CDATA
section is reported via the `characters' generic function like all
other textual content.")
  (:method ((handler t)) nil))

(defgeneric end-cdata (handler)
  (:documentation "Called at the end of parsing a CDATA section.

Handlers only have to implement this if they are interested in the
lexical structure of the parsed document. The content of the CDATA
section is reported via the `characters' generic function like all
other textual content.")
  (:method ((handler t)) nil))