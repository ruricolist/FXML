;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SAX; readtable: runes; Encoding: utf-8; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: A SAX2-like API for the xml parser
;;;   Created: 2003-06-30
;;;    Author: Henrik Motakef <hmot@henrik-motakef.de>
;;;    Author: David Lichteblau (DTD-related changes)
;;;   License: BSD
;;; ---------------------------------------------------------------------------
;;;  © copyright 2003 by Henrik Motakef
;;;  © copyright 2004 knowledgeTools Int. GmbH

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
;;   * The whole ErrorHandler class, this is better handled using
;;     conditions (but isn't yet)
;;   * The LexicalHandler (start-cdata etc) would be nice  [-- partly done]

(defpackage :sax
  (:use :common-lisp)
  (:export #:*namespace-processing*
	   #:*include-xmlns-attributes*
	   #:*use-xmlns-namespace*

           #:make-attribute
           #:find-attribute
           #:find-attribute-ns
           #:attribute-namespace-uri
           #:attribute-local-name
           #:attribute-qname
           #:attribute-value
           #:attribute-specified-p
	   
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
	   #:end-cdata
	   #:start-dtd
	   #:end-dtd
	   #:start-internal-subset
	   #:end-internal-subset
           #:unparsed-entity-declaration
           #:external-entity-declaration
           #:internal-entity-declaration
           #:notation-declaration
           #:element-declaration
           #:attribute-declaration
           #:entity-resolver))

(in-package :sax)

;; The http://xml.org/sax/features/namespaces property
(defvar *namespace-processing* t
  "If non-nil (the default), namespace processing is enabled.

See also `start-element' and `end-element' for a detailed description
of the consequences of modifying this variable, and
`*include-xmlns-attributes*' and `*use-xmlns-namespace*' for further
related options.")

;; The http://xml.org/sax/features/namespace-prefixes property.
(defvar *include-xmlns-attributes* t
  "If non-nil, namespace declarations are reported as normal
attributes.

This variable has no effect unless `*namespace-processing*' is
non-nil.

See also `*use-xmlns-namespace*', and `start-element' for a detailed
description of the consequences of setting this variable.")

(defvar *use-xmlns-namespace* t
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

(defstruct attribute
  namespace-uri
  local-name
  qname
  value
  specified-p)

(defun %rod= (x y)
  ;; allow rods *and* strings *and* null
  (cond
    ((zerop (length x)) (zerop (length y)))
    ((zerop (length y)) nil)
    ((stringp x) (string= x y))
    (t (runes:rod= x y))))

(defun find-attribute (qname attrs)
  (find qname attrs :key #'attribute-qname :test #'%rod=))

(defun find-attribute-ns (uri lname attrs)
  (find-if (lambda (attr)
	     (and (%rod= uri (sax:attribute-namespace-uri attr))
		  (%rod= lname (sax:attribute-local-name attr))))
	   attrs))

(defgeneric start-document (handler)
  (:documentation "Called at the beginning of the parsing process,
before any element, processing instruction or comment is reported.

Handlers that need to maintain internal state may use this to perform
any neccessary initializations.")
  (:method ((handler t)) nil))

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
  (:method ((handler t) namespace-uri local-name qname attributes)
    (declare (ignore namespace-uri local-name qname attributes))
    nil))

(defgeneric start-prefix-mapping (handler prefix uri)
  (:documentation "Called when the scope of a new prefix -> namespace-uri mapping begins.

This will always be called immediatly before the `start-element' event
for the element on which the namespaces are declared.

Clients don't usually have to implement this except under special
circumstances, for example when they have to deal with qualified names
in textual content. The parser will handle namespaces of elements and
attributes on its own.")
  (:method ((handler t) prefix uri) (declare (ignore prefix uri)) nil))

(defgeneric characters (handler data)
  (:documentation "Called for textual element content.

The data is passed as a rod, with all entity references resolved.
It is possible that the character content of an element is reported
via multiple subsequent calls to this generic function.")
  (:method ((handler t) data) (declare (ignore data)) nil))

(defgeneric processing-instruction (handler target data)
  (:documentation "Called when a processing instruction is read.

Both target and data are rods.")
  (:method ((handler t) target data) (declare (ignore target data)) nil))

(defgeneric end-prefix-mapping (handler prefix)
  (:documentation "Called when a prefix -> namespace-uri mapping goes out of scope.

This will always be called immediatly after the `end-element' event
for the element on which the namespace is declared. The order of the
end-prefix-mapping events is otherwise not guaranteed.

Clients don't usually have to implement this except under special
circumstances, for example when they have to deal with qualified names
in textual content. The parser will handle namespaces of elements and
attributes on its own.")
  (:method ((handler t) prefix) prefix nil))

(defgeneric end-element (handler namespace-uri local-name qname)
  (:documentation "Called to report the end of an element.

See the documentation for `start-element' for a description of the
parameters.")
  (:method ((handler t) namespace-uri local-name qname)
    (declare (ignore namespace-uri local-name qname))
    nil))

(defgeneric end-document (handler)
  (:documentation "Called at the end of parsing a document.
This is always the last function called in the parsing process.

In contrast to all of the other methods, the return value of this gf
is significant, it will be returned by the parse-file/stream/string function.")
  (:method ((handler t)) nil))

;; LexicalHandler

(defgeneric comment (handler data)
  (:method ((handler t) data) data nil))

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

(defgeneric start-dtd (handler name public-id system-id)
  (:documentation "Called at the beginning of parsing a DTD.")
  (:method ((handler t) name public-id system-id)
    (declare (ignore name public-id system-id))
    nil))

(defgeneric end-dtd (handler)
  (:documentation "Called at the end of parsing a DTD.")
  (:method ((handler t)) nil))

(defgeneric start-internal-subset (handler)
  (:documentation "Reports that an internal subset is present.  Called before
any definition from the internal subset is reported.")
  (:method ((handler t)) nil))

(defgeneric end-internal-subset (handler)
  (:documentation "Called after processing of the internal subset has
finished, if present.")
  (:method ((handler t)) nil))

(defgeneric unparsed-entity-declaration
    (handler name public-id system-id notation-name)
  (:documentation
   "Called when an unparsed entity declaration is seen in a DTD.")
  (:method ((handler t) name public-id system-id notation-name)
    (declare (ignore name public-id system-id notation-name))
    nil))

(defgeneric external-entity-declaration
    (handler kind name public-id system-id)
  (:documentation
   "Called when a parsed external entity declaration is seen in a DTD.")
  (:method ((handler t) kind name public-id system-id)
    (declare (ignore kind name public-id system-id))
    nil))

(defgeneric internal-entity-declaration
    (handler kind name value)
  (:documentation
   "Called when an internal entity declaration is seen in a DTD.")
  (:method ((handler t) kind name value)
    (declare (ignore kind name value))
    nil))

(defgeneric notation-declaration
    (handler name public-id system-id)
  (:documentation
   "Called when a notation declaration is seen while parsing a DTD.")
  (:method ((handler t) name public-id system-id)
    (declare (ignore name public-id system-id))
    nil))

(defgeneric element-declaration (handler name model)
  (:documentation
   "Called when a element declaration is seen in a DTD.  Model is not a string,
    but a nested list, with *, ?, +, OR, and AND being the operators, rods
    as names, :EMPTY and :PCDATA as special tokens.  (AND represents
    sequences.)")
  (:method ((handler t) name model)
    (declare (ignore name model))
    nil))

(defgeneric attribute-declaration
    (handler element-name attribute-name type default)
  (:documentation
   "Called when an attribute declaration is seen in a DTD.
    type        one of :CDATA, :ID, :IDREF, :IDREFS,
                :ENTITY, :ENTITIES, :NMTOKEN, :NMTOKENS,
                (:NOTATION <name>*), or (:ENUMERATION <name>*)
    default     :REQUIRED, :IMPLIED, (:FIXED content), or (:DEFAULT content)")
  (:method ((handler t) element-name attribute-name type value)
    (declare (ignore element-name attribute-name type value))
    nil))

(defgeneric entity-resolver
    (handler resolver)
  (:documentation
   "Called between sax:end-dtd and sax:end-document to register an entity
    resolver, a function of two arguments: An entity name and SAX handler.
    When called, the resolver function will parse the named entity's data.")
  (:method ((handler t) resolver)
    (declare (ignore resolver))
    nil))

;; internal for now
(defgeneric dtd (handler dtd)
  (:method ((handler t) dtd) (declare (ignore dtd)) nil))
