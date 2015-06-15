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

(defclass named-node-mixin ()
  ((local-name :reader local-name :accessor %local-name)
   (prefix :initform nil
	   :reader namespace-prefix
	   :accessor %namespace-prefix)
   (namespace-uri :initform nil
		  :reader namespace-uri
		  :accessor %namespace-uri)))

(defclass attribute (leaf-node named-node-mixin)
  ((value :accessor value))
  (:documentation
   "@short{Instances of this class represent attributes of an @class{element},
    excluding namespace declarations.}

    The @fun{parent} of an attribute is always an @class{element} or nil,
    but the attribute is not a child of that element.

    @see-slot{local-name}
    @see-slot{namespace-prefix}
    @see-slot{namespace-uri}
    @see-slot{qualified-name}
    @see-slot{value}
    @see{list-attributes}
    @see-constructor{make-attribute}"))

(defclass comment (leaf-node)
  ((data :initarg :data :accessor data))
  (:documentation
   "Instances of this class represent XML comments.
    @see-slot{data}
    @see-constructor{make-comment}"))

(defclass cxml-stp:document-type (leaf-node)
  ((root-element-name :accessor root-element-name)
   (system-id :initform nil :accessor system-id)
   (public-id :initform nil :accessor public-id)
   (internal-subset :initform nil :accessor internal-subset)
   (dtd :initform nil :accessor dtd))
  (:documentation
   "@short{Instances of this class represent the DOCTYPE declaration at the
    beginning of a document.}

    The document type is an optional child of a @class{document}.  At most
    one document type is allowed, and it must precede the document element.

    Since STP checks well-formedness only, not validity, the document type
    only declares what DTD the document claims to be conforming to, but
    does not guarantee that it actually does.

    @see-constructor{make-document-type}
    @see-slot{root-element-name}
    @see-slot{system-id}
    @see-slot{public-id}
    @see-slot{internal-subset}"))
(setf (find-class 'document-type) (find-class 'cxml-stp:document-type))
#+clozure (deftype document-type () 'cxml-stp:document-type)

(defclass cxml-stp:document (parent-node) ()
  (:documentation
   "@short{Instances of this class represent an entire XML document.}

    A document may have at most one document-type, and must have exactly one
    element as a child (in this order).

    It may also have comments and processing-instructions anywhere.

    @see-constructor{make-document}
    @see-slot{document-element}
    @see-slot{document-type}"))
(setf (find-class 'document) (find-class 'cxml-stp:document))
#+clozure (deftype document () (find-class 'cxml-stp:document))

(defclass element (parent-node named-node-mixin)
  ((attributes :initform nil :accessor %attributes)
   (namespaces :initform nil :accessor %namespaces))
  (:documentation
   "@short{Instances of this class represent XML elements with their attributes
    and namespaces.}

    See @class{node} for functions to query the list of children.

    See @class{parent-node} for functions to add or remove children.

    @see-slot{local-name}
    @see-slot{namespace-prefix}
    @see-slot{namespace-uri}
    @see-slot{qualified-name}
    @see{add-attribute}
    @see{remove-attribute}
    @see{find-attribute-named}
    @see{find-attribute-if}
    @see{with-attributes}
    @see{list-attributes}
    @see{map-attributes}
    @see{attribute-value}
    @see{find-namespace}
    @see{find-attribute-namespace}
    @see{find-local-namespace}
    @see{find-extra-namespace}
    @see{add-extra-namespace}
    @see{remove-extra-namespace}
    @see{map-extra-namespaces}
   @see-constructor{make-element}"))

(defclass leaf-node (node) ())

(defclass node ()
  ((parent :initform nil :reader parent :writer (setf %parent)))
  (:documentation
   "@short{The superclass of all nodes.}

    Although only @class{document} and @class{element} allow children,
    read-only functions accessing the list of children are available for
    all nodes and always return zero children for other classes.

    @see-slot{parent}
    @see-slot{base-uri}
    @see{document}
    @see{root}
    @see{detach}
    @see{copy}
    @see{serialize}
    @see{map-children}
    @see{do-children}
    @see{list-children}
    @see{first-child}
    @see{last-child}
    @see{nth-child}
    @see{previous-sibling}
    @see{next-sibling}
    @see{count-children}
    @see{find-child}
    @see{child-position}
    @see{count-children-if}
    @see{find-child-if}
    @see{child-position-if}
    @see{filter-children}
    @see{map-recursively}
    @see{do-recursively}
    @see{find-recursively}
    @see{filter-recursively}"))

(defclass parent-node (node)
  ((%base-uri :initform nil)
   (%children :initform nil :accessor %children))
  (:documentation
   "@short{Instances of this class can have children.}

    See @class{node} for functions to query the list of children without
    changing it.

    @see{prepend-child}
    @see{append-child}
    @see{delete-nth-child}
    @see{delete-child}
    @see{insert-child-before}
    @see{insert-child-after}
    @see{replace-child}
    @see{insert-child}
    @see{delete-child-if}
    @see{replace-children}"))

(defclass processing-instruction (leaf-node)
  ((target :initarg :target :accessor target)
   (data :initarg :data :accessor data))
  (:documentation
   "Instances of this class represent processing instructions.
    @see-slot{target}
    @see-slot{data}
    @see-constructor{make-processing-instruction}"))

(defclass text (leaf-node)
  ((data :initarg :data :accessor data))
  (:documentation
   "Instances of this class represent text nodes.
    @see-slot{data}
    @see-constructor{make-text}"))
