;;;; package.lisp -- Paketdefinition
;;;;
;;;; This file is part of the FXML parser, released under Lisp-LGPL.
;;;; See file COPYING for details.

(in-package :cl-user)

(defpackage :fxml.dom
  (:use)
  (:export
   ;; DOM 4 functions.
   #:next-element-sibling
   #:previous-element-sibling
   #:first-element-child
   #:last-element-child
   #:child-element-count
   #:contains
   #:get-elements-by-class-name

   ;; DOM 3 functions.
   #:text-content
   #:rename-node

   ;; DOM 2 functions
   #:owner-element
   #:import-node
   #:create-element-ns
   #:create-attribute-ns
   #:get-elements-by-tag-name-ns
   #:get-element-by-id
   #:get-named-item-ns
   #:set-named-item-ns
   #:remove-named-item-ns
   #:is-supported
   #:has-attributes
   #:namespace-uri
   #:prefix
   #:local-name
   #:internal-subset
   #:create-document-type
   #:create-document
   #:get-attribute-ns
   #:set-attribute-ns
   #:remove-attribute-ns
   #:get-attribute-node-ns
   #:set-attribute-node-ns
   #:has-attribute
   #:has-attribute-ns

   ;; DOM 1 functions
   #:has-feature
   #:doctype
   #:implementation
   #:document-element
   #:create-element
   #:create-document-fragment
   #:create-text-node
   #:create-comment
   #:create-cdata-section
   #:create-processing-instruction
   #:create-attribute
   #:create-entity-reference
   #:get-elements-by-tag-name
   #:node-name
   #:node-value
   #:node-type
   #:parent-node 
   #:child-nodes
   #:first-child
   #:last-child
   #:previous-sibling
   #:next-sibling
   #:attributes
   #:owner-document
   #:insert-before
   #:replace-child
   #:remove-child
   #:append-child
   #:has-child-nodes
   #:clone-node
   #:item
   #:length
   #:get-named-item
   #:set-named-item
   #:remove-named-item
   #:data
   #:substring-data
   #:append-data
   #:insert-data
   #:delete-data
   #:replace-data
   #:name
   #:specified
   #:value
   #:tag-name
   #:get-attribute
   #:set-attribute
   #:remove-attribute
   #:get-attribute-node
   #:set-attribute-node
   #:remove-attribute-node
   #:normalize
   #:split-text
   #:entities
   #:notations
   #:public-id
   #:system-id
   #:notation-name
   #:target
   #:code

   ;; IDL interfaces, exported "inofficially"
   #:node
   #:document
   #:document-fragment
   #:character-data
   #:attr
   #:element
   #:text
   #:comment
   #:cdata-section
   #:document-type
   #:notation
   #:entity
   #:entity-reference
   #:processing-instruction
   #:named-node-map
   ;; no classes:
;;;   #:dom-implementation
;;;   #:node-list

   ;;
   #:items

   ;;
   #:node-p
   #:document-p
   #:document-fragment-p
   #:character-data-p
   #:attribute-p
   #:element-p
   #:text-node-p
   #:comment-p
   #:cdata-section-p
   #:document-type-p
   #:notation-p
   #:entity-p
   #:entity-reference-p
   #:processing-instruction-p
   #:named-node-map-p

   #:map-node-list
   #:do-node-list
   #:map-node-map
   #:do-node-map
   #:create-document
   #:map-document))

(defclass fxml.dom:node () ())
(defclass fxml.dom:document (fxml.dom:node) ())
(defclass fxml.dom:document-fragment (fxml.dom:node) ())
(defclass fxml.dom:character-data (fxml.dom:node) ())
(defclass fxml.dom:attr (fxml.dom:node) ())
(defclass fxml.dom:element (fxml.dom:node) ())
(defclass fxml.dom:text (fxml.dom:character-data) ())
(defclass fxml.dom:comment (fxml.dom:character-data) ())
(defclass fxml.dom:cdata-section (fxml.dom:text) ())
(defclass fxml.dom:document-type (fxml.dom:node) ())
(defclass fxml.dom:notation (fxml.dom:node) ())
(defclass fxml.dom:entity (fxml.dom:node) ())
(defclass fxml.dom:entity-reference (fxml.dom:node) ())
(defclass fxml.dom:processing-instruction (fxml.dom:node) ())

(defclass fxml.dom:named-node-map () ())
