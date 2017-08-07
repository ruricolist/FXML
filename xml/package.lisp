;;;; package.lisp -- Paketdefinition
;;;;
;;;; This file is part of the FXML parser, released under Lisp-LGPL.
;;;; See file COPYING for details.

(in-package :cl-user)

(defpackage :fxml
  (:use :cl :fxml.runes :fxml.runes-encoding :split-sequence)
  (:import-from #:named-readtables #:in-readtable)
  (:import-from #:alexandria #:compose #:curry #:rcurry)
  (:import-from #:serapeum
    #:with-thunk
    #:defstruct-read-only
    #:block-compile)
  (:export
   ;; meta
   #:xml-character-p
   #:xml-characters-p
   #:name-start-rune-p
   #:name-rune-p
   #:nc-name-p
   
   ;; xstreams
   #:make-xstream
   #:make-rod-xstream
   #:close-xstream
   #:read-rune
   #:peek-rune
   #:unread-rune
   #:fread-rune
   #:fpeek-rune
   #:xstream-position
   #:xstream-line-number
   #:xstream-column-number
   #:xstream-plist
   #:xstream-encoding

   ;; xstream controller protocol
   #:read-octects
   #:xstream/close

   #:attribute-namespace-uri
   #:attribute-local-name
   #:attribute-qname
   #:attribute-value

   #:parse
   #:parse-rod                          ;For STP and testing.
   
   #:make-octet-vector-sink
   #:make-octet-stream-sink
   #:make-rod-sink
   #:make-string-sink
   #:make-character-stream-sink

   #:sink-encoding
   #:sink-omit-xml-declaration-p

   #:with-xml-output
   #:with-output-sink
   #:with-namespace
   #:with-element
   #:with-element*
   #:attribute
   #:attribute*
   #:unparse-attribute
   #:cdata
   #:text
   #:doctype
   #:processing-instruction
   #:comment
   #:unescaped

   #:xml-parse-error
   #:validity-error
   #:well-formedness-violation

   #:xml-security-error
   #:dtd-forbidden
   #:dtd-name
   #:dtd-sysid
   #:dtd-pubid
   #:entities-forbidden
   #:entity-name
   #:entity-value
   #:external-reference-forbidden
   #:external-reference-pubid
   #:external-reference-sysid

   #:undefined-entity
   #:undefined-entity-name
   #:undeclared-namespace
   #:undeclared-namespace-prefix

   #:current-encoding

   #:parse-dtd-file
   #:parse-dtd-stream
   #:make-validator

   #:*cache-all-dtds*
   #:*dtd-cache*
   #:getdtd
   #:remdtd
   #:make-dtd-cache
   #:clear-dtd-cache
   #:make-extid

   #:*catalog*
   #:*prefer*
   #:make-catalog
   #:resolve-uri
   #:resolve-extid

   #:make-recoder
   #:make-namespace-normalizer
   #:make-whitespace-normalizer
   #:rod-to-utf8-string
   #:utf8-string-to-rod

   #:broadcast-handler
   #:broadcast-handler-handlers
   #:make-broadcast-handler
   #:values-handler
   #:make-values-handler
   #:sax-proxy
   #:proxy-chained-handler

   #:make-source
   #:split-qname))
