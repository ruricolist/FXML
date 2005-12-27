;;;; package.lisp -- Paketdefinition
;;;;
;;;; This file is part of the CXML parser, released under Lisp-LGPL.
;;;; See file COPYING for details.

(in-package :cl-user)

(defpackage :cxml
  (:use :cl :runes :runes-encoding :trivial-gray-streams)
  (:export
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
   
   #:parse-file
   #:parse-stream
   ;; XXX encoding is mis-handled by parse-string, don't export it
   ;; #:parse-string
   #:parse-octets

   #:make-character-stream-sink
   #:make-octet-vector-sink
   #:make-octet-stream-sink
   #:unparse-document
   #:unparse-document-to-octets

   #:with-xml-output
   #:with-element
   #:attribute
   #:cdata
   #:text

   #:xml-parse-error
   #:well-formedness-violation
   #:validity-error

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
   #:sax-proxy
   #:proxy-chained-handler
   #:make-namespace-normalizer
   #:rod-to-utf8-string
   #:utf8-string-to-rod))
