;;;; package.lisp -- Paketdefinition
;;;;
;;;; This file is part of the CXML parser, released under (L)LGPL.
;;;; See file COPYING for details.

(in-package :cl-user)

(defpackage :cxml
  (:use :cl :runes :encoding)
  (:import-from #+sbcl :sb-gray
                #+allegro :excl
                #+cmu :ext
                #+clisp :gray
                #+openmcl :ccl
                #+lispworks :stream
                #-(or sbcl allegro cmu clisp openmcl lispworks) ...
                #:fundamental-binary-input-stream
                #-(or clisp openmcl) #:stream-read-sequence
                stream-read-byte)
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

   #:parse-dtd-file
   #:parse-dtd-stream
   #:validity-error
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
   #:proxy-chained-handler))
