;;;; sax-proxy.lisp
;;;;
;;;; This file is part of the CXML parser, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Copyright (c) 2004 David Lichteblau
;;;; Author: David Lichteblau

(in-package :cxml)

(defclass sax-proxy ()
  ((chained-handler :initform nil
                    :initarg :chained-handler
                    :accessor proxy-chained-handler)))

(macrolet ((define-proxy-method (name (&rest args))
             `(defmethod ,name ((handler sax-proxy) ,@args)
                (,name (proxy-chained-handler handler) ,@args))))
  (define-proxy-method sax:start-document ())
  (define-proxy-method sax:start-element (uri lname qname attributes))
  (define-proxy-method sax:start-prefix-mapping (prefix uri))
  (define-proxy-method sax:characters (data))
  (define-proxy-method sax:processing-instruction (target data))
  (define-proxy-method sax:end-prefix-mapping (prefix))
  (define-proxy-method sax:end-element (namespace-uri local-name qname))
  (define-proxy-method sax:end-document ())
  (define-proxy-method sax:comment (data))
  (define-proxy-method sax:start-cdata ())
  (define-proxy-method sax:end-cdata ())
  (define-proxy-method sax:start-dtd (name public-id system-id))
  (define-proxy-method sax:end-dtd ())
  (define-proxy-method sax:start-internal-subset ())
  (define-proxy-method sax:end-internal-subset ())
  (define-proxy-method sax:unparsed-entity-declaration (name pub sys not))
  (define-proxy-method sax:external-entity-declaration (kind name pub sys))
  (define-proxy-method sax:internal-entity-declaration (kind name value))
  (define-proxy-method sax:notation-declaration (name public-id system-id))
  (define-proxy-method sax:element-declaration (name model))
  (define-proxy-method sax:attribute-declaration (elt attr type default))
  (define-proxy-method sax:entity-resolver (resolver))
  (define-proxy-method sax::dtd (dtd)))
