;;;; sax-proxy.lisp
;;;;
;;;; This file is part of the CXML parser, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Copyright (c) 2004 David Lichteblau
;;;; Author: David Lichteblau

(in-package :cxml)

(defclass broadcast-handler (sax:abstract-handler)
  ((handlers :initform nil
	     :initarg :handlers
	     :accessor broadcast-handler-handlers)))

(defun make-broadcast-handler (&rest handlers)
  (make-instance 'broadcast-handler :handlers handlers))

(defclass sax-proxy (broadcast-handler)
  ())

(defmethod initialize-instance
    :after ((instance sax-proxy) &key chained-handler)
  (setf (proxy-chained-handler instance) chained-handler))

(defmethod proxy-chained-handler ((instance sax-proxy))
  (car (broadcast-handler-handlers instance)))

(defmethod (setf proxy-chained-handler) (newval (instance sax-proxy))
  (setf (broadcast-handler-handlers instance) (list newval)))

#-rune-is-character
(defmethod hax:%want-strings-p ((handler broadcast-handler))
  (hax:%want-strings-p (car (broadcast-handler-handlers instance))))

(macrolet ((define-proxy-method (name (&rest args))
             `(defmethod ,name ((handler broadcast-handler) ,@args)
                (let (result)
		  (dolist (next (broadcast-handler-handlers handler))
		    (setf result (,name next ,@args)))
		  result))))
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

(defmethod sax:register-sax-parser :after ((handler broadcast-handler) parser)
  (dolist (next (broadcast-handler-handlers handler))
    (sax:register-sax-parser next parser)))
