;;;; xml-compat.lisp -- XMLS-compatible data structures
;;;;
;;;; This file is part of the CXML parser, released under (L)LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Copyright (c) 2004 headcraft GmbH
;;;; Author: David Lichteblau

(defpackage cxml-xmls
  (:use :cl)
  (:export #:make-node #:node-name #:node-attrs #:node-children
           #:make-xmls-builder #:map-node))

(in-package :cxml-xmls)


;;;; Knoten

;; XXX Wie namespaces in xmls funktionieren nsollen verstehe ich noch nicht so
;; ganz.  Daher verzichte ich vorerst auf NODE-NS und verwende durchweg QNAMEs.
(defun make-node (&key name attrs children)
  `(,name ,attrs ,@children))

(defun node-name (node)
  (car node))

(defun (setf node-name) (newval node)
  (setf (car node) newval))

(defun node-attrs (node)
  (cadr node))

(defun (setf node-attrs) (newval node)
  (setf (cadr node) newval))

(defun node-children (node)
  (cddr node))

(defun (setf node-children) (newval node)
  (setf (cddr node) newval))


;;;; SAX-Handler (Parser)

(defclass xmls-builder ()
    ((element-stack :initform nil :accessor element-stack)
     (root :initform nil :accessor root)))

(defun make-xmls-builder ()
  (make-instance 'xmls-builder))

(defmethod sax:end-document ((handler xmls-builder))
  (root handler))

(defmethod sax:start-element
    ((handler xmls-builder) namespace-uri local-name qname attributes)
  (declare (ignore namespace-uri local-name))
  (let* ((attributes
          (mapcar (lambda (attr)
                    (list (sax:attribute-qname attr)
                          (sax:attribute-value attr)))
                  attributes))
         (node (make-node :name qname :attrs attributes))
         (parent (car (element-stack handler))))
    (if parent
        (push node (node-children parent))
        (setf (root handler) node))
    (push node (element-stack handler))))

(defmethod sax:end-element
    ((handler xmls-builder) namespace-uri local-name qname)
  (declare (ignore namespace-uri local-name qname))
  (let ((node (pop (element-stack handler))))
    (setf (node-children node) (reverse (node-children node)))))

(defmethod sax:characters ((handler xmls-builder) data)
  (let* ((parent (car (element-stack handler)))
         (prev (car (node-children parent))))
    (if (stringp prev)
        ;; um entities herum wird SAX:CHARACTERS mehrfach aufgerufen fuer
        ;; den gleichen Textknoten.  Hier muessen wir den bestehenden Knoten
        ;; erweitern, sonst ist das Dokument nicht normalisiert.
        ;; (XXX Oder sollte man besser den Parser entsprechend aendern?)
        (setf (car (node-children parent))
              (concatenate 'runes:rod prev data))
        (push data (node-children parent)))))


;;;; SAX-Treiber (fuer Serialisierung)

(defun map-node
    (handler node
     &key (include-xmlns-attributes sax:*include-xmlns-attributes*))
  (sax:start-document handler)
  (labels ((walk (node)
             (let ((attlist
                    (compute-attributes node include-xmlns-attributes))
                   (qname (node-name node)))
               ;; fixme: namespaces
               (sax:start-element handler nil nil qname attlist)
               (dolist (child (node-children node))
                 (typecase child
                   (list (walk child))
                   (string (sax:characters handler child))))
               (sax:end-element handler nil nil qname))))
    (walk node))
  (sax:end-document handler))

(defun compute-attributes (node xmlnsp)
  (remove nil
          (mapcar (lambda (a)
                    (destructuring-bind (name value) a
                      (if (or xmlnsp (not (cxml::xmlns-attr-p name)))
                          (sax:make-attribute :qname name
                                              :value value
                                              :specified-p t)
                          nil)))
                  (node-attrs node))))
