;;;; xml-compat.lisp -- XMLS-compatible data structures
;;;;
;;;; This file is part of the CXML parser, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Developed 2004 for headcraft - http://headcraft.de/
;;;; Copyright: David Lichteblau

;;;; XXX Der namespace-Support in xmls kommt mir zweifelhaft vor.
;;;; Wir imitieren das soweit es gebraucht wurde bisher.

(defpackage cxml-xmls
  (:use :cl :runes)
  (:export #:make-node #:node-name #:node-ns #:node-attrs #:node-children
           #:make-xmls-builder #:map-node))

(in-package :cxml-xmls)


;;;; Knoten

(defun make-node (&key name ns attrs children)
  `(,(if ns (cons name ns) name)
       ,attrs
       ,@children))

(defun node-name (node)
  (let ((car (car node)))
    (if (consp car)
        (car car)
        car)))

(defun (setf node-name) (newval node)
  (let ((car (car node)))
    (if (consp car)
        (setf (car car) newval)
        (setf (car node) newval))))

(defun node-ns (node)
  (let ((car (car node)))
    (if (consp car)
        (cdr car)
        nil)))

(defun (setf node-ns) (newval node)
  (let ((car (car node)))
    (if (consp car)
        (setf (cdr car) newval)
        (setf (car node) (cons car newval)))
    newval))

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
     (root :initform nil :accessor root)
     (include-default-values :initform t
                             :initarg :include-default-values
                             :accessor include-default-values)))

(defun make-xmls-builder (&key (include-default-values t))
  (make-instance 'xmls-builder :include-default-values include-default-values))

(defmethod sax:end-document ((handler xmls-builder))
  (root handler))

(defmethod sax:start-element
    ((handler xmls-builder) namespace-uri local-name qname attributes)
  (declare (ignore namespace-uri))
  (setf local-name (or local-name qname))
  (let* ((attributes
          (loop
              for attr in attributes
              when (or (sax:attribute-specified-p attr)
                       (include-default-values handler))
              collect
                (list (sax:attribute-qname attr)
                      (sax:attribute-value attr))))
         (node (make-node :name local-name
                          :ns (let ((lq (length qname))
                                    (ll (length local-name)))
                                (if (eql lq ll)
                                    nil
                                    (subseq qname 0 (- lq ll 1))))
                          :attrs attributes))
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
    ;; Be careful to accept both rods and strings here, so that xmls can be
    ;; used with strings even if cxml is configured to use octet string rods.
    (if (typep prev '(or rod string))
        ;; um entities herum wird SAX:CHARACTERS mehrfach aufgerufen fuer
        ;; den gleichen Textknoten.  Hier muessen wir den bestehenden Knoten
        ;; erweitern, sonst ist das Dokument nicht normalisiert.
        ;; (XXX Oder sollte man besser den Parser entsprechend aendern?)
        (setf (car (node-children parent))
              (concatenate `(vector ,(array-element-type prev))
                           prev
                           data))
        (push data (node-children parent)))))


;;;; SAX-Treiber (fuer Serialisierung)

(defun map-node
    (handler node
     &key (include-xmlns-attributes sax:*include-xmlns-attributes*))
  (sax:start-document handler)
  (labels ((walk (node)
             (let* ((attlist
                     (compute-attributes node include-xmlns-attributes))
                    (lname (rod (node-name node)))
                    (qname (if (node-ns node)
			       (concatenate 'rod
				 (rod (node-ns node))
				 (rod ":")
				 lname)
			       lname)))
               (sax:start-element handler nil lname qname attlist)
               (dolist (child (node-children node))
                 (typecase child
                   (list (walk child))
                   ((or string rod) (sax:characters handler (rod child)))))
               (sax:end-element handler nil lname qname))))
    (walk node))
  (sax:end-document handler))

(defun compute-attributes (node xmlnsp)
  (remove nil
          (mapcar (lambda (a)
                    (destructuring-bind (name value) a
                      (if (or xmlnsp (not (cxml::xmlns-attr-p (rod name))))
                          (sax:make-attribute :qname (rod name)
                                              :value (rod value)
                                              :specified-p t)
                          nil)))
                  (node-attrs node))))
