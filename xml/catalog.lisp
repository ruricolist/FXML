;;;; catalogs.lisp -- XML Catalogs  -*- Mode: Lisp; readtable: runes -*-
;;;;
;;;; This file is part of the CXML parser, released under (L)LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Developed 2004 for headcraft - http://headcraft.de/
;;;; Copyright: David Lichteblau

(in-package :cxml)

;;; http://www.oasis-open.org/committees/entity/spec.html
;;;
;;; Bugs:
;;;   - We validate using the Catalog DTD while parsing, which is too strict
;;;     and will will fail to parse files using other parser's extensions.
;;;     (Jedenfalls behauptet das die Spec.)
;;;     A long-term solution might be an XML Schema validator.
;;;
;;; XXX Das mit dem :dtd geht natuerlich gar nicht.  Die Option muss weg. 

(defvar *prefer* nil)
(defvar *catalog*
    '(;; libxml standard
      "/etc/xml/catalog"
      ;; FreeBSD
      "/usr/local/share/xml/catalog.ports"))

(defparameter *catalog-dtd* nil)

(defun parse-catalog (files)
  (let ((result '()))
    (loop
      (let ((file (pop files)))
        (unless file
          (return))
        (multiple-value-bind (entries next) (parse-catalog-file file)
          (setf result (append result entries))
          (setf files (append next files)))))
    result))

(defun parse-catalog-file (uri)
  (handler-case
      (parse-catalog-file/strict uri)
    (file-error () nil)
    (parser-error () nil)))

(defun parse-catalog-file/strict (uri)
  (when (stringp uri)
    (setf uri (puri:parse-uri uri)))
  (unless *catalog-dtd*
    (let ((cxml
           (slot-value (asdf:find-system :cxml) 'asdf::relative-pathname)))
      (setf *catalog-dtd*
            (parse-dtd-file (merge-pathnames "catalog.dtd" cxml)))))
  (with-open-stream (s (open (uri-to-pathname uri)
                             :element-type '(unsigned-byte 8)
                             :direction :input))
    (parse-stream s
                  (make-instance 'catalog-parser :uri uri)
                  :validate t
                  ;; XXX das geht nicht
                  :dtd *catalog-dtd*)))

(defclass catalog-parser ()
  ((entries :initform '() :accessor entries)
   (next :initform '() :accessor next)
   (prefer-stack :initform (list *prefer*) :accessor prefer-stack)
   (base-stack :accessor base-stack)))

(defmethod initialize-instance :after
    ((instance catalog-parser) &key uri)
  (setf (base-stack instance) (list uri)))

(defmethod prefer ((handler catalog-parser))
  (car (prefer-stack handler)))

(defmethod base ((handler catalog-parser))
  (car (base-stack handler)))

(defun get-attribute/lname (name attributes)
  (member name attributes
          :key (lambda (a)
                 (or (sax:attribute-local-name a)
                     (sax:attribute-qname a)))
          :test #'rod=))

(defmethod sax:start-element ((handler catalog-parser) uri lname qname attrs)
  (declare (ignore uri))
  (setf lname (or lname qname))
  ;; we can dispatch on lnames only because we validate against the DTD,
  ;; which disallows other namespaces.
  (push (string-or (get-attribute/lname #"prefer" attrs) (prefer handler))
        (prefer-stack handler))
  (push (string-or (get-attribute/lname #"base" attrs) (base handler))
        (base-stack handler))
  (cond
    ((rod= lname #"public")
      (push (list :public
                  (get-attribute/lname #"publicId" attrs)
                  (puri:merge-uris
                   (puri:parse-uri (get-attribute/lname #"uri" attrs))
                   (base handler)))
            (entries handler)))
    ((rod= lname #"system")
      (push (list :system
                  (get-attribute/lname #"systemId" attrs)
                  (puri:merge-uris
                   (puri:parse-uri (get-attribute/lname #"uri" attrs))
                   (base handler)))
            (entries handler)))
    ((rod= lname #"uri")
      (push (list :uri
                  (get-attribute/lname #"name" attrs)
                  (puri:merge-uris
                   (puri:parse-uri (get-attribute/lname #"uri" attrs))
                   (base handler)))
            (entries handler)))
    ((rod= lname #"rewriteSystem")
      (push (list :rewrite-system
                  (get-attribute/lname #"systemIdStartString" attrs)
                  (get-attribute/lname #"rewritePrefix" attrs))
            (entries handler)))
    ((rod= lname #"rewriteURI")
      (push (list :rewrite-uri
                  (get-attribute/lname #"uriStartString" attrs)
                  (get-attribute/lname #"rewritePrefix" attrs))
            (entries handler)))
    ((rod= lname #"delegatePublic")
      (push (list :delegate-public
                  (get-attribute/lname #"publicIdStartString" attrs)
                  (puri:merge-uris
                   (puri:parse-uri (get-attribute/lname #"catalog" attrs))
                   (base handler)))
            (entries handler)))
    ((rod= lname #"delegateSystem")
      (push (list :delegate-system
                  (get-attribute/lname #"systemIdStartString" attrs)
                  (puri:merge-uris
                   (puri:parse-uri (get-attribute/lname #"catalog" attrs))
                   (base handler)))
            (entries handler)))
    ((rod= lname #"delegateURI")
      (push (list :delegate-uri
                  (get-attribute/lname #"uriStartString" attrs)
                  (puri:merge-uris
                   (puri:parse-uri (get-attribute/lname #"catalog" attrs))
                   (base handler)))
            (entries handler)))
    ((rod= lname #"nextCatalog")
      (push (puri:merge-uris
             (puri:parse-uri (get-attribute/lname #"catalog" attrs))
             (base handler))
            (next handler)))))

(defmethod sax:end-element ((handler catalog-parser) uri lname qname)
  (declare (ignore uri lname qname))
  (pop (base-stack handler))
  (pop (prefer-stack handler)))

(defmethod sax:end-document ((handler catalog-parser))
  (values (reverse (entries handler)) (reverse (next handler))))
