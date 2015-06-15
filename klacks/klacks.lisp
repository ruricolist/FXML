;;; -*- Mode: Lisp; readtable: runes; -*-
;;;  (c) copyright 2007 David Lichteblau

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :fxml)

(defclass fxml.klacks:source ()
    (
     ;; fixme, terrible DTD kludges
     (internal-declarations)
     (external-declarations :initform nil)
     (dom-impl-dtd :initform nil)
     (dom-impl-entity-resolver :initform nil)))

(defgeneric fxml.klacks:close-source (source))

(defgeneric fxml.klacks:peek (source))
(defgeneric fxml.klacks:peek-value (source))
(defgeneric fxml.klacks:consume (source))

(defgeneric fxml.klacks:map-attributes (fn source))
(defgeneric fxml.klacks:list-attributes (source))
(defgeneric fxml.klacks:get-attribute (source lname &optional uri))
;;;(defgeneric fxml.klacks:current-uri (source))
;;;(defgeneric fxml.klacks:current-lname (source))
;;;(defgeneric fxml.klacks:current-qname (source))
;;;(defgeneric fxml.klacks:current-characters (source))
(defgeneric fxml.klacks:current-cdata-section-p (source))
(defgeneric fxml.klacks:map-current-namespace-declarations (fn source))

(defgeneric fxml.klacks:current-line-number (source))
(defgeneric fxml.klacks:current-column-number (source))
(defgeneric fxml.klacks:current-system-id (source))
(defgeneric fxml.klacks:current-xml-base (source))

(defgeneric fxml.klacks:find-namespace-binding (prefix source))
(defgeneric fxml.klacks:decode-qname (qname source))

(defmacro fxml.klacks:with-open-source ((var source) &body body)
  `(let ((,var ,source))
     (unwind-protect
	 (progn ,@body)
       (fxml.klacks:close-source ,var))))

(defun fxml.klacks:current-uri (source)
  (multiple-value-bind (key uri lname qname) (fxml.klacks:peek source)
    (declare (ignore lname qname))
    (check-type key (member :start-element :end-element))
    uri))

(defun fxml.klacks:current-lname (source)
  (multiple-value-bind (key uri lname qname) (fxml.klacks:peek source)
    (declare (ignore uri qname))
    (check-type key (member :start-element :end-element))
    lname))

(defun fxml.klacks:current-qname (source)
  (multiple-value-bind (key uri lname qname) (fxml.klacks:peek source)
    (declare (ignore uri lname))
    (check-type key (member :start-element :end-element))
    qname))

(defun fxml.klacks:current-characters (source)
  (multiple-value-bind (key characters) (fxml.klacks:peek source)
    (check-type key (member :characters))
    characters))

(defun fxml.klacks:consume-characters (source)
  (with-output-to-string (s)
    (while (eq (fxml.klacks:peek source) :characters)
      (write-string (fxml.klacks:current-characters source) s)
      (fxml.klacks:consume source))))

(defun fxml.klacks:serialize-event (source handler &key (consume t))
  (multiple-value-bind (key a b c) (fxml.klacks:peek source)
    (let ((result nil))
      (case key
	(:start-document
	  (fxml.sax:start-document handler)
	  (loop for (prefix . uri) in *initial-namespace-bindings* do
	       (fxml.sax:start-prefix-mapping handler prefix uri)))
	(:characters
	  (cond
	    ((fxml.klacks:current-cdata-section-p source)
	      (fxml.sax:start-cdata handler)
	      (fxml.sax:characters handler a)
	      (fxml.sax:end-cdata handler))
	    (t
	      (fxml.sax:characters handler a))))
	(:processing-instruction
	  (fxml.sax:processing-instruction handler a b))
	(:comment
	  (fxml.sax:comment handler a))
	(:dtd
	  (fxml.sax:start-dtd handler a b (and c (uri-rod c)))
	  (when (slot-boundp source 'internal-declarations)
	    (fxml.sax:start-internal-subset handler)
	    (serialize-declaration-kludge
	     (slot-value source 'internal-declarations)
	     handler)
	    (fxml.sax:end-internal-subset handler))
	  (serialize-declaration-kludge
	   (slot-value source 'external-declarations)
	   handler)
	  (fxml.sax:end-dtd handler)
	  (fxml.sax:entity-resolver handler
			       (slot-value source 'dom-impl-entity-resolver))
	  (fxml.sax::dtd handler (slot-value source 'dom-impl-dtd)))
	(:start-element
	  (fxml.klacks:map-current-namespace-declarations
	   (lambda (prefix uri)
	     (fxml.sax:start-prefix-mapping handler prefix uri))
	   source)
	  (fxml.sax:start-element handler a b c (fxml.klacks:list-attributes source)))
	(:end-element
	  (fxml.sax:end-element handler a b c)
	  (fxml.klacks:map-current-namespace-declarations
	   (lambda (prefix uri)
	     (declare (ignore uri))
	     (fxml.sax:end-prefix-mapping handler prefix))
	   source))
	(:end-document
	 (loop for (prefix . nil) in *initial-namespace-bindings* do
	      (fxml.sax:end-prefix-mapping handler prefix))
	  (setf result (fxml.sax:end-document handler)))
	((nil)
	  (error "serialize-event read past end of document"))
	(t
	  (error "unexpected klacks key: ~A" key)))
      (when consume
	(fxml.klacks:consume source))
      result)))

(defun serialize-declaration-kludge (list handler)
  (loop
      for (fn . args) in list
      do (apply fn handler args)))

(defun fxml.klacks:serialize-source (source handler)
  (loop
    (let ((document (fxml.klacks:serialize-event source handler)))
      (when document
	(return document)))))

(defclass klacksax (fxml.sax:sax-parser)
    ((source :initarg :source)))

(defmethod fxml.sax:line-number ((parser klacksax))
  (fxml.klacks:current-line-number (slot-value parser 'source)))

(defmethod fxml.sax:column-number ((parser klacksax))
  (fxml.klacks:current-column-number (slot-value parser 'source)))

(defmethod fxml.sax:system-id ((parser klacksax))
  (fxml.klacks:current-system-id (slot-value parser 'source)))

(defmethod fxml.sax:xml-base ((parser klacksax))
  (fxml.klacks:current-xml-base (slot-value parser 'source)))

(defun fxml.klacks:serialize-element (source handler &key (document-events t))
  (unless (eq (fxml.klacks:peek source) :start-element)
    (error "not at start of element"))
  (fxml.sax:register-sax-parser handler (make-instance 'klacksax :source source))
  (when document-events
    (fxml.sax:start-document handler))
  (labels ((recurse ()
	     (fxml.klacks:serialize-event source handler)
	     (loop
	       (let ((key (fxml.klacks:peek source)))
		 (ecase key
		   (:start-element (recurse))
		   (:end-element (return))
		   ((:characters :comment :processing-instruction)
		     (fxml.klacks:serialize-event source handler)))))
	     (fxml.klacks:serialize-event source handler)))
    (recurse))
  (when document-events
    (fxml.sax:end-document handler)))

(defun fxml.klacks:find-element (source &optional lname uri)
  (loop
    (multiple-value-bind (key current-uri current-lname current-qname)
	(fxml.klacks:peek source)
      (case key
	((nil)
	  (return nil))
	(:start-element
	  (when (and (eq key :start-element)
		     (or (null lname)
			 (equal lname (fxml.klacks:current-lname source)))
		     (or (null uri)
			 (equal uri (fxml.klacks:current-uri source))))
	    (return
	      (values key current-uri current-lname current-qname)))))
      (fxml.klacks:consume source))))

(defun fxml.klacks:find-event (source key)
  (loop
    (multiple-value-bind (this a b c)
	(fxml.klacks:peek source)
      (cond
	((null this)
	  (return nil))
	((eq this key)
	  (return (values this a b c))))
      (fxml.klacks:consume source))))

(define-condition fxml.klacks:klacks-error (xml-parse-error) ())

(defun klacks-error (fmt &rest args)
  (%error 'fxml.klacks:klacks-error
	  nil
	  (format nil "Klacks assertion failed: ~?" fmt args)))

(defun fxml.klacks:expect (source key &optional u v w)
  (multiple-value-bind (this a b c)
      (fxml.klacks:peek source)
    (unless (eq this key) (klacks-error "expected ~A but got ~A" key this))
    (when (and u (not (equal a u)))
      (klacks-error "expected ~A but got ~A" u a))
    (when (and v (not (equal b v)))
      (klacks-error "expected ~A but got ~A" v b))
    (when (and w (not (equal c w)))
      (klacks-error "expected ~A but got ~A" w c))
    (values this a b c)))

(defun fxml.klacks:skip (source key &optional a b c)
  (fxml.klacks:expect source key a b c)
  (fxml.klacks:consume source))

(defun invoke-expecting-element (fn source &optional lname uri)
  (multiple-value-bind (key a b)
      (fxml.klacks:peek source)
    (unless (eq key :start-element)
      (klacks-error "expected ~A but got ~A" (or lname "element") key))
    (when (and uri (not (equal a uri)))
      (klacks-error "expected ~A but got ~A" uri a))
    (when (and lname (not (equal b lname)))
      (klacks-error "expected ~A but got ~A" lname b))
    (multiple-value-prog1
	(funcall fn)
      (fxml.klacks:skip source :end-element a b))))

(defmacro fxml.klacks:expecting-element ((source &optional lname uri) &body body)
  `(invoke-expecting-element (lambda () ,@body) ,source ,lname ,uri))
