;;;; space-normalizer.lisp -- whitespace removal
;;;;
;;;; This file is part of the FXML parser, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Copyright (c) 2005 David Lichteblau

(in-package :fxml)
(in-readtable :runes)

(defclass whitespace-normalizer (sax-proxy)
    ((attributes :initform '(t) :accessor xml-space-attributes)
     (models :initform nil :accessor xml-space-models)
     (dtd :initarg :dtd :accessor xml-space-dtd)))

(defun make-whitespace-normalizer (chained-handler &optional dtd)
  (make-instance 'whitespace-normalizer
    :dtd dtd
    :chained-handler chained-handler))

(defmethod fxml.sax::dtd ((handler whitespace-normalizer) dtd)
  (unless (xml-space-dtd handler)
    (setf (xml-space-dtd handler) dtd)))

(defmethod fxml.sax:start-element
    ((handler whitespace-normalizer) uri lname qname attrs)
  (declare (ignore uri lname))
  (let ((dtd (xml-space-dtd handler)))
    (when dtd
      (let ((xml-space
              (fxml.sax:find-attribute "xml:space" attrs)))
	(push (if xml-space
		  (rod= (rod (fxml.sax:attribute-value xml-space)) #"default")
		  (car (xml-space-attributes handler)))
	      (xml-space-attributes handler)))
      (let* ((e (fxml::find-element (rod qname) dtd))
	     (cspec (when e (fxml::elmdef-content e))))
	(push (and (consp cspec)
		   (not (and (eq (car cspec) '*)
			     (let ((subspec (second cspec)))
			       (and (eq (car subspec) 'or)
				    (eq (cadr subspec) :PCDATA))))))
	      (xml-space-models handler)))))
  (call-next-method))

(defmethod fxml.sax:characters ((handler whitespace-normalizer) data)
  (cond
    ((and (xml-space-dtd handler)
	  (car (xml-space-attributes handler))
	  (car (xml-space-models handler)))
      (unless (every #'white-space-rune-p (rod data))
	(warn "non-whitespace character data in element content")
	(call-next-method)))
    (t
      (call-next-method))))

(defmethod fxml.sax:end-element ((handler whitespace-normalizer) uri lname qname)
  (declare (ignore uri lname qname))
  (when (xml-space-dtd handler)
    (pop (xml-space-attributes handler))
    (pop (xml-space-models handler)))
  (call-next-method))
