;;;; xmlns-normalizer.lisp -- DOM 3-style namespace normalization
;;;;
;;;; This file is part of the FXML parser, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Copyright (c) 2005 David Lichteblau

;;;; Hier eine Variante des reichlich furchtbaren Algorithmus zur
;;;; Namespace-Normalisierung aus DOM 3 Core.[1]
;;;;
;;;; Gebraucht wir die Sache, weil Element- und Attributknoten in DOM
;;;; zwar ein Prefix-Attribut speichern, massgeblich fuer ihren Namespace
;;;; aber nur die URI sein soll.  Und eine Anpassung der zugehoerigen
;;;; xmlns-Attribute findet bei Veraenderungen im DOM-Baum nicht statt,
;;;; bzw. wird dem Nutzer ueberlassen.
;;;;
;;;; Daher muss letztlich spaetestens beim Serialisieren eine
;;;; Namespace-Deklaration fuer die angegebene URI nachgetragen und das
;;;; Praefix ggf. umbenannt werden, damit am Ende doch etwas
;;;; Namespace-konformes heraus kommt.
;;;;
;;;; Und das nennen sie dann Namespace-Support.
;;;;
;;;; [1] http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/namespaces-algorithms.html#normalizeDocumentAlgo

(in-package :fxml)

(defclass namespace-normalizer (sax-proxy)
    ((xmlns-stack :initarg :xmlns-stack :accessor xmlns-stack)))

(defvar *xmlns-namespace* #"http://www.w3.org/2000/xmlns/")

(defun make-namespace-normalizer (chained-handler)
  "@arg[chained-handler]{A @class{SAX handler}.}
   @return{A @class{SAX handler}.}

   Return a SAX handler that performs @a[http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/namespaces-algorithms.html#normalizeDocumentAlgo]{DOM
     3-style namespace normalization} on attribute lists in
   @fun{fxml.sax:start-element} events before passing them on the next handler."
  (make-instance 'namespace-normalizer
    :xmlns-stack (list (mapcar (lambda (cons)
				 (make-xmlns-attribute (car cons) (cdr cons)))
			       *initial-namespace-bindings*))
    :chained-handler chained-handler))

(defun normalizer-find-prefix (handler prefix)
  (when (zerop (length prefix))
    (setf prefix #"xmlns"))
  (block t
    (dolist (bindings (xmlns-stack handler))
      (dolist (attribute bindings)
	(when (rod= (fxml.sax:attribute-local-name attribute) prefix)
	  (return-from t attribute))))))

(defun normalizer-find-uri (handler uri)
  (block t
    (dolist (bindings (xmlns-stack handler))
      (dolist (attribute bindings)
	(when (and (rod= (fxml.sax:attribute-value attribute) uri)
		   ;; default-namespace interessiert uns nicht
		   (not (rod= (fxml.sax:attribute-qname attribute) #"xmlns")))
	  (return-from t attribute))))))

(defun make-xmlns-attribute (prefix uri)
  (if (and (plusp (length prefix)) (not (equal prefix #"xmlns")))
      (fxml.sax:make-attribute
       :qname (concatenate 'rod #"xmlns:" prefix)
       :namespace-uri *xmlns-namespace*
       :local-name prefix
       :value uri)
      (fxml.sax:make-attribute
       :qname #"xmlns"
       :namespace-uri *xmlns-namespace*
       :local-name #"xmlns"
       :value uri)))

(defun rename-attribute (a new-prefix)
  (setf (fxml.sax:attribute-qname a)
	(concatenate 'rod new-prefix #":" (fxml.sax:attribute-local-name a))))

(defmethod fxml.sax:start-element
    ((handler namespace-normalizer) uri lname qname attrs)
  (when (null uri)
    (setf uri #""))
  (let ((normal-attrs '()))
    (push nil (xmlns-stack handler))
    (dolist (a attrs)
      (if (rod= *xmlns-namespace* (fxml.sax:attribute-namespace-uri a))
	  (push a (car (xmlns-stack handler)))
	  (push a normal-attrs)))
    (flet ((push-namespace (prefix uri)
	     (let ((new (make-xmlns-attribute prefix uri)))
	       (unless (find (fxml.sax:attribute-qname new)
			     attrs
			     :test #'rod=
			     :key #'fxml.sax:attribute-qname)
		 (push new (car (xmlns-stack handler)))
		 (push new attrs)))))
      (multiple-value-bind (prefix local-name) (split-qname qname)
	(setf lname local-name)
	(let ((binding (normalizer-find-prefix handler prefix)))
	  (cond
	    ((null binding)
	      (unless (and (null prefix) (zerop (length uri)))
		(push-namespace prefix uri)))
	    ((rod= (fxml.sax:attribute-value binding) uri))
	    ((member binding (car (xmlns-stack handler)))
	      (setf (fxml.sax:attribute-value binding) uri))
	    (t
	      (push-namespace prefix uri)))))
      (dolist (a normal-attrs)
	(let ((u (fxml.sax:attribute-namespace-uri a)))
	  (when u
	    (let* ((prefix (split-qname (fxml.sax:attribute-qname a)))
		   (prefix-binding
		    (when prefix
		      (normalizer-find-prefix handler prefix))))
	      (when (or (null prefix-binding)
			(not (rod= (fxml.sax:attribute-value prefix-binding) u)))
		(let ((uri-binding (normalizer-find-uri handler u)))
		  (cond
		    (uri-binding
		      (rename-attribute
		       a
		       (fxml.sax:attribute-local-name uri-binding)))
		    ((and prefix (null prefix-binding))
		      (push-namespace prefix u))
		    (t
		      (loop
			  for i from 1
			  for prefix = (rod (format nil "NS~D" i))
			  unless (normalizer-find-prefix handler prefix)
			  do
			    (push-namespace prefix u)
			    (rename-attribute a prefix)
			    (return))))))))))))
  (fxml.sax:start-element (proxy-chained-handler handler) uri lname qname attrs))

(defmethod fxml.sax:end-element ((handler namespace-normalizer) uri lname qname)
  (pop (xmlns-stack handler))
  (fxml.sax:end-element (proxy-chained-handler handler) (or uri #"") lname qname))
