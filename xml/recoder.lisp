;;;; recoder.lisp -- SAX handler for string conversion
;;;;
;;;; This file is part of the FXML parser, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Developed 2004 for headcraft - http://headcraft.de/
;;;; Copyright: David Lichteblau

(in-package :fxml)

(defclass recoder ()
    ((recoder :initarg :recoder :accessor recoder)
     (chained-handler :initarg :chained-handler :accessor chained-handler)))

(defun make-recoder (chained-handler recoder-fn)
  (make-instance 'recoder
    :recoder recoder-fn
    :chained-handler chained-handler))

(macrolet ((%string (rod)
             `(let ((rod ,rod))
                (if (typep rod '(or rod string))
                    (funcall (recoder handler) rod)
                    rod)))
           (defwrapper (name (&rest args) &rest forms)
             `(defmethod ,name ((handler recoder) ,@args)
                (,name (chained-handler handler) ,@forms))))
  (defwrapper fxml.sax:start-document ())

  (defwrapper fxml.sax:start-element
      (namespace-uri local-name qname attributes)
    (%string namespace-uri)
    (%string local-name)
    (%string qname)
    (mapcar (lambda (attr)
              (fxml.sax:make-attribute
               :namespace-uri (%string (fxml.sax:attribute-namespace-uri attr))
               :local-name (%string (fxml.sax:attribute-local-name attr))
               :qname (%string (fxml.sax:attribute-qname attr))
               :value (%string (fxml.sax:attribute-value attr))
               :specified-p (fxml.sax:attribute-specified-p attr)))
            attributes))

  (defwrapper fxml.sax:start-prefix-mapping (prefix uri)
    (%string prefix)
    (%string uri))

  (defwrapper fxml.sax:characters (data)
    (%string data))

  (defwrapper fxml.sax:processing-instruction (target data)
    (%string target)
    (%string data))

  (defwrapper fxml.sax:end-prefix-mapping (prefix)
    (%string prefix))

  (defwrapper fxml.sax:end-element (namespace-uri local-name qname)
    (%string namespace-uri)
    (%string local-name)
    (%string qname))

  (defwrapper fxml.sax:end-document ())

  (defwrapper fxml.sax:comment (data)
    (%string data))

  (defwrapper fxml.sax:start-cdata ())

  (defwrapper fxml.sax:end-cdata ())

  (defwrapper fxml.sax:start-dtd (name public-id system-id)
    (%string name)
    (%string public-id)
    (%string system-id))

  (defwrapper fxml.sax:start-internal-subset ())
  (defwrapper fxml.sax:end-internal-subset ())

  (defwrapper fxml.sax:end-dtd ())

  (defwrapper fxml.sax:unparsed-entity-declaration
      (name public-id system-id notation-name)
    (%string name)
    (%string public-id)
    (%string system-id)
    (%string notation-name))

  (defwrapper fxml.sax:external-entity-declaration
      (kind name public-id system-id)
    (%string kind)
    (%string name)
    (%string public-id)
    (%string system-id))

  (defwrapper fxml.sax:internal-entity-declaration
      (kind name value)
    kind
    (%string name)
    (%string value))

  (defwrapper fxml.sax:notation-declaration
      (name public-id system-id)
    (%string name)
    (%string public-id)
    (%string system-id))

  (defwrapper fxml.sax:element-declaration (name model)
    (%string name)
    model)

  (defwrapper fxml.sax:attribute-declaration
      (element-name attribute-name type default)
    (%string element-name)
    (%string attribute-name)
    (%string type)
    (%string default))

  (defwrapper fxml.sax:entity-resolver
      (resolver)
    resolver)

  (defwrapper fxml.sax::dtd
      (dtd)
    dtd))
