(in-package #:fxml.cxml)

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *sax-protocol*
    '((#:start-element (handler uri lname qname attrs))
      (#:end-element (handler uri lname qname))
      (#:characters (handler data-string))
      (#:start-document (handler))
      (#:end-document (handler))
      (#:comment (handler comment))
      (#:processing-instruction (handler target data))
      (#:unescaped (handler string))
      (#:start-cdata (handler))
      (#:end-cdata (handler))
      (#:start-prefix-mapping (handler prefix uri))
      (#:end-prefix-mapping (handler prefix))
      ;; DTDs.
      (#:start-dtd (handler name public-id system-id))
      (#:end-dtd (handler))
      (#:start-internal-subset (handler))
      (#:end-internal-subset (handler))
      (#:unparsed-internal-subset (handler string))
      (#:unparsed-entity-declaration (handler name public-id system-id notation-name))
      (#:external-entity-declaration (handler kind name public-id system-id))
      (#:internal-entity-declaration (handler kind name value))
      (#:notation-declaration (handler name public-id system-id))
      (#:element-declaration (handler name model))
      (#:attribute-declaration (handler element-name attribute-name type default))
      (#:entity-resolver (handler resolver))
      ;; Context.
      (#:line-number (handler))
      (#:column-number (handler))
      (#:system-id (handler))
      (#:xml-base (handler)))))

(defmacro define-sax-translations ()
  `(progn
     ,@(loop for spec in *sax-protocol*
             collect `(define-translation ,spec
                        :cxml-class sax:abstract-handler
                        :fxml-class fxml.sax:abstract-handler
                        :cxml-package :sax
                        :fxml-package :fxml.sax))))

(define-sax-translations)

(flet ((dtd-not-supported (dtd)
         (declare (ignore dtd))
         (warn "Persisting DTDs between FXML and CXML is not (yet) supported.")))
  (defmethod sax::dtd ((handler fxml.sax:abstract-handler) dtd)
    (dtd-not-supported dtd))
  (defmethod fxml.sax:dtd ((handler sax:abstract-handler) dtd)
    (dtd-not-supported dtd)))
