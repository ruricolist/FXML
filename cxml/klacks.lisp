(in-package #:fxml.cxml)

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *klacks-protocol*
    '((#:close-source (source))
      (#:peek (source))
      (#:peek-value (source))
      (#:peek-next (source))
      (#:consume (source))
      (#:map-attributes (fn source))
      (#:list-attributes (source))
      (#:get-attribute (source lname &optional uri))
      (#:current-cdata-section-p (source))
      (#:map-current-namespace-declarations (fn source))
      (#:current-line-number (source))
      (#:current-column-number (source))
      (#:current-system-id (source))
      (#:current-xml-base (source))
      (#:find-namespace-binding (prefix source))
      (#:decode-qname (qname source)))))

(defmacro define-klacks-translations ()
  `(progn
     ,@(loop for spec in *klacks-protocol*
             collect `(define-translation ,spec
                        :cxml-class klacks:source
                        :fxml-class fxml.klacks:source
                        :cxml-package :klacks
                        :fxml-package :fxml.klacks))))

(define-klacks-translations)

(flet ((dtd-not-supported (dtd)
         (declare (ignore dtd))
         (warn "Persisting DTDs between FXML and CXML is not (yet) supported.")))
  (defmethod sax::dtd ((handler fxml.sax:abstract-handler) dtd)
    (dtd-not-supported dtd))
  (defmethod fxml.sax:dtd ((handler sax:abstract-handler) dtd)
    (dtd-not-supported dtd)))
