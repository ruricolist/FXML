(in-package #:fxml.cxml)

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *accessors*
    '(#:attribute-namespace-uri
      #:attribute-local-name
      #:attribute-qname
      #:attribute-value
      #:attribute-specified-p)))

(defmacro define-accessor-translation (accessor)
  (let ((cxml-name (find-symbol (string accessor) :sax))
        (fxml-name (find-symbol (string accessor) :fxml.sax)))
    `(progn
       (defmethod ,cxml-name ((attribute fxml.sax:standard-attribute))
         (,fxml-name attribute))
       (defmethod (setf ,cxml-name) (value (attribute fxml.sax:standard-attribute))
         (setf (,fxml-name attribute) value))
       
       (defmethod ,fxml-name ((attribute sax::standard-attribute))
         (,cxml-name attribute))
       (defmethod (setf ,fxml-name) (value (attribute sax::standard-attribute))
         (setf (,cxml-name attribute) value)))))

(defmacro define-accessor-translations ()
  `(progn
     ,@(loop for accessor in *accessors*
             collect `(define-accessor-translation ,accessor))))

(define-accessor-translations)
