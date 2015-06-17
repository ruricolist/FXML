(in-package #:fxml.cxml)

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *sax-protocol*
    '((#:start-element handler uri lname qname attrs)
      (#:end-element handler uri lname qname)
      (#:characters handler data-string)
      (#:start-document handler)
      (#:end-document handler)
      (#:comment handler comment)
      (#:processing-instruction handler target data)
      (#:unescaped handler string)
      (#:start-cdata handler)
      (#:end-cdata handler)
      (#:start-prefix-mapping handler prefix uri)
      (#:end-prefix-mapping handler prefix)
      ;; DTDs.
      (#:start-dtd handler name public-id system-id)
      (#:end-dtd handler)
      (#:start-internal-subset handler)
      (#:end-internal-subset handler)
      (#:unparsed-internal-subset handler string)
      (#:unparsed-entity-declaration handler name public-id system-id notation-name)
      (#:external-entity-declaration handler kind name public-id system-id)
      (#:internal-entity-declaration handler kind name value)
      (#:notation-declaration handler name public-id system-id)
      (#:element-declaration handler name model)
      (#:attribute-declaration handler element-name attribute-name type default)
      (#:entity-resolver handler resolver)
      ;; Context.
      (#:line-number handler)
      (#:column-number handler)
      (#:system-id handler)
      (#:xml-base handler))))

(defmacro define-translation (spec)
  (flet ((find-symbol* (name package)
           (multiple-value-bind (found status)
               (find-symbol (string name) package)
             (unless found
               (error "No such symbol as ~a in ~a" name package))
             (unless (eql status :external)
               (error "~s is not external in ~a" found package))
             found)))
    (destructuring-bind (name handler . args) spec
      (let ((cxml-name (find-symbol* name :sax))
            (fxml-name (find-symbol* name :fxml.sax)))
        `(progn
           (defmethod ,cxml-name ((,handler fxml.sax:abstract-handler) ,@args)
             (handler-bind ((warning #'muffle-warning))
               (,fxml-name ,handler ,@args)))
           (defmethod ,fxml-name ((,handler sax:abstract-handler) ,@args)
             (handler-bind ((warning #'muffle-warning))
               (,cxml-name ,handler ,@args))))))))

(defmacro define-translations ()
  `(progn
     ,@(loop for spec in *sax-protocol*
             collect `(define-translation ,spec))))

(define-translations)
