(in-package #:css-selectors)

(defmethod tag-name ((elt fxml.dom:element))
  (fxml.dom:tag-name elt))

(defmethod element-p ((elt fxml.dom:element))
  elt)

(defmethod get-attribute ((elt fxml.dom:element) attrib)
  ;; TODO: special case namespace-uri
  (fxml.dom:get-attribute elt attrib))

(defmethod parent-node ((n fxml.dom:node))
  (fxml.dom:parent-node n))

(defmethod previous-sibling ((n fxml.dom:element))
  "gets the parent fxml.dom:element (rather than ever returning the document node)"
  (element-p (fxml.dom:previous-sibling n)))

(defmethod child-elements ((n fxml.dom:node))
  (let ((acc '()))
    (fxml.dom:do-node-list (kid (fxml.dom:child-nodes n) (nreverse acc))
      (when (element-p kid)
        (push kid acc)))))

(defmethod child-nodes ((n fxml.dom:node))
  (let ((acc '()))
    (fxml.dom:do-node-list (kid (fxml.dom:child-nodes n) (nreverse acc))
      (push kid acc))))

(defmethod document-of ((n fxml.dom:node))
  (fxml.dom:owner-document n))

(defmethod %do-query (matcher (elt fxml.dom:node) &key (first? nil))
  (labels ((walk (node fn)
             (funcall fn node)
             (fxml.dom:do-node-list (node (fxml.dom:child-nodes node))
               (walk node fn))))
    (let ((acc '()))
      (walk
       elt
       (lambda (n)
         (when (and (element-p n) (funcall matcher n))
           (if first?
               (return-from %do-query n)
               (push n acc)))))
      (nreverse acc))))

