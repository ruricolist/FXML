(in-package #:css-selectors)

(defmethod tag-name ((elt fxml.stp:element))
  (fxml.stp:local-name elt))

(defmethod get-attribute ((elt fxml.stp:element) attr)
  ;; TODO: special case namespace-uri
  (fxml.stp:attribute-value elt (string-downcase attr)))

(defmethod element-p ((elt fxml.stp:element))
  elt)

(defmethod parent-node ((n fxml.stp:node))
  "gets the parent node"
  (fxml.stp:parent n))

(defmethod previous-sibling ((n fxml.stp:element))
  "gets the parent dom:element (rather than ever returning the document node)"
  (unless (eq n (fxml.stp:first-child (fxml.stp:parent n)))
    (element-p (fxml.stp:previous-sibling n))))

(defmethod child-elements ((n fxml.stp:node))
  (iter (for kid in (fxml.stp:list-children n))
    (when (element-p kid) (collect kid))))

(defmethod child-nodes ((n fxml.stp:node))
  (fxml.stp:list-children n))

(defmethod document-of ((n fxml.stp:node))
  (fxml.stp:document n))

(defun stp-do-query/fxml (matcher elt first?)
  (if first?
      (fxml.stp:find-recursively-if
       (lambda (node)
         (when (and (element-p node) (funcall matcher node))
           node))
       elt)
      (let ((matches '()))
        (fxml.stp:do-recursively (node elt)
          (when (and (element-p node) (funcall matcher node))
            (push node matches)))
        (nreverse matches))))

(defmethod %do-query (matcher (elt fxml.stp:element) &key (first? nil))
  (stp-do-query/fxml matcher elt first?))

(defmethod %do-query (matcher (elt fxml.stp:document) &key (first? nil))
  (stp-do-query/fxml matcher elt first?))
