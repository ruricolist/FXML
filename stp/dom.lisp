(in-package #:fxml.stp)

;;; Allow read-only methods from DOM and STP to work on both.

(defmacro equivalents (&rest pairs)
  `(progn
     ,@(loop for (dom stp . nil) on pairs by #'cddr
             collect `(progn
                        (defmethod ,stp ((node fxml.dom:node))
                          (,dom node))
                        (defmethod ,dom ((node node))
                          (,stp node))))))

;;; Cf. DOM-COMPARISON.

(equivalents
 fxml.dom:local-name local-name
 fxml.dom:tag-name qualified-name
 fxml.dom:namespace-uri namespace-uri
 fxml.dom:prefix namespace-prefix
 fxml.dom:previous-sibling previous-sibling
 fxml.dom:next-sibling next-sibling
 fxml.dom:first-child first-child
 fxml.dom:last-child last-child
 fxml.dom:data data
 fxml.dom:value value
 fxml.dom:parent-node parent
 fxml.dom:owner-document document
 fxml.dom:system-id system-id
 fxml.dom:public-id public-id
 fxml.dom:internal-subset internal-subset)

(defmethod fxml.dom:get-attribute ((el element) attr)
  (attribute-value el attr))

(defmethod fxml.dom:get-attribute-ns ((el element) attr ns)
  (attribute-value el attr ns))

(defmethod attribute-value ((el fxml.dom:element) attr &optional ns)
  (if ns
      (fxml.dom:get-attribute-ns el attr ns)
      (fxml.dom:get-attribute el attr)))

(defmethod map-children (result-type fn (node fxml.dom:node))
  (if (null result-type)
      (dom:map-node-map fn (dom:child-nodes node))
      (let ((results '()))
        (dom:map-node-map
         (lambda (item)
           (push (funcall fn item) results))
         (dom:child-nodes node))
        
        (if (subtypep result-type 'list)
            (nreverse results)
            (replace (make-sequence result-type (length results))
                     (nreverse results))))))

(defmethod map-recursively (fn (node fxml.dom:node))
  (funcall fn node)
  (map-children nil
                (lambda (child)
                  (map-recursively fn child))
                node))

(defmethod fxml.dom:node-type ((self document)) :document)
(defmethod fxml.dom:node-type ((self document-fragment)) :document-fragment)
(defmethod fxml.dom:node-type ((self text)) :text)
(defmethod fxml.dom:node-type ((self comment)) :comment)
(defmethod fxml.dom:node-type ((self attribute)) :attribute)
(defmethod fxml.dom:node-type ((self element)) :element)
(defmethod fxml.dom:node-type ((self document-type)) :document-type)
(defmethod fxml.dom:node-type ((self processing-instruction)) :processing-instruction)


