(in-package :dom-impl)

(defun dom:map-document
    (handler document
     &key (include-xmlns-attributes sax:*include-xmlns-attributes*)
          include-default-values)
  (sax:start-document handler)
  (let ((doctype (dom:doctype document)))
    (when doctype
      (sax:start-dtd handler (dom:name doctype) nil nil)
      (let ((ns (dom:notations doctype)))
        (dotimes (k (dom:length ns))
          (let ((n (dom:item ns k)))
            (sax:notation-declaration handler
                                      (dom:name n)
                                      (dom:public-id n)
                                      (dom:system-id n)))
          ;; fixme: entities!
          )
        (sax:end-dtd handler))))
  (labels ((walk (node)
             (dom:do-node-list (child (dom:child-nodes node))
               (ecase (dom:node-type child)
                 (:element
                   ;; fixme: namespaces
                   (let ((attlist
                          (compute-attributes child
                                              include-xmlns-attributes
                                              include-default-values))
                         (lname (dom:tag-name child))
                         (qname (dom:tag-name child)))
                     (sax:start-element handler nil lname qname attlist)
                     (walk child)
                     (sax:end-element handler nil lname qname)))
                 (:cdata-section
                   (sax:start-cdata handler)
                   (sax:characters handler (dom:data child))
                   (sax:end-cdata handler))
                 (:text
                   (sax:characters handler (dom:data child)))
                 (:comment
                   (sax:comment handler (dom:data child)))
                 (:processing-instruction
                   (sax:processing-instruction handler
                                               (dom:target child)
                                               (dom:data child)))))))
    (walk document))
  (sax:end-document handler))

(defun compute-attributes (element xmlnsp defaultp)
  (let ((results '()))
    (dom:do-node-list (a (dom:attributes element))
      (when (and (or defaultp (dom:specified a))
                 (or xmlnsp (not (cxml::xmlns-attr-p (dom:name a)))))
        (push
         (cxml::make-attribute :qname (dom:name a)
                               :value (dom:value a)
                               :specified-p (dom:specified a))
         results)))
    (reverse results)))
