(in-package :dom-impl)

(export 'dom-builder)

(defclass dom-builder ()
  ((document      :initform nil :accessor document)
   (element-stack :initform '() :accessor element-stack)))

(defmethod sax:start-document ((handler dom-builder))
  (let ((document (make-instance 'dom-impl::document))
	(doctype (make-instance 'dom-impl::document-type
				:notations (make-hash-table :test #'equalp))))
    (setf (slot-value document 'dom-impl::owner) document
	  (slot-value document 'dom-impl::doc-type) doctype)
    (setf (document handler) document)
    (push document (element-stack handler))))

(defmethod sax:end-document ((handler dom-builder))
  (setf (slot-value (document handler) 'children )
	(nreverse (slot-value (document handler) 'children)))
  (document handler))

(defmethod sax:start-element ((handler dom-builder) namespace-uri local-name qname attributes)
  (with-slots (document element-stack) handler
    (let ((element (dom:create-element document qname))
	  (parent (car element-stack)))
      (dolist (attr attributes)
	(dom:set-attribute element (xml::attribute-qname attr) (xml::attribute-value attr)))
      (setf (slot-value element 'dom-impl::parent) parent)
      (push element (slot-value parent 'dom-impl::children))
      (push element element-stack))))

(defmethod sax:end-element ((handler dom-builder) namespace-uri local-name qname)
  (let ((element (pop (element-stack  handler))))
    (setf (slot-value element 'dom-impl::children)
	  (nreverse (slot-value element 'dom-impl::children)))))

(defmethod sax:characters ((handler dom-builder) data)
  (with-slots (document element-stack) handler
    (let ((node (dom:create-text-node document data)))
      (push node (slot-value (car element-stack) 'dom-impl::children)))))

(defmethod sax:processing-instruction ((handler dom-builder) target data)
  (with-slots (document element-stack) handler
    (let ((node (dom:create-processing-instruction document target data)))
      (push node (slot-value (car element-stack) 'dom-impl::children)))))
