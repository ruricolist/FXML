(in-package :xml)

;;; Implementation of a simple but faster DOM.

(defclass simple-document () 
  ((children :initform nil :accessor simple-document-children)))

(defstruct node 
  parent)

(defstruct (processing-instruction (:include node))
  target
  data)

(defstruct (text (:include node)
                 (:constructor make-text-boa (parent data)))
  data)

(defstruct (element (:include node))
  gi
  attributes
  children)

(defmethod dom:create-processing-instruction ((document simple-document) target data)
  (make-processing-instruction :target target :data data))

(defmethod dom:append-child ((node element) child)
  (setf (node-parent child) node)
  (push child (element-children node)))

(defmethod dom:append-child ((node simple-document) child)
  (push child (simple-document-children node))
  nil)

(defmethod dom:create-element ((document simple-document) name)
  (make-element :gi name))

(defmethod dom:set-attribute ((node element) name value)
  (push (cons name value)
        (element-attributes node)))

(defmethod dom:create-text-node ((document simple-document) data)
  (make-text-boa nil data))

(defmethod dom:create-cdata-section ((document simple-document) data)
  (make-text-boa nil data))
