;;; XXX this DOM builder knows too much about the specifics of the DOM
;;; implementation for my taste.  While document creation is not specified
;;; by the DOM Level 1 spec, we shouldn't really be manually setting slots
;;; in other nodes IMHO.
;;;
;;; As a follow-up to that, the children list is created in the wrong order
;;; and then reversed.  Is it really worth the improved speed to do this?
;;; Calling APPEND-NODE would be portable.
;;;
;;; In particular, that design choice has lead to other bugs, for example the
;;; PARENT slot has to be set manually, too.  A DOM test finally showed
;;; that this had been forgotten for Text nodes and PIs.
;;;
;;; Opinions?
;;;
;;;   -- David

;;; Now at least the children list isn't reversed anymore, because I changed
;;; the representation to be an extensible vector.  Still its not clear to
;;; me whether the DOM Builder should be affected by such changes at all.
;;;
;;;   -- David

(in-package :dom-impl)

(defclass dom-builder ()
  ((document      :initform nil :accessor document)
   (element-stack :initform '() :accessor element-stack)))

(defun dom:make-dom-builder ()
  (make-instance 'dom-builder))

(defun fast-push (new-element vector)
  (vector-push-extend new-element vector (max 1 (array-dimension vector 0))))

(defmethod sax:start-document ((handler dom-builder))
  (let ((document (make-instance 'dom-impl::document)))
    (setf (slot-value document 'dom-impl::owner) nil
	  (slot-value document 'dom-impl::doc-type) nil)
    (setf (document handler) document)
    (push document (element-stack handler))))

(defmethod sax:end-document ((handler dom-builder))
  (setf (slot-value (document handler) 'entities) xml::*entities*)
  (let ((doctype (dom:doctype (document handler))))
    (when doctype
      (setf (slot-value (dom:entities doctype) 'read-only-p) t)
      (setf (slot-value (dom:notations doctype) 'read-only-p) t)))
  (document handler))

(defmethod sax:start-dtd ((handler dom-builder) name publicid systemid)
  (declare (ignore publicid systemid))
  (let* ((document (document handler))
         (doctype (make-instance 'dom-impl::document-type
                    :name name
                    :notations (make-instance 'dom-impl::named-node-map
                                 :element-type :notation
                                 :owner document)
                    :entities (make-instance 'dom-impl::named-node-map
                                :element-type :entity
                                :owner document))))
    (setf (slot-value doctype 'dom-impl::owner) document
	  (slot-value document 'dom-impl::doc-type) doctype)))

(defmethod sax:start-element ((handler dom-builder) namespace-uri local-name qname attributes)
  (with-slots (document element-stack) handler
    (let ((element (dom:create-element document qname))
	  (parent (car element-stack)))
      (dolist (attr attributes)
	(dom:set-attribute element (xml::attribute-qname attr) (xml::attribute-value attr)))
      (setf (slot-value element 'dom-impl::parent) parent)
      (fast-push element (slot-value parent 'dom-impl::children))
      (push element element-stack))))

(defmethod sax:end-element ((handler dom-builder) namespace-uri local-name qname)
  (pop (element-stack handler)))

(defmethod sax:characters ((handler dom-builder) data)
  (with-slots (document element-stack) handler
    (let* ((parent (car element-stack))
           (last-child (dom:last-child parent)))
      (cond
        ((eq (dom:node-type parent) :cdata-section)
          (setf (dom:data parent) data))
        ((and last-child (eq (dom:node-type last-child) :text))
          ;; um entities herum wird SAX:CHARACTERS mehrfach aufgerufen fuer
          ;; den gleichen Textknoten.  Hier muessen wir den bestehenden Knoten
          ;; erweitern, sonst ist das Dokument nicht normalisiert.
          ;; (XXX Oder sollte man besser den Parser entsprechend aendern?)
          (dom:append-data last-child data))
        (t
          (let ((node (dom:create-text-node document data)))
            (setf (slot-value node 'dom-impl::parent) parent)
            (fast-push node (slot-value (car element-stack) 'dom-impl::children))))))))

(defmethod sax:start-cdata ((handler dom-builder))
  (with-slots (document element-stack) handler
    (let ((node (dom:create-cdata-section document #""))
          (parent (car element-stack)))
      (setf (slot-value node 'dom-impl::parent) parent)
      (fast-push node (slot-value parent 'dom-impl::children))
      (push node element-stack))))

(defmethod sax:end-cdata ((handler dom-builder))
  (let ((node (pop (slot-value handler 'element-stack))))
    (assert (eq (dom:node-type node) :cdata-section))))

(defmethod sax:processing-instruction ((handler dom-builder) target data)
  (with-slots (document element-stack) handler
    (let ((node (dom:create-processing-instruction document target data))
          (parent (car element-stack)))
      (setf (slot-value node 'dom-impl::parent) parent)
      (fast-push node (slot-value (car element-stack) 'dom-impl::children)))))

(defmethod sax:comment ((handler dom-builder) data)
  (with-slots (document element-stack) handler
    (let ((node (dom:create-comment document data))
          (parent (car element-stack)))
      (setf (slot-value node 'dom-impl::parent) parent)
      (fast-push node (slot-value (car element-stack) 'dom-impl::children)))))

(defmethod sax:unparsed-entity-declaration
    ((handler dom-builder) name public-id system-id notation-name)
  (dom:set-named-item (dom:entities (dom:doctype (document handler)))
                      (make-instance 'dom-impl::entity
                        :owner (document handler)
                        :name name
                        :public-id public-id
                        :system-id system-id
                        :notation-name notation-name)))

(defmethod sax:notation-declaration
    ((handler dom-builder) name public-id system-id)
  (dom:set-named-item (dom:notations (dom:doctype (document handler)))
                      (make-instance 'dom-impl::notation
                        :owner (document handler)
                        :name name
                        :public-id public-id
                        :system-id system-id)))
