(defpackage :dom-impl
  (:use :glisp))

(in-package :dom-impl)

;; Classes

(defclass node ()
  ((parent      :initarg :parent        :initform nil)
   (children    :initarg :children      :initform nil)
   (owner       :initarg :owner         :initform nil)))

(defclass document (node)
  ((doc-type    :initarg :doc-type     :reader dom:doctype)))

(defclass document-fragment (node)
  ())

(defclass character-data (node)
  ((data        :initarg :data          :reader dom:data)))

(defclass attribute (node)
  ((name        :initarg :name          :reader dom:name)
   (value       :initarg :value         :reader dom:value)
   (specified-p :initarg :specified-p   :reader dom:specified)))

(defclass element (node)
  ((tag-name    :initarg :tag-name      :reader dom:tag-name)
   (attributes  :initarg :attributes    :reader dom:attributes
                :initform (make-instance 'named-node-map))))

(defclass text (character-data)
  ())

(defclass comment (character-data)
  ())

(defclass cdata-section (text)
  ())

(defclass document-type (node)
  ((name          :initarg :name          :reader dom:name)
   (entities      :initarg :entities      :reader dom:entities)
   (notations     :initarg :notations     :reader dom:notations)))

(defclass notation (node)
  ((name          :initarg :name          :reader dom:name)
   (public-id     :initarg :public-id     :reader dom:public-id)
   (system-id     :initarg :system-id     :reader dom:system-id)))

(defclass entity (node)
  ((name          :initarg :name          :reader dom:name)
   (public-id     :initarg :public-id     :reader dom:public-id)
   (system-id     :initarg :system-id     :reader dom:system-id)
   (notation-name :initarg :notation-name :reader dom:notation-name)))

(defclass entity-reference (node) 
  ((name          :initarg :name          :reader dom:name)))

(defclass processing-instruction (node)
  ((target        :initarg :target        :reader dom:target)
   (data          :initarg :data          :reader dom:data)))

(defclass named-node-map ()
  ((items         :initarg :items         :reader dom:items
                  :initform nil) ))


;;; Implementation

;; document-fragment protocol
;; document protocol

(defmethod dom:implementation ((document document))
  'implementation)

(defmethod dom:document-element ((document document))
  (dolist (k (dom:child-nodes document))
    (cond ((typep k 'element)
           (return k)))))

(defmethod dom:create-element ((document document) tag-name)
  (setf tag-name (rod tag-name))
  (make-instance 'element 
    :tag-name tag-name
    :owner document))

(defmethod dom:create-document-fragment ((document document))
  (make-instance 'document-fragment
    :owner document))

(defmethod dom:create-text-node ((document document) data)
  (setf data (rod data))
  (make-instance 'text
    :data data
    :owner document))

(defmethod dom:create-comment ((document document) data)
  (setf data (rod data))
  (make-instance 'comment
    :data data
    :owner document))

(defmethod dom:create-cdata-section ((document document) data)
  (setf data (rod data))
  (make-instance 'cdata-section
    :data data
    :owner document))

(defmethod dom:create-processing-instruction ((document document) target data)
  (setf target (rod target))
  (setf data (rod data))
  (make-instance 'processing-instruction
    :owner document
    :target target
    :data data))

(defmethod dom:create-attribute ((document document) name)
  (setf name (rod name))
  (make-instance 'attribute
    :name name
    :specified-p nil                    ;???
    :owner document))

(defmethod dom:create-entity-reference ((document document) name)
  (setf name (rod name))
  (make-instance 'entity-reference
    :name name
    :owner document))

(defmethod dom:get-elements-by-tag-name ((document document) tag-name)
  (setf tag-name (rod tag-name))
  (let ((result nil))
    (setf tag-name (rod tag-name))
    (let ((wild-p (rod= tag-name '#.(string-rod "*"))))
      (labels ((walk (n)
                 (when (and (dom:element-p n)
                            (or wild-p (tag-name-eq tag-name (dom:node-name n))))
                   (push n result))
                 (mapc #'walk (dom:child-nodes n))))
        (walk document)
        (reverse result)))))

;;; Node

(defmethod dom:parent-node ((node node))
  (slot-value node 'parent))

(defmethod dom:child-nodes ((node node))
  (slot-value node 'children))

(defmethod dom:first-child ((node node))
  (car (slot-value node 'children)))

(defmethod dom:last-child ((node node))
  (car (last (slot-value node 'children))))

(defmethod dom:previous-sibling ((node node))
  (with-slots (parent) node
    (when parent
      (with-slots (children) parent
        (do ((q children (cdr q)))
            ((null (cdr q)) niL)
          (cond ((eq (cadr q) node)
                 (return (car q)))))))))

(defmethod dom:next-sibling ((node node))
  (with-slots (parent) node
    (when parent
      (with-slots (children) parent
        (do ((q children (cdr q)))
            ((null (cdr q)) niL)
          (cond ((eq (car q) node)
                 (return (cadr q)))))))))

(defmethod dom:owner-document ((node node))
  (slot-value node 'owner))

(defun ensure-valid-insertion-request (node new-child)
  (unless (can-adopt-p node new-child)
    ;; HIERARCHY_REQUEST_ERR
    (error "~S cannot adopt ~S." node new-child))
  (unless (eq (dom:owner-document node) 
              (dom:owner-document new-child))
    ;; WRONG_DOCUMENT_ERR
    (error "~S cannot adopt ~S, since it was created by a different document."
           node new-child))
  (with-slots (children) node
    (unless (null (slot-value new-child 'parent))
      (cond ((eq (slot-value new-child 'parent)
                 node)
             ;; remove it first
             (setf children (delete new-child children)))
            (t
             ;; otherwise it is an error.
             ;; GB_INTEGRITY_ERR
             (error "~S is already adopted." new-child)))) ))

(defmethod dom:insert-before ((node node) (new-child node) (ref-child t))
  (ensure-valid-insertion-request node new-child)
  (with-slots (children) node
    (cond ((eq (car children) ref-child)
           (setf (slot-value new-child 'parent) node)
           (setf children (cons new-child children)))
          (t
           (do ((q children (cdr q)))
               ((null (cdr q))
                (cond ((null ref-child)
                       (setf (slot-value new-child 'parent) node)
                       (setf (cdr q) (cons new-child nil)))
                      (t
                       ;; NOT_FOUND_ERR
                       (error "~S is no child of ~S." ref-child node))))
             (cond ((eq (cadr q) ref-child)
                    (setf (slot-value new-child 'parent) node)
                    (setf (cdr q) (cons new-child (cdr q)))
                    (return))))))
    new-child))

(defmethod dom:insert-before ((node node) (fragment document-fragment) ref-child)
  (dolist (child (dom:child-nodes fragment))
    (dom:insert-before node child ref-child))
  fragment)

(defmethod dom:replace-child ((node node) (new-child node) (old-child node))
  (ensure-valid-insertion-request node new-child)
  (with-slots (children) node
    (do ((q children (cdr q)))
        ((null q)
         ;; NOT_FOUND_ERR
         (error "~S is no child of ~S." old-child node))
      (cond ((eq (car q) old-child)
             (setf (car q) new-child)
             (setf (slot-value new-child 'parent) node)
             (setf (slot-value old-child 'parent) nil)
             (return))))
    old-child))

(defmethod dom:append-child ((node node) (new-child node))
  (ensure-valid-insertion-request node new-child)
  (with-slots (children) node
    (setf children (nconc children (list new-child)))
    (setf (slot-value new-child 'parent) node)
    new-child))

(defmethod dom:has-child-nodes ((node node))
  (not (null (slot-value node 'children))))

(defmethod dom:append-child ((node node) (new-child document-fragment))
  (dolist (child (dom:child-nodes new-child))
    (dom:append-child node child))
  new-child)

;; was auf node noch implemetiert werden muss:
;; - node-type
;; - can-adopt-p
;; - ggf attributes 
;; - node-name
;; - node-value

;; node-name 

(defmethod dom:node-name ((self document))
  '#.(string-rod "#document"))

(defmethod dom:node-name ((self document-fragment))
  '#.(string-rod "#document-fragment"))

(defmethod dom:node-name ((self text))
  '#.(string-rod "#text"))

(defmethod dom:node-name ((self cdata-section))
  '#.(string-rod "#cdata-section"))

(defmethod dom:node-name ((self comment))
  '#.(string-rod "#comment"))

(defmethod dom:node-name ((self attribute))
  (dom:name self))

(defmethod dom:node-name ((self element))
  (dom:tag-name self))

(defmethod dom:node-name ((self document-type))
  (dom:name self))

(defmethod dom:node-name ((self notation))
  (dom:name self))

(defmethod dom:node-name ((self entity))
  (dom:name self))

(defmethod dom:node-name ((self entity-reference))
  (dom:name self))

(defmethod dom:node-name ((self processing-instruction))
  (dom:target self))

;; node-type

(defmethod dom:node-type ((self document)) :document)
(defmethod dom:node-type ((self document-fragment)) :document-fragment)
(defmethod dom:node-type ((self text)) :text)
(defmethod dom:node-type ((self comment)) :comment)
(defmethod dom:node-type ((self cdata-section)) :cdata-section)
(defmethod dom:node-type ((self attribute)) :attribute)
(defmethod dom:node-type ((self element)) :element)
(defmethod dom:node-type ((self document-type)) :document-type)
(defmethod dom:node-type ((self notation)) :notation)
(defmethod dom:node-type ((self entity)) :entity)
(defmethod dom:node-type ((self entity-reference)) :entity-reference)
(defmethod dom:node-type ((self processing-instruction)) :processing-instruction)

;; node-value

(defmethod dom:node-value ((self document)) nil)
(defmethod dom:node-value ((self document-fragment)) nil)
(defmethod dom:node-value ((self character-data)) (dom:data self))
(defmethod dom:node-value ((self attribute)) (dom:name self))
(defmethod dom:node-value ((self element)) nil)
(defmethod dom:node-value ((self document-type)) nil)
(defmethod dom:node-value ((self notation)) nil)
(defmethod dom:node-value ((self entity)) nil)
(defmethod dom:node-value ((self entity-reference)) nil)
(defmethod dom:node-value ((self processing-instruction)) (dom:data self))

;; attributes

;; (gibt es nur auf element)

(defmethod dom:attributes ((self node))
  nil)

;; dann fehlt noch can-adopt und attribute conventions fuer adoption

;;; NAMED-NODE-MAP

(defmethod dom:get-named-item ((self named-node-map) name)
  (setf name (rod name))
  (with-slots (items) self
    (dolist (k items nil)
      (cond ((rod= name (dom:node-name k))
             (return k))))))

(defmethod dom:set-named-item ((self named-node-map) arg)
  (let ((name (dom:node-name arg)))
    (with-slots (items) self
      (dolist (k items (progn (setf items (cons arg items))nil))
        (cond ((rod= name (dom:node-name k))
               (setf items (cons arg (delete k items)))
               (return k)))))))

(defmethod dom:remove-named-item ((self named-node-map) name)
  (setf name (rod name))
  (with-slots (items) self
    (dolist (k items nil)
      (cond ((rod= name (dom:node-name k))
             (setf items (delete k items))
             (return k))))))

(defmethod dom:length ((self named-node-map))
  (with-slots (items) self
    (length items)))

(defmethod dom:item ((self named-node-map) index)
  (with-slots (items) self
    (elt items index)))

;;; CHARACTER-DATA

(defmethod dom:length ((node character-data))
  (length (slot-value node 'value)))

(defmethod dom:substring-data ((node character-data) offset count)
  (subseq (slot-value node 'value) offset (+ offset count)))

(defmethod dom:append-data ((node character-data) arg)
  (setq arg (rod arg))
  (with-slots (value) node
    (setf value (concatenate (type-of value) value arg)))
  (values))

(defmethod dom:delete-data ((node character-data) offset count)
  (with-slots (value) node
    (let ((new (make-array (- (length value) count) :element-type (type-of value))))
      (replace new value 
               :start1 0 :end1 offset
               :start2 0 :end2 offset)
      (replace new value 
               :start1 offset :end1 (length new)
               :start2 (+ offset count) :end2 (length value))
      (setf value new)))
  (values))

(defmethod dom:replace-data ((node character-data) offset count arg)
  (setf arg (rod arg))
  (with-slots (value) node
    (replace value arg
             :start1 offset :end1 (+ offset count)
             :start2 0 :end2 count))
  (values))

;;; ATTR

;; hmm... value muss noch entities lesen und text-nodes in die hierarchie hängen.

(defmethod (setf dom:value) (new-value (node attribute))
  (setf (slot-value node 'value) (rod new-value)))

;;; ELEMENT

(defmethod dom:get-attribute-node ((element element) name)
  (dom:get-named-item (dom:attributes element) name))

(defmethod dom:set-attribute-node ((element element) (new-attr attribute))
  (dom:set-named-item (dom:attributes element) new-attr))

(defmethod dom:get-attribute ((element element) name)
  (let ((a (dom:get-attribute-node element name)))
    (if a
        (dom:value a)
      nil)))

(defmethod dom:set-attribute ((element element) name value)
  (with-slots (owner) element
    (dom:set-attribute-node 
     element (make-instance 'attribute 
               :owner owner
               :name name
               :value value
               :specified-p t))
    (values)))

(defmethod dom:remove-attribute-node ((element element) (old-attr attribute))
  (let ((res (dom:remove-named-item element (dom:name old-attr))))
    (if res
        res
      ;; NOT_FOUND_ERR
      (error "Attribute not found."))))

(defmethod dom:get-elements-by-tag-name ((element element) name)
  name
  (error "Not implemented."))

(defmethod dom:normalize ((element element))
  (error "Not implemented.") )

;;; TEXT

(defmethod dom:split-text ((text text) offset)
  offset
  (error "Not implemented."))

;;; COMMENT -- nix
;;; CDATA-SECTION -- nix

;;; DOCUMENT-TYPE -- missing
;;; NOTATION -- nix
;;; ENTITY -- nix
;;; ENTITY-REFERENCE -- nix
;;; PROCESSING-INSTRUCTION -- nix

;; Notbehelf!
(defun can-adopt-p (x y) x y t)


;;; predicates

(defmethod dom:node-p ((object node)) t)
(defmethod dom:node-p ((object t)) nil)

(defmethod dom:document-p ((object document)) t)
(defmethod dom:document-p ((object t)) nil)

(defmethod dom:document-fragment-p ((object document-fragment)) t)
(defmethod dom:document-fragment-p ((object t)) nil)

(defmethod dom:character-data-p ((object character-data)) t)
(defmethod dom:character-data-p ((object t)) nil)

(defmethod dom:attribute-p ((object attribute)) t)
(defmethod dom:attribute-p ((object t)) nil)

(defmethod dom:element-p ((object element)) t)
(defmethod dom:element-p ((object t)) nil)

(defmethod dom:text-node-p ((object text)) t)
(defmethod dom:text-node-p ((object t)) nil)

(defmethod dom:comment-p ((object comment)) t)
(defmethod dom:comment-p ((object t)) nil)

(defmethod dom:cdata-section-p ((object cdata-section)) t)
(defmethod dom:cdata-section-p ((object t)) nil)

(defmethod dom:document-type-p ((object document-type)) t)
(defmethod dom:document-type-p ((object t)) nil)

(defmethod dom:notation-p ((object notation)) t)
(defmethod dom:notation-p ((object t)) nil)

(defmethod dom:entity-p ((object entity)) t)
(defmethod dom:entity-p ((object t)) nil)

(defmethod dom:entity-reference-p ((object entity-reference)) t)
(defmethod dom:entity-reference-p ((object t)) nil)

(defmethod dom:processing-instruction-p ((object processing-instruction)) t)
(defmethod dom:processing-instruction-p ((object t)) nil)

(defmethod dom:named-node-map-p ((object named-node-map)) t)
(defmethod dom:named-node-map-p ((object t)) nil)
