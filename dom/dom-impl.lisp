(defpackage :dom-impl
  (:use :cl :runes))

(in-package :dom-impl)

;; Classes

(define-condition dom-exception (error)
  ((key       :initarg :key       :reader dom-exception-key)
   (string    :initarg :string    :reader dom-exception-string)
   (arguments :initarg :arguments :reader dom-exception-arguments))
  (:report
   (lambda (c s)
     (format s "~A (~D):~%~?"
             (dom-exception-key c)
             (dom:code c)
             (dom-exception-string c)
             (dom-exception-arguments c)))))

(defclass node ()
  ((parent      :initarg :parent        :initform nil)
   (children    :initarg :children      :initform (make-node-list))
   (owner       :initarg :owner         :initform nil)
   (read-only-p :initform nil           :reader read-only-p)
   (map         :initform nil)))

(defclass document (node)
  ((doc-type    :initarg :doc-type     :reader dom:doctype)
   (entities    :initform nil          :reader entities)))

(defclass document-fragment (node)
  ())

(defclass character-data (node)
  ((value       :initarg :data          :reader dom:data)))

(defclass attribute (node)
  ((name        :initarg :name          :reader dom:name)
   (specified-p :initarg :specified-p   :reader dom:specified)))

(defmethod print-object ((object attribute) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A=~S"
            (rod-string (dom:name object))
            (rod-string (dom:value object)))))

(defclass element (node)
  ((tag-name    :initarg :tag-name      :reader dom:tag-name)
   (attributes  :initarg :attributes    :reader dom:attributes)))

(defmethod print-object ((object element) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (rod-string (dom:tag-name object)) stream)))

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
                  :initform nil)
   (owner         :initarg :owner         :reader dom:owner-document)
   (read-only-p   :initform nil           :reader read-only-p)
   (element-type  :initarg :element-type)))


;;; Implementation

(defun assert-writeable (node)
  (when (read-only-p node)
    (dom-error :NO_MODIFICATION_ALLOWED_ERR "~S is marked read-only." node)))

(defun dom:map-node-list (fn nodelist)
  (dotimes (i (dom:length nodelist))
    (funcall fn (dom:item nodelist i))))

(defmacro dom:do-node-list ((var nodelist &optional resultform) &body body)
  `(block nil
     (dom:map-node-list (lambda (,var) ,@body) ,nodelist)
     ,resultform))

(defmacro dovector ((var vector &optional resultform) &body body)
  `(loop
       for ,var across ,vector do (progn ,@body)
       ,@(when resultform `(finally (return ,resultform)))))

(defun move (from to from-start to-start length)
  ;; like (setf (subseq to to-start (+ to-start length))
  ;;            (subseq from from-start (+ from-start length)))
  ;; but without creating the garbage
  (if (< to-start from-start)
      (loop
          repeat length
          for i from from-start
          for j from to-start
          do (setf (elt to j) (elt from i)))
      (loop
          repeat length
          for i from (+ from-start length -1) by -1
          for j from (+ to-start length -1) by -1
          do (setf (elt to j) (elt from i)))))

(defun adjust-vector-exponentially (vector new-dimension set-fill-pointer-p)
  (let ((d (array-dimension vector 0)))
    (when (< d new-dimension)
      (loop
	  do (setf d (* 2 d))
	  while (< d new-dimension))
      (adjust-array vector d))
    (when set-fill-pointer-p
      (setf (fill-pointer vector) new-dimension))))

(defun make-space (vector &optional (n 1))
  (adjust-vector-exponentially vector (+ (length vector) n) nil))

(defun extension (vector)
  (max (array-dimension vector 0) 1))

;; dom-exception

(defun dom-error (key fmt &rest args)
  (error 'dom-exception :key key :string fmt :arguments args))

(defmethod dom:code ((self dom-exception))
  (ecase (dom-exception-key self)
    (:INDEX_SIZE_ERR                    1)
    (:DOMSTRING_SIZE_ERR                2)
    (:HIERARCHY_REQUEST_ERR             3)
    (:WRONG_DOCUMENT_ERR                4)
    (:INVALID_CHARACTER_ERR             5)
    (:NO_DATA_ALLOWED_ERR               6)
    (:NO_MODIFICATION_ALLOWED_ERR       7)
    (:NOT_FOUND_ERR                     8)
    (:NOT_SUPPORTED_ERR                 9)
    (:INUSE_ATTRIBUTE_ERR               10)))

;; document-fragment protocol
;; document protocol

(defmethod dom:implementation ((document document))
  'implementation)

(defmethod dom:document-element ((document document))
  (dovector (k (dom:child-nodes document))
    (cond ((typep k 'element)
           (return k)))))

(defmethod dom:create-element ((document document) tag-name)
  (setf tag-name (rod tag-name))
  (unless (xml::valid-name-p tag-name)
    (dom-error :INVALID_CHARACTER_ERR "not a name: ~A" (rod-string tag-name)))
  (make-instance 'element 
    :tag-name tag-name
    :owner document
    :attributes (make-instance 'named-node-map
                  :element-type :attribute
                  :owner document)))

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
  (unless (xml::valid-name-p target)
    (dom-error :INVALID_CHARACTER_ERR "not a name: ~A" (rod-string target)))
  (make-instance 'processing-instruction
    :owner document
    :target target
    :data data))

(defmethod dom:create-attribute ((document document) name)
  (setf name (rod name))
  (unless (xml::valid-name-p name)
    (dom-error :INVALID_CHARACTER_ERR "not a name: ~A" (rod-string name)))
  (make-instance 'attribute
    :name name
    :specified-p t
    :owner document))

(defmethod dom:create-entity-reference ((document document) name)
  (setf name (rod name))
  (unless (xml::valid-name-p name)
    (dom-error :INVALID_CHARACTER_ERR "not a name: ~A" (rod-string name)))
  (make-instance 'entity-reference
    :name name
    :owner document))

(defmethod get-elements-by-tag-name-internal (node tag-name)
  (setf tag-name (rod tag-name))
  (let ((result (make-node-list)))
    (setf tag-name (rod tag-name))
    (let ((wild-p (rod= tag-name '#.(string-rod "*"))))
      (labels ((walk (n)
                 (dovector (c (dom:child-nodes n))
                   (when (dom:element-p c)
                     (when (or wild-p (rod= tag-name (dom:node-name c)))
                       (vector-push-extend c result (extension result)))
                     (walk c)))))
        (walk node)))
    result))

(defmethod dom:get-elements-by-tag-name ((document document) tag-name)
  (get-elements-by-tag-name-internal document tag-name))

;;; Node

(defmethod dom:parent-node ((node node))
  (slot-value node 'parent))

(defmethod dom:child-nodes ((node node))
  (slot-value node 'children))

(defmethod dom:first-child ((node node))
  (dom:item (slot-value node 'children) 0))

(defmethod dom:last-child ((node node))
  (with-slots (children) node
    (if (plusp (length children))
        (elt children (1- (length children)))
        nil)))

(defmethod dom:previous-sibling ((node node))
  (with-slots (parent) node
    (when parent
      (with-slots (children) parent
        (let ((index (1- (position node children))))
          (if (eql index -1)
              nil
              (elt children index)))))))

(defmethod dom:next-sibling ((node node))
  (with-slots (parent) node
    (when parent
      (with-slots (children) parent
        (let ((index (1+ (position node children))))
          (if (eql index (length children))
              nil
              (elt children index)))))))

(defmethod dom:owner-document ((node node))
  (slot-value node 'owner))

(defun ensure-valid-insertion-request (node new-child)
  (assert-writeable node)
  (unless (can-adopt-p node new-child)
    (dom-error :HIERARCHY_REQUEST_ERR "~S cannot adopt ~S." node new-child))
  #+(or)                                ;XXX needs to be moved elsewhere
  (when (eq (dom:node-type node) :document)
    (let ((child-type (dom:node-type new-child)))
      (when (and (member child-type '(:element :document-type))
                 (find child-type (dom:child-nodes node) :key #'dom:node-type))
        (dom-error :HIERARCHY_REQUEST_ERR
                   "~S cannot adopt a second child of type ~S."
                   node child-type))))
  (unless (eq (if (eq (dom:node-type node) :document)
                  node
                  (dom:owner-document node))
              (dom:owner-document new-child))
    (dom-error :WRONG_DOCUMENT_ERR
               "~S cannot adopt ~S, since it was created by a different document."
               node new-child))
  (do ((n node (dom:parent-node n)))
      ((null n))
    (when (eq n new-child)
      (dom-error :HIERARCHY_REQUEST_ERR
                 "~S cannot adopt ~S, since that would create a cycle"
                 node new-child)))
  (unless (null (slot-value new-child 'parent))
    (dom:remove-child (slot-value new-child 'parent) new-child)))

(defmethod dom:insert-before ((node node) (new-child node) ref-child)
  (ensure-valid-insertion-request node new-child)
  (with-slots (children) node
    (if ref-child
        (let ((i (position ref-child children)))
          (unless i
            (dom-error :NOT_FOUND_ERR "~S is no child of ~S." ref-child node))
          (make-space children 1)
          (move children children i (1+ i) (- (length children) i))
          (incf (fill-pointer children))
          (setf (elt children i) new-child))
        (vector-push-extend new-child children (extension children)))
    (setf (slot-value new-child 'parent) node)
    new-child))

(defmethod dom:insert-before ((node node) (fragment document-fragment) ref-child)
  (dovector (child (dom:child-nodes fragment))
    (dom:insert-before node child ref-child))
  fragment)

(defmethod dom:replace-child ((node node) (new-child node) (old-child node))
  (ensure-valid-insertion-request node new-child)
  (with-slots (children) node
    (let ((i (position old-child children)))
      (unless i
        (dom-error :NOT_FOUND_ERR "~S is no child of ~S." old-child node))
      (setf (elt children i) new-child)) 
    (setf (slot-value new-child 'parent) node)
    (setf (slot-value old-child 'parent) nil)
    old-child))

(defmethod dom:replace-child
    ((node node) (new-child document-fragment) (old-child node))
  (dom:insert-before node new-child old-child)
  (dom:remove-child node old-child))

(defmethod dom:remove-child ((node node) (old-child node))
  (assert-writeable node)
  (with-slots (children) node
    (let ((i (position old-child children)))
      (unless i
        (dom-error :NOT_FOUND_ERR "~A not found in ~A" old-child node))
      (move children children (1+ i) i (- (length children) i 1))
      (decf (fill-pointer children)))
    (setf (slot-value old-child 'parent) nil)
    old-child))

(defmethod dom:append-child ((node node) (new-child node))
  (ensure-valid-insertion-request node new-child)
  (with-slots (children) node
    (vector-push-extend new-child children (extension children))
    (setf (slot-value new-child 'parent) node)
    new-child))

(defmethod dom:has-child-nodes ((node node))
  (plusp (length (slot-value node 'children))))

(defmethod dom:append-child ((node node) (new-child document-fragment))
  (assert-writeable node)
  (dovector (child (dom:child-nodes new-child))
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
(defmethod dom:node-value ((self attribute)) (dom:value self))
(defmethod dom:node-value ((self element)) nil)
(defmethod dom:node-value ((self document-type)) nil)
(defmethod dom:node-value ((self notation)) nil)
(defmethod dom:node-value ((self entity)) nil)
(defmethod dom:node-value ((self entity-reference)) nil)
(defmethod dom:node-value ((self processing-instruction)) (dom:data self))

;; (setf node-value), first the meaningful cases...

(defmethod (setf dom:node-value) (newval (self character-data))
  (assert-writeable self)
  (setf (dom:data self) newval))

(defmethod (setf dom:node-value) (newval (self attribute))
  (assert-writeable self)
  (setf (dom:value self) newval))

(defmethod (setf dom:node-value) (newval (self processing-instruction))
  (assert-writeable self)
  (setf (dom:data self) newval))

;; ... and (setf node-value), part II.  The DOM Level 1 spec fails to explain
;; this case, but it is covered by the (Level 1) test suite and clarified
;; in Level 2:
;;         nodeValue of type DOMString
;;                 The value of this node, depending on its type; see the
;;                 table above.  When it is defined to be null, setting
;;                 it has no effect.

(defmethod (setf dom:node-value) (newval (self element))
  (declare (ignore newval)))

(defmethod (setf dom:node-value) (newval (self entity-reference))
  (declare (ignore newval)))

(defmethod (setf dom:node-value) (newval (self entity))
  (declare (ignore newval)))

(defmethod (setf dom:node-value) (newval (self document))
  (declare (ignore newval)))

(defmethod (setf dom:node-value) (newval (self document-type))
  (declare (ignore newval)))

(defmethod (setf dom:node-value) (newval (self document-fragment))
  (declare (ignore newval)))

(defmethod (setf dom:node-value) (newval (self notation))
  (declare (ignore newval)))

;; attributes

;; (gibt es nur auf element)

(defmethod dom:attributes ((self node))
  nil)

;; dann fehlt noch can-adopt und attribute conventions fuer adoption

;;; NodeList

(defun make-node-list (&optional initial-contents)
  (make-array (length initial-contents)
              :adjustable t
              :fill-pointer (length initial-contents)
              :initial-contents initial-contents))

(defmethod dom:item ((self vector) index)
  (if (< index (length self))
      (elt self index)
      nil))

(defmethod dom:length ((self vector))
  (length self))

;;; NAMED-NODE-MAP

(defmethod dom:get-named-item ((self named-node-map) name)
  (setf name (rod name))
  (with-slots (items) self
    (dolist (k items nil)
      (cond ((rod= name (dom:node-name k))
             (return k))))))

(defmethod dom:set-named-item ((self named-node-map) arg)
  (assert-writeable self)
  (unless (eq (dom:node-type arg) (slot-value self 'element-type))
    (dom-error :HIERARCHY_REQUEST_ERR
               "~S cannot adopt ~S, since it is not of type ~S."
               self arg (slot-value self 'element-type)))
  (unless (eq (dom:owner-document self) (dom:owner-document arg))
    (dom-error :WRONG_DOCUMENT_ERR
               "~S cannot adopt ~S, since it was created by a different document."
               self arg))
  (let ((old-map (slot-value arg 'map)))
    (when (and old-map (not (eq old-map self)))
      (dom-error :INUSE_ATTRIBUTE_ERR "Attribute node already mapped" arg)))
  (setf (slot-value arg 'map) self)
  (let ((name (dom:node-name arg)))
    (with-slots (items) self
      (dolist (k items (progn (setf items (cons arg items))nil))
        (cond ((rod= name (dom:node-name k))
               (setf items (cons arg (delete k items)))
               (return k)))))))

(defmethod dom:remove-named-item ((self named-node-map) name)
  (assert-writeable self)
  (setf name (rod name))
  (with-slots (items) self
    (dolist (k items (dom-error :NOT_FOUND_ERR "~A not found in ~A" name self))
      (cond ((rod= name (dom:node-name k))
             (setf items (delete k items))
             (return k))))))

(defmethod dom:length ((self named-node-map))
  (with-slots (items) self
    (length items)))

(defmethod dom:item ((self named-node-map) index)
  (with-slots (items) self
    (do ((nthcdr items (cdr nthcdr))
         (i index (1- i)))
        ((zerop i) (car nthcdr)))))

;;; CHARACTER-DATA

(defmethod (setf dom:data) (newval (self character-data))
  (assert-writeable self)
  (setf newval (rod newval))
  (setf (slot-value self 'value) newval))

(defmethod dom:length ((node character-data))
  (length (slot-value node 'value)))

(defmethod dom:substring-data ((node character-data) offset count)
  (with-slots (value) node
    (unless (<= 0 offset (length value))
      (dom-error :INDEX_SIZE_ERR "offset is invalid"))
    (let ((end (min (length value) (+ offset count))))
      (subseq value offset end))))

(defmethod dom:append-data ((node character-data) arg)
  (assert-writeable node)
  (setq arg (rod arg))
  (with-slots (value) node
    (setf value (concatenate (type-of value) value arg)))
  (values))

(defmethod dom:delete-data ((node character-data) offset count)
  (assert-writeable node)
  (with-slots (value) node
    (unless (<= 0 offset (length value))
      (dom-error :INDEX_SIZE_ERR "offset is invalid"))
    (when (minusp count)
      (dom-error :INDEX_SIZE_ERR "count is negative"))
    (setf count (min count (- (length value) offset)))
    (let ((new (make-array (- (length value) count)
                           :element-type (array-element-type value))))
      (replace new value 
               :start1 0 :end1 offset
               :start2 0 :end2 offset)
      (replace new value 
               :start1 offset :end1 (length new)
               :start2 (+ offset count) :end2 (length value))
      (setf value new)))
  (values))

(defmethod dom:replace-data ((node character-data) offset count arg)
  ;; Although we could implement this by calling DELETE-DATA, then INSERT-DATA,
  ;; we implement this function directly to avoid creating temporary garbage.
  (assert-writeable node)
  (setf arg (rod arg))
  (with-slots (value) node
    (unless (<= 0 offset (length value))
      (dom-error :INDEX_SIZE_ERR "offset is invalid"))
    (when (minusp count)
      (dom-error :INDEX_SIZE_ERR "count is negative"))
    (setf count (min count (- (length value) offset)))
    (if (= count (length arg))
        (replace value arg
                 :start1 offset :end1 (+ offset count)
                 :start2 0 :end2 count)
        (let ((new (make-array (+ (length value) (length arg) (- count))
                               :element-type (array-element-type value))))
          (replace new value :end1 offset)
          (replace new arg :start1 offset)
          (replace new value
                   :start1 (+ offset (length arg))
                   :start2 (+ offset count))
          (setf value new))))
  (values))

(defmethod dom:insert-data ((node character-data) offset arg)
  (assert-writeable node)
  (setf arg (rod arg))
  (with-slots (value) node
    (unless (<= 0 offset (length value))
      (dom-error :INDEX_SIZE_ERR "offset is invalid"))
    (let ((new (make-array (+ (length value) (length arg))
                           :element-type (array-element-type value)))
          (arglen (length arg)))
      (replace new value :end1 offset)
      (replace new arg :start1 offset)
      (replace new value :start1 (+ offset arglen) :start2 offset)
      (setf value new)))
  (values))

;;; ATTR
;;;
;;; An attribute value can be read and set as a string using DOM:VALUE
;;; or frobbed by changing the attribute's children!
;;;
;;; We store the value in a TEXT node and read this node's DATA slot
;;; when asked for our VALUE -- until the user changes the child nodes,
;;; in which case we have to compute VALUE by traversing the children.

(defmethod dom:value ((node attribute))
  (with-slots (children) node
    (cond
      ((zerop (length children))
        #.(rod-string ""))
      ((and (eql (length children) 1)
            (eq (dom:node-type (elt children 0)) :text))
        ;; we have as single TEXT-NODE child, just return its DATA
        (dom:data (elt children 0)))
      (t
        ;; traverse children to compute value
        (attribute-to-string node)))))

(defmethod (setf dom:value) (new-value (node attribute))
  (assert-writeable node)
  (let ((rod (rod new-value)))
    (with-slots (children owner) node
      ;; remove children, add new TEXT-NODE child
      ;; (alas, we must not reuse an old TEXT-NODE)
      (while (plusp (length children))
        (dom:remove-child node (dom:last-child node)))
      (dom:append-child node (dom:create-text-node owner rod))))
  new-value)

(defun attribute-to-string (attribute)
  (let ((stream (make-rod-stream)))
    (flet ((doit ()
             (dovector (child (dom:child-nodes attribute))
               (write-attribute-child child stream))))
      (doit)
      (initialize-rod-stream stream)
      (doit))
    (rod-stream-buf stream)))

(defmethod write-attribute-child ((node node) stream)
  (write-rod (dom:node-value node) stream))

(defmethod write-attribute-child ((node entity-reference) stream)
  (dovector (child (dom:child-nodes node))
    (write-attribute-child child stream)))

;;; ROD-STREAM als Ersatz fuer MAKE-STRING-OUTPUT-STREAM zu verwenden,
;;; nur dass der Buffer statische Groesse hat.  Solange er NIL ist,
;;; zaehlt der Stream nur die Runen.  Dann ruft man INITIALIZE-ROD-STREAM
;;; auf, um den Buffer zu erzeugen und die Position zurueckzusetzen, und
;;; schreibt alles abermals.  Dann ist der Buffer gefuellt.
(defstruct rod-stream
  (buf nil)
  (position 0))

(defun write-rod (rod rod-stream)
  (let ((buf (rod-stream-buf rod-stream)))
    (when buf
      (move rod buf 0 (rod-stream-position rod-stream) (length rod)))
    (incf (rod-stream-position rod-stream) (length rod)))
  rod)

(defun initialize-rod-stream (stream)
  (setf (rod-stream-buf stream) (make-rod (rod-stream-position stream)))
  (setf (rod-stream-position stream) 0)
  stream)

;;; ELEMENT

(defmethod dom:get-attribute-node ((element element) name)
  (dom:get-named-item (dom:attributes element) name))

(defmethod dom:set-attribute-node ((element element) (new-attr attribute))
  (assert-writeable element)
  (dom:set-named-item (dom:attributes element) new-attr))

(defmethod dom:get-attribute ((element element) name)
  (let ((a (dom:get-attribute-node element name)))
    (if a
        (dom:value a)
        #.(string-rod ""))))

(defmethod dom:set-attribute ((element element) name value)
  (assert-writeable element)
  (with-slots (owner) element
    (let ((attr (dom:create-attribute owner name)))
      (setf (dom:value attr) value)
      (dom:set-attribute-node element attr))
    (values)))

(defmethod dom:remove-attribute ((element element) name)
  (assert-writeable element)
  (dom:remove-attribute-node element (dom:get-attribute-node element name)))

(defmethod dom:remove-attribute-node ((element element) (old-attr attribute))
  (assert-writeable element)
  (with-slots (items) (dom:attributes element)
    (unless (find old-attr items)
      (dom-error :NOT_FOUND_ERR "Attribute not found."))
    (setf items (remove old-attr items))
    old-attr))

(defmethod dom:get-elements-by-tag-name ((element element) name)
  (assert-writeable element)
  (get-elements-by-tag-name-internal element name))

(defmethod dom:normalize ((element element))
  (assert-writeable element)
  (labels ((walk (n)
             (when (eq (dom:node-type n) :element)
               (map nil #'walk (dom:items (dom:attributes n))))
             (let ((children (dom:child-nodes n))
                   (i 0)
                   (previous nil))
               ;; careful here, we're modifying the array we are iterating over
               (while (< i (length children))
                 (let ((child (elt children i)))
                   (cond
                     ((not (eq (dom:node-type child) :text))
                       (setf previous nil)
                       (incf i))
                     ((and previous (eq (dom:node-type previous) :text))
                       (setf (slot-value previous 'value)
                             (concatenate 'vector
                               (dom:data previous)
                               (dom:data child)))
                       (dom:remove-child n child)
                       ;; not (incf i)
                       )
                     (t
                       (setf previous child)
                       (incf i)))))) 
             (map nil #'walk (dom:child-nodes n))))
    (walk element))
  (values))

;;; TEXT

(defmethod dom:split-text ((text text) offset)
  (assert-writeable text)
  (with-slots (owner parent value) text
    (unless (<= 0 offset (length value))
      (dom-error :INDEX_SIZE_ERR "offset is invalid"))
    (prog1
        (dom:insert-before parent
                           (dom:create-text-node owner (subseq value offset))
                           (dom:next-sibling text))
      (setf value (subseq value 0 offset)))))

;;; COMMENT -- nix
;;; CDATA-SECTION -- nix

;;; DOCUMENT-TYPE -- missing
;;; NOTATION -- nix
;;; ENTITY -- nix

;;; ENTITY-REFERENCE

(defmethod initialize-instance :after ((instance entity-reference) &key)
  (let* ((owner (dom:owner-document instance))
         (entities (or (entities owner) xml::*entities*))
         (children (xml::resolve-entity (dom:name instance) entities)))
    (setf (slot-value instance 'children)
          (make-node-list
           (map 'vector
             (lambda (node) (dom:import-node owner node t))
             children))))
  (labels ((walk (n)
             (setf (slot-value n 'read-only-p) t)
             (when (dom:element-p n)
               (map nil #'walk (dom:items (dom:attributes n))))
             (map nil #'walk (dom:child-nodes n))))
    (walk instance)))

;;; PROCESSING-INSTRUCTION

(defmethod (setf dom:data) (newval (self processing-instruction))
  (assert-writeable self)
  (setf newval (rod newval))
  (setf (slot-value self 'data) newval))

;; das koennte man auch mit einer GF machen
(defun can-adopt-p (parent child)
  (member (dom:node-type child)
          (let ((default '(:element :processing-instruction :comment :text
                           :cdata-section :entity-reference)))
            (etypecase parent
              (document
                '(:element :processing-instruction :comment :document-type))
              (document-fragment default)
              (document-type nil)
              (entity-reference default)
              (element default)
              (attribute '(:text :entity-reference))
              (processing-instruction nil)
              (comment nil)
              (text nil)
              (cdata-section nil)
              (entity default)
              (notation nil)))))


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


;;; IMPORT-NODE

(defvar *clone-not-import* nil)         ;not beautiful, I know.  See below.

(defmethod import-node-internal (class document node deep &rest initargs)
  (let ((result (apply #'make-instance class :owner document initargs)))
    (when deep
      (dovector (child (dom:child-nodes node))
        (dom:append-child result (dom:import-node document child t))))
    result))

(defmethod dom:import-node ((document document) (node attribute) deep)
  (declare (ignore deep))
  (import-node-internal 'attribute document node t :name (dom:name node)))

(defmethod dom:import-node ((document document) (node document-fragment) deep)
  (import-node-internal 'document-fragment document node deep))

(defmethod dom:import-node ((document document) (node element) deep)
  (let* ((attributes (make-instance 'named-node-map
                       :element-type :attribute
                       :owner document))
         (result (import-node-internal 'element document node deep
                                       :attributes attributes
                                       :tag-name (dom:tag-name node))))
    (dolist (attribute (dom:items (dom:attributes node)))
      (when (or (dom:specified attribute) *clone-not-import*)
        (dom:set-attribute result (dom:name attribute) (dom:value attribute))))
    result))

(defmethod dom:import-node ((document document) (node entity) deep)
  (import-node-internal 'entity document node deep
                        :public-id (dom:public-id node)
                        :system-id (dom:system-id node)
                        :notation-name (dom:notation-name node)))

(defmethod dom:import-node ((document document) (node entity-reference) deep)
  (declare (ignore deep))
  #+(or)
  (import-node-internal 'entity-reference document node nil
                        :name (dom:name node))
  ;; XXX If the document being imported into provides a definition for
  ;; this entity name, its value is assigned.
  (dom-error :NOT_SUPPORTED_ERR "not implemented"))

(defmethod dom:import-node ((document document) (node notation) deep)
  (import-node-internal 'notation document node deep
                        :name (dom:name node)
                        :public-id (dom:public-id node)
                        :system-id (dom:system-id node)))

(defmethod dom:import-node
    ((document document) (node processing-instruction) deep)
  (import-node-internal 'processing-instruction document node deep
                        :target (dom:target node)
                        :data (dom:data node)))

;; TEXT_NODE, CDATA_SECTION_NODE, COMMENT_NODE
(defmethod dom:import-node
    ((document document) (node character-data) deep)
  (import-node-internal (class-of node) document node deep
                        :data (copy-seq (dom:data node))))

;;; CLONE-NODE
;;;
;;; As far as I can tell, cloneNode is the same as importNode, except
;;; for one difference involving element attributes: importNode imports
;;; only specified attributes, cloneNode copies even default values.
;;;
;;; Since I don't want to reimplement all of importNode here, we run
;;; importNode with a special flag...

(defmethod dom:clone-node ((node node) deep)
  (let ((*clone-not-import* t))
    (dom:import-node (dom:owner-document node) node deep)))
