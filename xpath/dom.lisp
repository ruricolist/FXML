(in-package :fxml.rune-dom)

(eval-when (:compile-toplevel :load-toplevel)
  (import '(xpath-protocol:define-default-method
            xpath::empty-pipe
            xpath::vector->pipe
            xpath::filter-pipe)))

;; DOM mapping: simple slots

(define-default-method xpath-protocol:node-p ((node fxml.dom:node)) t)

(define-default-method xpath-protocol:parent-node ((node fxml.dom:attr))
  (fxml.dom:owner-element node))

(define-default-method xpath-protocol:parent-node ((node fxml.dom:node))
  (fxml.dom:parent-node node))

(define-default-method xpath-protocol:local-name ((node fxml.dom:node))
  ;; fixme?
  (or (fxml.dom:local-name node) (fxml.dom:node-name node)))

(define-default-method xpath-protocol:namespace-prefix ((node fxml.dom:node))
  (fxml.dom:prefix node))

(define-default-method xpath-protocol:namespace-uri ((node fxml.dom:node))
  (or (fxml.dom:namespace-uri node) ""))

(define-default-method xpath-protocol:qualified-name ((node fxml.dom:node))
  (fxml.dom:node-name node))

(define-default-method xpath-protocol:processing-instruction-target
    ((node fxml.dom:node))
  (fxml.dom:node-value node))

(define-default-method xpath-protocol:base-uri ((node fxml.dom:node))
  ;; fixme
  "")


;; DOM mapping: pipes

(define-default-method xpath-protocol:child-pipe ((node fxml.dom:node))
  empty-pipe)

(define-default-method xpath-protocol:child-pipe ((node fxml.dom:document))
  (list (fxml.dom:document-element node)))

(define-default-method xpath-protocol:child-pipe ((node fxml.dom:element))
  (vector->pipe (fxml.dom:child-nodes node)))

(define-default-method xpath-protocol:attribute-pipe ((node fxml.dom:node))
  empty-pipe)

(define-default-method xpath-protocol:attribute-pipe ((node fxml.dom:element))
  (filter-pipe (lambda (item)
                 (not (equal (fxml.dom:namespace-uri item)
                             "http://www.w3.org/2000/xmlns/")))
               (vector->pipe (fxml.dom:items (fxml.dom:attributes node)))))

(define-default-method xpath-protocol:namespace-pipe ((node fxml.dom:node))
  (when (fxml.dom:parent-node node)
    (xpath-protocol:namespace-pipe (fxml.dom:parent-node node))))

(defstruct (dom-namespace
            (:constructor make-dom-namespace (parent prefix uri)))
  parent
  prefix
  uri)

(define-default-method xpath-protocol:node-p ((node dom-namespace))
  t)

(define-default-method xpath-protocol:node-equal
    ((a dom-namespace) (b dom-namespace))
  (and (eq (dom-namespace-parent a) (dom-namespace-parent b))
       (equal (dom-namespace-prefix a) (dom-namespace-prefix b))))

(define-default-method xpath-protocol:hash-key
    ((node dom-namespace))
  (cons (dom-namespace-parent node) (dom-namespace-prefix node)))

(define-default-method xpath-protocol:child-pipe
    ((node dom-namespace))
  empty-pipe)
(define-default-method xpath-protocol:attribute-pipe
    ((node dom-namespace))
  empty-pipe)
(define-default-method xpath-protocol:namespace-pipe
    ((node dom-namespace))
  empty-pipe)

(define-default-method xpath-protocol:parent-node ((node dom-namespace))
  (dom-namespace-parent node))
(define-default-method xpath-protocol:local-name ((node dom-namespace))
  (dom-namespace-prefix node))
(define-default-method xpath-protocol:qualified-name ((node dom-namespace))
  (dom-namespace-prefix node))
(define-default-method xpath-protocol:namespace-prefix ((node dom-namespace))
  nil)
(define-default-method xpath-protocol:namespace-uri ((node dom-namespace))
  "")

(define-default-method xpath-protocol:namespace-pipe ((node fxml.dom:element))
  ;; FIXME: completely untested
  ;; FIXME: rewrite this lazily?
  (let ((table (make-hash-table :test 'equal))
        (result '()))
    (labels ((record* (parent prefix uri)
               (unless (or (equal prefix "xmlns")
                           (gethash prefix table))
                 (setf (gethash prefix table)
                       (make-dom-namespace parent prefix uri))))
             (record (parent node)
               (record* parent
                        (or (fxml.dom:prefix node) "")
                        (fxml.dom:namespace-uri node)))
             (recurse (node)
               (record node node)
               (dolist (attribute (fxml.dom:items (fxml.dom:attributes node)))
                 (cond
                   ((equal (fxml.dom:namespace-uri attribute)
                           "http://www.w3.org/2000/xmlns/")
                    ;; record explicitly declared namespaces, which might
                    ;; not be in use anywhere
                    (record* node
                             (fxml.dom:local-name attribute)
                             (fxml.dom:value attribute)))
                   ((plusp (length (fxml.dom:prefix attribute)))
                    ;; record namespaces from DOM 2 slots, which might not
                    ;; be declared in an attribute
                    (record node attribute))))
               (let ((parent (fxml.dom:parent-node node)))
                 (when parent
                   (recurse parent)))))
      (record* nil "xml" "http://www.w3.org/XML/1998/namespace")
      (recurse node))
    (maphash (lambda (prefix nsnode)
               (declare (ignore prefix))
               (push nsnode result))
             table)
    result))

(define-default-method xpath-protocol:node-text ((node fxml.dom:node))
  (with-output-to-string (s)
    (labels ((write-text (node)
               (when (fxml.dom:document-p node)
                 (setf node (fxml.dom:document-element node)))
               (when (fxml.dom:document-fragment-p node)
                 (write-string (fxml.dom:text-content node) s)
                 (return-from write-text))
               (let ((value (fxml.dom:node-value node)))
                 (when value (write-string value s))
                 (unless (fxml.dom:attribute-p node) ;; FIXME: verify CDATA sections
                   (fxml.dom:do-node-list (child (fxml.dom:child-nodes node))
                     (cond ((or (fxml.dom:element-p child)
                                (fxml.dom:entity-reference-p child))
                            (write-text child))
                           ((or (fxml.dom:text-node-p child)
                                (fxml.dom:attribute-p child)
                                (fxml.dom:cdata-section-p child))
                            (write-string (fxml.dom:node-value child) s))))))))
      (write-text node))))

(define-default-method xpath-protocol:node-text ((node dom-namespace))
  (dom-namespace-uri node))

;; currently computed from child-pipe
;;; (defmethod preceding-sibling-pipe ()
;;;   (let ((parent (fxml.dom:parent-node node)))
;;;     (if parent
;;;     (let* ((children (fxml.dom:child-nodes parent))
;;;            (pos (position node children)))
;;;       (loop
;;;          for i from (1- pos) downto 0
;;;          collect (elt children i)))
;;;     empty-pipe)))

(define-default-method xpath-protocol:node-type-p ((node fxml.dom:node) type)
  (declare (ignore type))
  nil)

(define-default-method xpath-protocol:node-type-p ((node dom-namespace) type)
  (declare (ignore type))
  nil)

(macrolet ((deftypemapping (class keyword)
             `(define-default-method xpath-protocol:node-type-p
                  ((node ,class) (type (eql ,keyword)))
                t)))
  (deftypemapping fxml.dom:comment :comment)
  (deftypemapping fxml.dom:processing-instruction :processing-instruction)
  (deftypemapping fxml.dom:text :text)
  (deftypemapping fxml.dom:attr :attribute)
  (deftypemapping fxml.dom:element :element)
  (deftypemapping dom-namespace :namespace)
  (deftypemapping fxml.dom:document :document))

(define-default-method xpath-protocol:get-element-by-id ((node fxml.dom:node) id)
  (fxml.dom:get-element-by-id
   (if (fxml.dom:document-p node) node (fxml.dom:owner-document node)) id))

(define-default-method xpath-protocol:unparsed-entity-uri
    ((node fxml.dom:node) name)
  (let ((dtd (fxml.rune-dom::dtd (if (fxml.dom:document-p node)
                                     node
                                     (fxml.dom:owner-document node)))))
    (when dtd
      (let ((entdef (cdr (gethash name (cxml::dtd-gentities dtd)))))
        (when (typep entdef 'cxml::external-entdef)
          (let ((uri (cxml::extid-system (cxml::entdef-extid entdef))))
            (when uri
              (puri:render-uri uri nil))))))))

;; Character data

(define-default-method xpath-protocol:local-name
    ((node fxml.dom:character-data))
  "")

(define-default-method xpath-protocol:namespace-prefix
    ((node fxml.dom:character-data))
  "")

(define-default-method xpath-protocol:namespace-uri
    ((node fxml.dom:character-data))
  "")

(define-default-method xpath-protocol:qualified-name
    ((node fxml.dom:character-data))
  "")
