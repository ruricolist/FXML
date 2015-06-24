(in-package #:fxml.stp.impl)

(defun make-document-fragment (&rest elements)
  (let ((result (make-instance 'fxml.stp:document-fragment)))
    (dolist (elt elements)
      (append-child result elt))
    result))

(defmethod copy ((fragment fxml.stp:document-fragment))
  (let ((result (make-instance 'fxml.stp:document-fragment)))
    (do-children (child fragment)
      (append-child result (copy child)))
    result))

(defmethod check-insertion-allowed ((parent document-fragment) child i)
  ;; XXX same as for element
  (declare (ignore i))
  (check-type child node)
  (assert-orphan child)
  (typecase child
    (element
     (when (childp parent child)
       (stp-error "attempt to add a node as its own descendant")))
    ((or comment processing-instruction text))
    (t (stp-error "not a valid child of an element: ~A" child))))

(defmethod insert-child ((parent parent-node) (fragment fxml.stp:document-fragment) pos)
  (let* ((children (node.children fragment))
         (len (length children)))
    (unless (= len 0)
      (loop for i downfrom (1- len) to 0
            for child = (elt children i)
            do (setf (node.parent child) parent)
               (insert-child parent child pos)))
    parent))

(defmethod replace-child ((parent parent-node) old-child (fragment fxml.stp:document-fragment))
  (declare (optimize (debug 0)))
  (labels ((rec ()
             (case (length (node.children fragment))
               (0)
               (1 (replace-child parent old-child (detach (first-child fragment))))
               (t (let ((new-child (detach (last-child fragment))))
                    (insert-child-after parent old-child new-child)
                    (rec))))))
    (rec)))


