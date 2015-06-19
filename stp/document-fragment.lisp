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

(defmethod insert-child ((parent parent-node) (fragment fxml.stp:document-fragment) pos)
  (let* ((children (node.children fragment))
         (len (length children)))
    (unless (= len 0)
      (loop for i downfrom (1- len) to 0
            for child = (elt children i)
            do (setf (node.parent child) parent)
               (insert-child parent child pos)))
    parent))


