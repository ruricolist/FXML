(defpackage :string-dom
  (:use))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-external-symbols (var :cdom)
    (let* ((home-package
            (if (member var '(cdom:data cdom:name cdom:value cdom:tag-name
                              cdom:node-name cdom:node-value
                              cdom:substring-data cdom:get-attribute))
                :string-dom
                :cdom))
           (symbol (intern (symbol-name var) home-package)))
      (import symbol :string-dom)
      (export (list symbol) :string-dom))))

(defpackage :string-dom-impl (:use :cl))
(in-package :string-dom-impl)

(defun rod-to-string (frob)
  (if (null frob)
      nil
      (map 'string #'code-char frob)))

(defun string-dom:data (node)		(rod-to-string (cdom:data node)))
(defun string-dom:name (node)		(rod-to-string (cdom:name node)))
(defun string-dom:value (node)		(rod-to-string (cdom:value node)))
(defun string-dom:tag-name (node)	(rod-to-string (cdom:tag-name node)))
(defun string-dom:node-name (node)	(rod-to-string (cdom:node-name node)))
(defun string-dom:node-value (node)	(rod-to-string (cdom:node-value node)))

(defun string-dom:substring-data (node offset count)
  (rod-to-string (cdom:substring-data node offset count)))

(defun string-dom:get-attribute (elt name)
  (rod-to-string (cdom:get-attribute elt name)))
