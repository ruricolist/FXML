;;; A wrapper package STRING-DOM around the ordinary DOM presents
;;; DOMString as Lisp STRING.  This was a workaround until
;;; RUNE-IS-CHARACTER was implemented, but might still be useful on
;;; Lisps without Unicode support.

(defpackage :string-dom
  (:use))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-external-symbols (var :dom)
    (let* ((home-package
            (if (member var '(dom:data dom:name dom:value dom:tag-name
                              dom:node-name dom:node-value
                              dom:substring-data dom:get-attribute
                              dom:set-attribute dom:public-id dom:system-id
                              dom:notation-name dom:target))
                :string-dom
                :dom))
           (symbol (intern (symbol-name var) home-package)))
      (import symbol :string-dom)
      (export (list symbol) :string-dom))))

(defpackage :string-dom-impl (:use :cl))
(in-package :string-dom-impl)

(defun rod-to-string (frob)
  (if (null frob)
      nil
      (map 'string #'code-char frob)))

(defun string-dom:data (node)		(rod-to-string (dom:data node)))
(defun string-dom:name (node)		(rod-to-string (dom:name node)))
(defun string-dom:value (node)		(rod-to-string (dom:value node)))
(defun string-dom:tag-name (node)	(rod-to-string (dom:tag-name node)))
(defun string-dom:node-name (node)	(rod-to-string (dom:node-name node)))
(defun string-dom:node-value (node)	(rod-to-string (dom:node-value node)))

(defun (setf string-dom:data) (newval node)
  (setf (dom:data node) newval))

(defun (setf string-dom:value) (newval node)
  (setf (dom:value node) newval))

(defun (setf string-dom:node-value) (newval node)
  (setf (dom:node-value node) newval))

(defun string-dom:substring-data (node offset count)
  (rod-to-string (dom:substring-data node offset count)))

(defun string-dom:get-attribute (elt name)
  (rod-to-string (dom:get-attribute elt name)))

(defun string-dom:set-attribute (elt name value)
  (dom:set-attribute elt (runes:rod name) (runes:rod value)))

(defun string-dom:public-id (node)
  (rod-to-string (dom:public-id node)))

(defun string-dom:system-id (node)
  (rod-to-string (dom:system-id node)))

(defun string-dom:notation-name (node)
  (rod-to-string (dom:notation-name node)))

(defun string-dom:target (node)
  (rod-to-string (dom:target node)))
