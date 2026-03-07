;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; Copyright (c) 2008 Ivan Shvedunov. All rights reserved.
;;; Copyright (c) 2008 David Lichteblau. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :fxml.xmls)

(defstruct xmls-attribute
  local-name
  uri
  value)

(defstruct xmls-namespace
  parent
  uri)


(defmethod xpath-protocol:node-p-using-navigator
    ((navi xpath-navigator) node)
  nil)
(defmethod xpath-protocol:node-p-using-navigator
    ((navi xpath-navigator) (node cons))
  t)
(defmethod xpath-protocol:node-p-using-navigator
    ((navi xpath-navigator) (node string))
  ;; check that we've seen it before
  (with-slots (parents) navi
    (nth-value 1 (gethash node parents))))
(defmethod xpath-protocol:node-p-using-navigator
    ((navi xpath-navigator) (node xmls-attribute))
  t)
(defmethod xpath-protocol:node-p-using-navigator
    ((navi xpath-navigator) (node xmls-namespace))
  t)

(defmethod xpath-protocol:node-equal-using-navigator
    ((navi xpath-navigator) (a t) (b t))
  nil)

(defmethod xpath-protocol:node-equal-using-navigator
    ((navi xpath-navigator) (a cons) (b cons))
  (eq a b))

(defmethod xpath-protocol:node-equal-using-navigator
    ((navi xpath-navigator) (a string) (b string))
  (equal a b))

(defmethod xpath-protocol:hash-key-using-navigator
    ((navi xpath-navigator) node)
  node)

(defun strip-prefix (car)
  (let ((pos (position #\: car)))
    (if pos
        (subseq car (1+ pos))
        car)))

(defmethod xpath-protocol:local-name-using-navigator
    ((navi xpath-navigator) (node cons))
  (let ((car (car node)))
    (if (consp car)
        (car car)
        (strip-prefix car))))

(defmethod xpath-protocol:namespace-prefix-using-navigator
    ((navi xpath-navigator) (node cons))
  (register-prefix navi (node-ns node)))

;; the xmls tree doesn't record prefixes, only URIs, so just invent a prefix
(defun register-prefix (navi uri)
  (with-slots (prefixes) navi
    (if (zerop (length uri))
        ""
        (or (gethash uri prefixes)
            (setf (gethash uri prefixes)
                  (format nil "ns-~D" (hash-table-count prefixes)))))))

(defmethod xpath-protocol:namespace-uri-using-navigator
    ((navi xpath-navigator) (node cons))
  (or (node-ns node) ""))

(defmethod xpath-protocol:qualified-name-using-navigator
    ((navi xpath-navigator) node)
  (let ((uri (xpath-protocol:namespace-uri-using-navigator navi node))
        (lname (xpath-protocol:local-name-using-navigator navi node)))
    (if (zerop (length uri))
        lname
        (format nil "~A:~A"
                (xpath-protocol:namespace-prefix-using-navigator navi node)
                lname))))

(defmethod xpath-protocol:base-uri-using-navigator
    ((navi xpath-navigator) (node cons))
  "#unsupported-by-xmls")

(defmethod xpath-protocol:local-name-using-navigator
    ((navi xpath-navigator) (node cons))
  (node-name node))

(defun copy-node (x)
  (etypecase x
    (string
     (replace (make-string (length x)) x))
    (list
     (apply #'list x))))

(defmethod xpath-protocol:parent-node-using-navigator
    ((navi xpath-navigator) node)
  (with-slots (children parents) navi
    (gethash node parents)))

(defmethod xpath-protocol:child-pipe-using-navigator
    ((navi xpath-navigator) (node cons))
  (with-slots (children parents) navi
    (multiple-value-bind (c cp)
        (gethash node children)
      (if cp
          c
          (setf (gethash node children)
                (loop
                  for child in (node-children node)
                  ;; protect against structure sharing, in particular strings
                  ;; used multiple times:
                  for oops = (gethash child parents)
                  for replacement = (if oops (copy-node child) child)
                  collect replacement
                  ;; register the parent
                  do (setf (gethash replacement parents) node)))))))

(defmethod xpath-protocol:attribute-pipe-using-navigator
    ((navi xpath-navigator) (node cons))
  (with-slots (attributes parents) navi
    (multiple-value-bind (a ap)
        (gethash node attributes)
      (if ap
          a
          (setf (gethash node attributes)
                (loop
                  for (name value) in (second node)
                  for struct = (if (consp name)
                                   (make-xmls-attribute
                                    :local-name (car name)
                                    :uri (cdr name)
                                    :value value)
                                   (make-xmls-attribute
                                    :local-name (strip-prefix name)
                                    :uri ""
                                    :value value))
                  collect struct
                  ;; register the parent
                  do (setf (gethash struct parents) struct)))))))

(defmethod xpath-protocol:namespace-pipe-using-navigator
    ((navi xpath-navigator) (node cons))
  (with-slots (namespaces) navi
    (multiple-value-bind (ns nsp)
        (gethash node namespaces)
      (if nsp
          ns
          (setf (gethash node namespaces)
                (build-xpath-namespaces navi node))))))

(defun build-xpath-namespaces (navi node)
  (with-slots (parents) navi
    (let ((table (make-hash-table :test 'equal))
          (result '()))
      (labels ((record* (parent uri)
                 (unless (gethash uri table)
                   (let ((struct
                           (make-xmls-namespace :parent parent :uri uri)))
                     (setf (gethash struct parents) parent)
                     (setf (gethash uri table) struct))))
               (record (parent node)
                 (let ((uri
                         (xpath-protocol:namespace-uri-using-navigator
                          navi node)))
                   (record* parent uri)))
               (recurse (node)
                 (let ((parent (gethash node parents)))
                   (when parent
                     (recurse parent)))
                 (record node node)
                 (dolist (attribute
                          (xpath-protocol:attribute-pipe-using-navigator
                           navi node))
                   (record node attribute))))
        (record* nil "http://www.w3.org/XML/1998/namespace")
        (recurse node))
      (maphash #'(lambda (prefix nsnode)
                   (declare (ignore prefix))
                   (push nsnode result))
               table)
      result)))

(defmethod xpath-protocol:node-p-using-navigator
    ((navi xpath-navigator) (node xmls-namespace))
  t)

(defmethod xpath-protocol:child-pipe-using-navigator
    ((navi xpath-navigator) (node xmls-namespace))
  nil)

(defmethod xpath-protocol:attribute-pipe-using-navigator
    ((navi xpath-navigator) (node xmls-namespace))
  nil)

(defmethod xpath-protocol:namespace-pipe-using-navigator
    ((navi xpath-navigator) (node xmls-namespace))
  nil)

(defmethod xpath-protocol:local-name-using-navigator
    ((navi xpath-navigator) (node xmls-namespace))
  (xpath-protocol:namespace-prefix-using-navigator navi node))

(defmethod xpath-protocol:qualified-name-using-navigator
    ((navi xpath-navigator) (node xmls-namespace))
  (xpath-protocol:namespace-prefix-using-navigator navi node))

(defmethod xpath-protocol:namespace-uri-using-navigator
    ((navi xpath-navigator) (node xmls-namespace))
  (xmls-namespace-uri node))

(defmethod xpath-protocol:node-p-using-navigator
    ((navi xpath-navigator) (node xmls-attribute))
  t)

(defmethod xpath-protocol:child-pipe-using-navigator
    ((navi xpath-navigator) (node xmls-attribute))
  nil)

(defmethod xpath-protocol:attribute-pipe-using-navigator
    ((navi xpath-navigator) (node xmls-attribute))
  nil)

(defmethod xpath-protocol:namespace-pipe-using-navigator
    ((navi xpath-navigator) (node xmls-attribute))
  nil)

(defmethod xpath-protocol:local-name-using-navigator
    ((navi xpath-navigator) (node xmls-attribute))
  (xmls-attribute-local-name node))

(defmethod xpath-protocol:namespace-uri-using-navigator
    ((navi xpath-navigator) (node xmls-attribute))
  (xmls-attribute-uri node))

(defmethod xpath-protocol:node-text-using-navigator
    ((navi xpath-navigator) node)
  (with-output-to-string (*standard-output*)
    (labels ((write-text (node)
               (etypecase node
                 (xmls-attribute
                  (write-string (xmls-attribute-value node)))
                 (string
                  (write-string node))
                 (cons
                  (mapc #'write-text (node-children node))))))
      (write-text node))))

(defmethod xpath-protocol:node-type-p-using-navigator
    ((navi xpath-navigator) (node cons) (type t))
  nil)

(defmethod xpath-protocol:node-type-p-using-navigator
    ((navi xpath-navigator) (node string) (type t))
  nil)

(defmethod xpath-protocol:node-type-p-using-navigator
    ((navi xpath-navigator) (node xmls-attribute) (type t))
  nil)

(defmethod xpath-protocol:node-type-p-using-navigator
    ((navi xpath-navigator) (node xmls-namespace) (type t))
  nil)

(macrolet ((deftypemapping (class keyword)
             `(defmethod xpath-protocol:node-type-p-using-navigator
                  ((navi xpath-navigator) (node ,class) (type (eql ,keyword)))
                t)))
  (deftypemapping string :text)
  (deftypemapping cons :element)
  (deftypemapping xmls-attribute :attribute)
  (deftypemapping xmls-namespace :namespace))

(defmethod xpath-protocol:local-name-using-navigator
    ((navi xpath-navigator) (node string))
  "")

(defmethod xpath-protocol:namespace-prefix-using-navigator
    ((navi xpath-navigator) (node string))
  "")

(defmethod xpath-protocol:namespace-uri-using-navigator
    ((navi xpath-navigator) (node string))
  "")

(defmethod xpath-protocol:qualified-name-using-navigator
    ((navi xpath-navigator) (node string))
  "")

(defmethod xpath-protocol:child-pipe-using-navigator
    ((navi xpath-navigator) (node string))
  nil)

(defmethod xpath-protocol:attribute-pipe-using-navigator
    ((navi xpath-navigator) (node string))
  nil)

(defmethod xpath-protocol:namespace-pipe-using-navigator
    ((navi xpath-navigator) (node string))
  nil)

(defmethod xpath-protocol:get-element-by-id-using-navigator
    ((navi xpath-navigator) (node t) (id t))
  nil)

(defmethod xpath-protocol:unparsed-entity-uri-using-navigator
    ((navi xpath-navigator) (node t) (name t))
  nil)

(defmethod xpath-protocol:node-equal-using-navigator
    ((navi xpath-navigator) (a xmls-namespace) (b xmls-namespace))
  (and (eq (xmls-namespace-parent a) (xmls-namespace-parent b))
       (equal (xmls-namespace-uri a) (xmls-namespace-uri b))))

(defmethod xpath-protocol:hash-key-using-navigator
    ((navi xpath-navigator) (node xmls-namespace))
  (cons (xmls-namespace-parent node) (xmls-namespace-uri node)))
