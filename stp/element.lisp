;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-

;;; Copyright (c) 2007 David Lichteblau. All rights reserved.

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

(in-package :cxml-stp-impl)

#+sbcl
(declaim (optimize (debug 2)))


;;;; Class ELEMENT

(defgeneric local-name (node)
  (:documentation
   "@arg[node]{an @class{element} or @class{attribute}}
    @return{string, an NCName}
    @short{Returns the node's local name.}
    @see{qualified-name}
    @see{namespace-uri}
    @see{namespace-prefix}"))

(defgeneric namespace-uri (node)
  (:documentation
   "@arg[node]{an @class{element} or @class{attribute}}
    @return{string, a URI}
    @short{Returns the node's namespace URI.}
    @see{qualified-name}
    @see{local-name}
    @see{namespace-prefix}"))

(defgeneric namespace-prefix (node)
  (:documentation
   "@arg[node]{an @class{element} or @class{attribute}}
    @return{string, an NCName}
    @short{Returns the node's namespace prefix.}
    @see{qualified-name}
    @see{local-name}
    @see{namespace-uri}"))

(defgeneric (setf value) (newval attribute)
  (:documentation
   "@arg[newval]{a string of XML characters}
    @arg[attribute]{an @class{attribute}}
    @return{the value}
    @short{Sets the attribute's value.}"))


(defun make-element (name &optional (uri ""))
  "@arg[name]{string, a QName or NCName}
   @arg[uri]{a string, the namespace URI}
   @return{an @class{element}}
   @short{This function creates an element node of the given name.}"
  (check-type name runes:rod)
  (let ((result (make-instance 'element)))
    (multiple-value-bind (prefix local-name)
	(cxml::split-qname name)
      (setf prefix (or prefix ""))
      (setf (namespace-prefix result) prefix)
      (setf (namespace-uri result) uri)
      (setf (local-name result) local-name))
    result))

(defmethod copy ((node element))
  (let ((result (make-instance 'element)))
    (setf (%namespace-prefix result) (%namespace-prefix node))
    (setf (%local-name result) (%local-name node))
    (setf (%namespace-uri result) (%namespace-uri node))
    (setf (%namespaces result)
	  (when (%namespaces node)
	    (alexandria:copy-hash-table (%namespaces node))))
    (setf (%attributes result) (copy-attributes result node))
    (setf (%base-uri result) (find-base-uri node))
    (do-children (child node)
      (append-child result (copy child)))
    result))

(defun copy-attributes (new old)
  (mapcar (lambda (x)
	    (let ((y (copy x)))
	      (setf (%parent y) new)
	      y))
	  (%attributes old)))

(defun of-name (name &optional (uri ""))
  "@arg[name]{an NCName string or @code{nil}}
   @arg[uri]{a string, the namespace URI}
   @return{an function of one argument}
   @short{This function creates a test function for nodes of this name.}

   The function returned will return T if the argument is an instance
   of @class{attribute} or @class{element} and has the specified local-name
   and namespace URI, and will return NIL otherwise.

   If local-name is nil, only the namespace URI is considered for comparison.

   @see{local-name}
   @see{namespace-uri}"
  (when (find #\: name)
    (stp-error "of-name used with QName as an argument"))
  (lambda (x)
    (and (typep x '(or attribute element))
	 (or (null name) (equal (local-name x) name))
	 (equal (namespace-uri x) uri))))

(defun qualified-of-name (qname element)
  "@arg[qname]{string, a QName}
   @arg[element]{an element in which to look up @code{name}'s namespace}
   @return{an function of one argument}
   @short{This function creates a test function for nodes of this name.}

   @code{qname}'s namespace prefix is resolved into its namespace URI
   as declared by @code{element}.  If @code{qname} does not have a prefix,
   the namespace URI is the empty string.  If @code{qname}'s prefix is
   not declared on @code{element}, an error is signalled.

   A function is returned that will return T if the argument is an instance
   of @class{attribute} or @class{element} and has the local-name
   namespace URI specified by @code{qname}, and will return NIL otherwise.

   @see{qualified-name}
   @see{local-name}
   @see{find-namespace}
   @see{namespace-uri}"
  (multiple-value-bind (prefix local-name)
      (cxml::split-qname qname)
    (let ((uri (find-namespace prefix element)))
      (unless uri
	(stp-error "namespace ~A not declared on ~A" prefix element))
      (of-name local-name uri))))

(defun map-extra-namespaces (fn element)
  "@arg[fn]{a designator for a function of two arguments}
   @arg[element]{an instance of @class{element}}
   @return{nil}
   Call fn for each extra namespace declared on @code{element} with
   namespace prefix and URI as arguments."
  (when (%namespaces element)
    (maphash fn (%namespaces element))))

(defun find-extra-namespace (prefix element)
  "@arg[prefix]{a string}
   @arg[element]{an instance of @class{element}}
   @return{the namespace URI (a string), or nil}
   Find the extra namespace named @code{prefix} declared on @code{element}
   and return its namespace URI, or return nil if no such namespace was found."
  (when (%namespaces element)
    (gethash prefix (%namespaces element))))

(defun add-attribute (element attribute)
  "@arg[element]{an instance of @class{element}}
   @arg[attribute]{an instance of @class{attribute}}
   @short{Add a new attribute to @code{element} or replace an existing
     attribute node of the same name.}

   It is an error if the attribute's namespace conflicts with existing
   namespace declarations on this element."
  (check-type element element)
  (check-type attribute attribute)
  (assert-orphan attribute)
  (let ((local-name (local-name attribute))
	(prefix (namespace-prefix attribute))
	(uri (namespace-uri attribute)))
    (when (and (plusp (length prefix))
	       (not (equal "xml" prefix)))
      (when (and (equal prefix (namespace-prefix element))
		 (not (equal uri (namespace-uri element))))
	(stp-error "namespace collision with element when adding ~A to ~A"
		   attribute element))
      (let ((extra-uri (find-extra-namespace prefix element)))
	(when (and extra-uri (not (equal extra-uri uri)))
	  (stp-error "collision with extra namespaces when adding ~A to ~A"
		     attribute element))))
    (let ((other (find-attribute-namespace prefix element)))
      (when (and other (not (equal other uri)))
	(stp-error "collision with attribute namespace when adding ~A to ~A"
		   attribute element)))
    (let ((old (find-attribute-named element local-name uri)))
      (when old
	(%remove-attribute old)))
    (%add-attribute attribute element)
    (setf (%parent attribute) element)))

(defun %add-attribute (attribute element)
  (push attribute (%attributes element)))

(defun %remove-attribute (attribute)
  (alexandria:deletef (%attributes (parent attribute)) attribute)
  (setf (%parent attribute) nil)
  attribute)

(defun remove-attribute (element attribute)
  "@arg[element]{an instance of @class{element}}
   @arg[attribute]{an instance of @class{attribute}}
   @return{the attribute}
   @short{Remove an attribute node from @code{element}.}

   It is an error if @code{attribute} is not an attribute of @code{element}."
  (check-type element element)
  (check-type attribute attribute)
  (unless (eq (parent attribute) element)
    (stp-error "attempt to remove ~A from non-parent ~A" attribute element))
  (%remove-attribute attribute))

(defun find-attribute-named (element name &optional (uri ""))
  "@arg[element]{an instance of @class{element}}
   @arg[name]{string, an NCName} 
   @arg[uri]{string, a namespace URI} 
   @return{an @class{attribute} or nil}
   @short{Searches for an attribute node of @code{element} with the
     specified local name and namespace URI and returns it.}

   Returns nil if no such attribute was found."
  (find-attribute-if (of-name name uri) element))

(defun find-attribute-if (test element)
  "@arg[test]{a designator for a function of one argument.}
   @arg[element]{an instance of @class{element}}
   @return{an @class{attribute} or nil}
   @short{Searches for an attribute node of @code{element} satisfying
     @code{test}}

   Returns nil if no such attribute was found."
  (find-if test (%attributes element)))

(defun sanitize-attribute-name (element name uri urip)
  (multiple-value-bind (prefix local-name)
      (cxml::split-qname name)
    (when prefix
      (let ((uri2 (find-namespace prefix element)))
	(cond
	  ((null uri2))
	  ((not urip) (setf uri uri2))
	  ((equal uri uri2))
	  (t (stp-error "prefix ~A does not match uri ~A" prefix uri)))))
    (values local-name uri)))

(defun attribute-value (element name &optional (uri "" urip))
  "@arg[element]{an instance of @class{element}}
   @arg[name]{string, an NCName} 
   @arg[uri]{string, a namespace URI} 
   @return{a string or nil}
   @short{Searches for an attribute node of @code{element} with the
     specified local name and namespace URI and returns its value.}

   Returns nil if no such attribute was found."
  (multiple-value-bind (local-name uri)
      (sanitize-attribute-name element name uri urip)
    (let ((a (find-attribute-named element local-name uri)))
      (if a
	  (value a)
	  nil))))

(defun (setf attribute-value) (newval element name &optional (uri "" urip))
  (multiple-value-bind (local-name uri)
      (sanitize-attribute-name element name uri urip)
    (let ((a (find-attribute-named element local-name uri)))
      (if a
	  (setf (value a) newval)
	  (add-attribute element (make-attribute newval name uri)))
      newval)))

(defmacro with-attributes ((&rest entries) element &body body)
  "Evaluate body with the specified attributes bound lexically as if they
   were variables.

   Each entry in @code{entries} is a list of the form
   @em{(variable-name attribute-name &optional uri)}, where
   @code{variable-name}
   is a symbol and @code{attribute-name} and @code{uri} are strings.

   The macro with-attributes invokes @fun{attribute-value}
   to access the attributes. specified by each entry.
   Both setf and setq can be used to set the value of the attribute."
  (alexandria:once-only (element)
     `(symbol-macrolet
         ,(mapcar (lambda (entry)
                    (destructuring-bind (var name &optional (uri ""))
                       (if (and (listp entry) (cdr entry))
                            entry
                            (list entry (string-downcase
					 (princ-to-string
					  (symbol-name entry)))))
                      `(,var (attribute-value ,element ,name ,uri))))
                  entries)
        ,@body)))
(defun list-attributes (element)
  "@arg[element]{an @class{element}}
   @return{a list of @class{attribute} nodes}
   Returns a freshly consed list containing the attributes of @code{element}."
  (copy-list (%attributes element)))

(defun map-attributes (result-type fn element)
  "@arg[result-type]{a sequence type specifier, or nil}
   @arg[fn]{a designator for a function of one argument}
   @arg[element]{an instance of @class{element}}
   @return{an sequence of @code{result-type}, or nil}
   @short{Applies @code{fn} to each attribute nodes of @code{element}.}

    The @code{result-type} specifies the type of the resulting sequence.
    @code{map-children} returns nil if @code{result-type} is nil."
  (map result-type fn (%attributes element)))

(defun qualified-name (node)
  "@arg[node]{an @class{element} or @class{attribute}}
   @return{string, a QName}
   @short{Returns the node's qualified name.}
   The qualified name is computed as prefix ':' local-name.
   @see{local-name}
   @see{namespace-uri}
   @see{namespace-prefix}"
  (let ((prefix (namespace-prefix node))
	(local-name (local-name node)))
    (if (plusp (length prefix))
	(format nil "~A:~A" prefix local-name)
	local-name)))

(defun find-namespace (prefix element)
  "@arg[prefix]{a string}
   @arg[element]{an instance of @class{element}}
   @return{the namespace URI (a string), or nil}
   @short{Find the namespace @code{prefix} declared on @code{element}
   or its parent and return its namespace URI, or return nil if no such
   namespace was found.}

   This functions returns the same result as @fun{find-local-namespace}
   if the namespace is declared directly on @code{element}.  Otherwise
   it takes into account namespaces declared on parent elements."
  (setf prefix (or prefix ""))
  (cond
    ((find-local-namespace prefix element))
    ((typep (parent element) 'element)
      (find-namespace prefix (parent element)))
    ((equal prefix "")
      "")
    (t
      nil)))

(defun find-attribute-namespace (prefix element)
  (setf prefix (or prefix ""))
  (unless (equal prefix "")
    (let ((a (find prefix
		   (%attributes element)
		   :key #'namespace-prefix
		   :test #'equal)))
      (if a
	  (namespace-uri a)
	  nil))))

(defun find-local-namespace (prefix element)
  "@arg[prefix]{a string}
   @arg[element]{an instance of @class{element}}
   @return{the namespace URI (a string), or nil}
   @short{Find the namespace @code{prefix} declared on @code{element}
   and return its namespace URI, or return nil if no such namespace was found.}

   The namespaces considered by this function are: The namespace of the element
   itself.  The namespaces of element's attributes.  Extra namespaces declared
   by the element.  The \"xmlns\" namespace, which is always fixed."
  (setf prefix (or prefix ""))
  (cond
    ((equal prefix (namespace-prefix element))
      (namespace-uri element))
    ((equal prefix "xml")
     "http://www.w3.org/XML/1998/namespace")
    ((equal prefix "xmlns")
      "http://www.w3.org/2000/xmlns/")
    ((find-extra-namespace prefix element))
    (t
      (find-attribute-namespace prefix element))))

(defun namep (str)
  (and (not (zerop (length str)))
       (cxml::name-start-rune-p (elt str 0))
       (every #'cxml::name-rune-p str)))

(defun nc-name-p (str)
  (and (namep str) (cxml::nc-name-p str)))

(defun check-nc-name (str)
  (unless (nc-name-p str)
    (stp-error "not an NCName: ~A" str)))

(defgeneric (setf local-name) (newval node))
(defmethod (setf local-name) (newval (node element))
  (check-nc-name newval)
  (setf (%local-name node) newval))

(defun check-uri-like (newval)
  (declare (optimize speed (safety 0)))
  (check-type newval string)
  (when (some (lambda (c)
		(let ((code (char-code c)))
		  (or (> code 126)
		      (and (< code 32)
			   (not (eql code 9))
			   (not (eql code 10))
			   (not (eql code 13))))))
	      newval)
    (stp-error "invalid characters in URI")))

(defun (setf namespace-uri) (newval element)
  (check-type element element)
  (unless newval
    (setf newval ""))
  (unless (equal newval (%namespace-uri element))
    (check-uri-like newval)
    (if (zerop (length newval))
	(unless (zerop (length (%namespace-prefix element)))
	  (stp-error "attempt to set empty URI on element with a prefix"))
	(check-namespace-uri newval))
    (when (or (find-extra-namespace (%namespace-prefix element) element)
	      (find-attribute-namespace (%namespace-prefix element) element))
      (stp-error "cannot change element URI because of a conflicting ~
                  declaration for its prefix"))
    (when (xor (equal newval "http://www.w3.org/XML/1998/namespace")
	       (equal (%namespace-prefix element) "xml"))
      (stp-error "prefix/URI mismatch for `xml' namespace"))
    (setf (%namespace-uri element) newval))
  newval)

(defun (setf namespace-prefix) (newval element)
  (check-type element element)
  (unless newval
    (setf newval ""))
  (when (plusp (length newval))
    (check-nc-name newval))
  (let ((uri (find-local-namespace newval element)))
    (if uri
	(unless (or (equal uri (%namespace-uri element))
		    (equal newval "xml"))
	  (stp-error "conflicting declarations in namespace prefix change"))
	(when (and (equal (%namespace-uri element) "") ;not for unintialized
		   (not (zerop (length newval))))
	  (stp-error "cannot assign prefix to element in no namespace"))))
  (setf (%namespace-prefix element) newval))

(defun delete-children (parent)
  "@arg[parent]{an @class{element}}
   @return{nil}
   Deletes all children of @code{element}."
  (delete-child-if (constantly t) parent))

(defun childp (a b)
  (loop
     for node = a then (parent node)
     while node
     thereis (eq node b)))

(defmethod check-insertion-allowed ((parent element) child i)
  (check-type child node)
  (assert-orphan child)
  (typecase child
    (element
     (when (childp parent child)
       (stp-error "attempt to add a node as its own descendant")))
    ((or comment processing-instruction text))
    (t
     (stp-error "not a valid child of an element: ~A" child))))

(defmethod check-deletion-allowed ((parent element) (child node) i))

;; ;; trivial optimization
;; (defmethod replace-children
;;     ((parent element) seq &key start1 end1 start2 end2)
;;   (setf start1 (or start1 0))
;;   (setf start2 (or start2 0))
;;   (setf end1 (or end1 (length (%children parent))))
;;   (setf end2 (or end2 (length seq)))
;;   (cond
;;     ((and (eql (- start1 end1) (length (%children parent)))
;; 	  (eql start2 end2))
;;       (do-children (loser parent)
;; 	(fill-in-base-uri loser)
;; 	(setf (%parent loser) nil))
;;       (setf (fill-pointer (%children parent)) 0))
;;     (t
;;      (call-next-method)))
;;   t)

(defun add-extra-namespace (element prefix uri)
  "@arg[prefix]{string, an NCName}
   @arg[uri]{string, a namespace URI}
   @arg[element]{an instance of @class{element}}
   @return{@code{uri}}
   @short{Add an extra namespace to @code{element} that maps @code{prefix} to
   @code{uri}.}

   It is an error if the new namespace conflicts with existing namespace
   declarations on this element."
  (unless prefix (setf prefix ""))
  (unless uri (setf uri ""))
  (check-uri-like uri)
  (unless
      (cond
	((equal prefix "xmlns")
	 (unless (equal uri "")
	   (stp-error "attempt to declare `xmlns' prefix"))
	 t)
	((equal prefix "xml")
	 (unless (equal uri "http://www.w3.org/XML/1998/namespace")
	   (stp-error "incorrect URI for `xml' namespace"))
	 t)
	((equal uri "http://www.w3.org/XML/1998/namespace")
	 (stp-error "incorrect prefix for `xml' namespace")))
    (cond
      ((plusp (length prefix))
        (check-nc-name prefix)
        (check-namespace-uri uri))
      ((plusp (length uri))
        (check-namespace-uri uri)))
    (let ((old (find-local-namespace prefix element)))
      (when (and old (not (equal old uri)))
	(stp-error "extra namespace conflicts with existing declarations")))
    (unless (%namespaces element)
      (setf (%namespaces element) (make-hash-table :test 'equal)))
    (setf (gethash prefix (%namespaces element)) uri)
    uri))

(defun remove-extra-namespace (element prefix)
  "@arg[prefix]{string, an NCName}
   @arg[element]{an instance of @class{element}}
   @return{@code{uri}}
   Removed the extra namespace declared on @code{element} for @code{prefix}."
  (when (%namespaces element)
    (remhash (or prefix "") (%namespaces element))))

(defun collect-local-namespaces (element)
  ;; zzz ERH optimiert das noch fuer den fall nur eines ergebnisses
  (let ((result (if (%namespaces element)
		    (alexandria:copy-hash-table (%namespaces element))
		    (make-hash-table :test 'equal))))
    (setf (gethash (%namespace-prefix element) result)
	  (%namespace-uri element))
    (dolist (a (%attributes element))
      (when (plusp (length (namespace-prefix a)))
	(setf (gethash (namespace-prefix a) result) (namespace-uri a))))
    result))

(defmethod serialize ((node element) handler)
  (let ((uri (%namespace-uri node))
	(local-name (%local-name node))
	(qname (qualified-name node))
	(attrs (mapcar (lambda (a)
			 (sax:make-attribute
			  :namespace-uri (namespace-uri a)
			  :local-name (local-name a)
			  :qname (qualified-name a)
			  :value (value a)))
		       (%attributes node)))
	(element-parent
	 (when (typep (parent node) 'element)
	   (parent node))))
    (maphash (lambda (prefix uri)
	       (unless (equal prefix "xml")
		 (let ((upper (when element-parent
				(find-namespace prefix element-parent))))
		   (unless (or (equal upper uri)
			       (and (null upper) (zerop (length uri))))
		     (push (if (plusp (length prefix))
			       (sax:make-attribute 
				:namespace-uri "http://www.w3.org/2000/xmlns/"
				:local-name prefix
				:qname (concatenate 'string "xmlns:" prefix)
				:value uri)
			       (sax:make-attribute 
				:namespace-uri "http://www.w3.org/2000/xmlns/"
				:local-name "xmlns"
				:qname "xmlns"
				:value uri))
			   attrs)))))
	     (collect-local-namespaces node))
    (sax:start-element handler uri local-name qname attrs)
    (map nil (lambda (x) (serialize x handler)) (%children node))
    (sax:end-element handler uri local-name qname)))

(defmethod (setf base-uri) (newval (node element))
  (setf (%base-uri node) newval))

(defun escape-uri (string)
  (with-output-to-string (out)
    (loop for c across (cxml::rod-to-utf8-string string) do
	  (let ((code (char-code c)))
	    ;; http://www.w3.org/TR/xlink/#link-locators
	    (if (or (>= code 127) (<= code 32) (find c "<>\"{}|\\^`"))
		(format out "%~2,'0X" code)
		(write-char c out))))))

(defmethod base-uri ((node element))
  (let ((xml-base
	 (or (attribute-value node
			      "base"
			      "http://www.w3.org/XML/1998/namespace")
	     (%base-uri node)))
	(parent (parent node)))
    (if parent
	(puri:render-uri (puri:merge-uris xml-base (base-uri parent)) nil)
	xml-base)))

;;; below a literal translation of XOM's Java code for BASE-URI.
;;; Unfortunately I don't understand a word of what's going on here, hence
;;; the trivial definition above instead.

;;;(defmethod base-uri ((node element))
;;;  (let ((defaults "")
;;;	(relative-uri (%base-uri node)))
;;;    (loop
;;;       for n = node then (parent node)
;;;       while n
;;;       do
;;;	 (let ((%base-uri (%base-uri n)))
;;;	   (when (and (plusp (length relative-uri))
;;;		      (not (equals relative-uri %base-uri)))
;;;	     (return (merge-uris relative-uri defaults)))
;;;	   (when (typep n 'document)
;;;	     (return (merge-uris %base-uri defaults)))
;;;	   (let ((xml-base (attribute-value
;;;			    n
;;;			    "base"
;;;			    "http://www.w3.org/XML/1998/namespace")))
;;;	     (when xml-base
;;;	       (setf xml-base (escape-uri xml-base))
;;;	       (cond
;;;		 ((zerop (length xml-base))
;;;		   (setf defaults (get-entity-uri node)))
;;;		 (t
;;;		   (cond
;;;		     ((zerop (length defaults))
;;;		       (setf defaults xml-base))
;;;		     (...isopaque...
;;;		      (return defaults))
;;;		     (t
;;;		      (setf defaults (merge-uris xml-base defaults))))
;;;		   (when (isabsolute xml-base)
;;;		     (return defaults)))))))
;;;       finally				;parent is null
;;;	 (return (merge-uris %base-uri defaults)))))

(defmethod string-value ((node element))
  (with-output-to-string (s)
    (labels ((recurse (x)
	       (do-children (child x)
		 (typecase child
		   ((or comment processing-instruction))
		   (text (write-string (string-value child) s))
		   (element (recurse child))))))
      (recurse node))))


;;; printing

(defun non-empty-string (x)
  (plusp (length x)))

(defmethod slots-for-print-object append ((node named-node-mixin))
  '((:local-name local-name)
    (:namespace-prefix namespace-prefix non-empty-string)
    (:namespace-uri namespace-uri non-empty-string)))

(defun attributes-for-print (elt)
  (sort (list-attributes elt) #'string< :key #'qualified-name))

(defmethod slots-for-print-object append ((node element))
  '((:attributes attributes-for-print identity)
    (:extra-namespaces namespaces-for-print identity)))

(defun namespaces-for-print (element)
  (when (%namespaces element)
    (loop
       for prefix being each hash-key in (%namespaces element)
       using (hash-value uri)
       collect `(,prefix ,uri))))

(defreader named-node-mixin (local-name
			     (namespace-prefix "")
			     (namespace-uri ""))
  (setf (%local-name this) local-name)
  (setf (%namespace-prefix this) namespace-prefix)
  (setf (%namespace-uri this) namespace-uri))

(defreader element ((attributes nil) (extra-namespaces nil))
  (dolist (a attributes)
    (add-attribute this a))
  (loop for (prefix uri) in extra-namespaces do
       (add-extra-namespace this prefix uri)))
