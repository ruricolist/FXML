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

(defvar *check-uri-syntax* nil
  "If true (the default), a warning is issued if a string specified
   as a namespace URI does not have URI syntax.")
(defun check-namespace-uri (uri)
  (when (and *check-uri-syntax*
	     (not (or (search "://" uri)
		      (eql 4 (mismatch "uri:" uri))
		      (eql 4 (mismatch "urn:" uri)))))
    (warn "namespace URI does not look like an absolute URL: ~S" uri)))

(define-condition stp-error (simple-error)
  ()
  (:documentation "The class of all STP errors."))

(defun stp-error (fmt &rest args)
  "@unexport{}"
  (error 'stp-error :format-control fmt :format-arguments args))


;;;; Class NODE

(defgeneric string-value (node)
  (:documentation
   "@arg[node]{an instance of @class{node}}
    @return{a string}
    @short{Returns the string value of @code{node} as defined by XPath.}

    For a document, this is the value of its root element.

    For an element, the concatenation of the values of those child nodes
    is returned that are elements or text nodes.
    (Leaving only the PCDATA content.)

    For a text, comment, and processing instruction nodes, the node's data
    is returned.

    For an attribute, the attribute value is returned.

    The value for document types is not specified."))

(defgeneric parent (node)
  (:documentation
   "@arg[node]{an @class{node}}
    @return{the parent node, or nil}
    @short{Returns the node's parent.}"))

(defgeneric base-uri (node)
  (:documentation
   "@arg[node]{an @class{node}}
    @return{a string}
    @short{Returns the node's base URI.}"))

(defun cxml-stp:document (node)
  "@arg[node]{an instance of @class{node}}
   @return{a @class{document} or nil}
   @short{Returns the document node ancestor of @code{node}.}

   Returns the @class{document} node that is the @fun{root} of @code{node}
   or @code{nil} if the root node is not a document."
  (check-type node node)
  (loop
     for parent = node then (parent parent)
     while (and parent (not (typep parent 'cxml-stp:document)))
     finally (return parent)))

(defun root (node)
  "@arg[node]{an instance of @class{node}}
   @return{a @class{node} or nil}
   @short{Returns the root of the tree of nodes @code{node} is part of.}

   In a complete document, this is an instance of @class{document}, but
   a detached subtree can have any node as its root.  In particular, the
   argument itself is returned if it does not have a @fun{parent}."
  (check-type node node)
  (loop
     for p = (parent node) then (parent p)
     and q = node then p
     while p
     finally (return q)))

;; (defgeneric base-uri (node)) ;fixme: hier muessen wir wissen, ob specified
;; (defmethod base-uri ((node node))
;;   (let ((parent (parent node)))
;;     (if parent
;;         (base-uri parent)
;;         "")))

(defgeneric detach (node)
  (:documentation
   "@arg[node]{a @class{node}}
    @short{This function removes a child node or attribute.}

    In contrast to functions for child nodes, this function can also remove
    an attribute from its parent.

    @see{parent}"))
(defmethod detach ((node node))
  (when (parent node)
    (delete-child node (parent node))))

(defgeneric copy (node)
  (:documentation
   "@arg[node]{a @class{node}}
    @short{This function copies a node recursively.}

    The resulting node is of the same class as the argument, and all
    child nodes and attributes are copied in the same way.

    Shared structure includes only primitive slot values like strings.
    (The consequences are undefined if user code mutates such values, whether
    @code{copy} is used or not.)"))

(defgeneric serialize (node handler)
  (:documentation
   "@arg[node]{a @class{node}}
    @short{This function generates SAX events representing @code{node}.}

    Use this function together with a serialization sink to generate
    a serialized XML document.

    Examples. Serializing to a stream:
    @begin{pre}CL-USER> (stp:serialize (stp:make-document (stp:make-element \"test\"))
			(cxml:make-character-stream-sink *standard-output*))
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<test/>
#<SWANK-BACKEND::SLIME-OUTPUT-STREAM {10037EA611@}>
@end{pre}
    Examples. Serializing to a string:
    @begin{pre}CL-USER> (stp:serialize (stp:make-document (stp:make-element \"test\"))
			(cxml:make-string-sink))
\"<?xml version=\\\"1.0\\\" encoding=\\\"UTF-8\\\"?>
<test/>\"
@end{pre}

   @see{make-builder}"))

;;; CHILDREN-related convenience functions

(defgeneric map-children (result-type function node)
  (:documentation
   "@arg[result-type]{a sequence type specifier, or nil}
    @arg[function]{a designator for a function of one argument}
    @arg[node]{a @class{node}}
    @return{an sequence of @code{result-type}, or nil}
    @short{Applies @code{function} to successive child nodes.}

    The @code{result-type} specifies the type of the resulting sequence.
    @code{map-children} returns nil if @code{result-type} is nil.  Otherwise
    it returns a sequence such that element i is the result of applying
    @code{function} to child i of @class{node}."))

(defmacro do-children ((var node &optional result) &body body)
  "@arg[var]{symbol, a variable name}
   @arg[node]{a @class{node}}
   @arg[result]{a form}
   @return{the result of evaluating @code{result}}
   Executes @code{body} with @code{var} bound to successive child
     nodes."
  `(block nil
     (map-children nil (lambda (,var) ,@body) ,node)
     (let (,var)
       (declare (ignorable ,var))
       ,result)))

(defun list-children (node)
  "@arg[node]{a @class{node}}
   @return{a list of nodes}
   Returns a freshly consed list containing the child nodes of @code{node}."
  (map-children 'list #'identity node))

(defun nth-child (n parent)
  "@arg[n]{a non-negative integer}
   @arg[parent]{a @class{node}}
   @return{a @class{node}}
   @short{Returns child node @code{n} of @code{parent}}, or signals an error
   if n is negative or as large or larger that the number of child nodes."
  (elt (%children parent) n))

(defun first-child (node)
  "@arg[node]{a @class{node}}
   @return{a @class{node} or nil}
   Returns first child of @code{node}, or nil."
  (let ((c (%children node)))	   ;VECTOR or NIL, but not arbitrary list
    (when (plusp (length c))
      (elt c 0))))

(defun last-child (node)
  "@arg[node]{a @class{node}}
   @return{a @class{node} or nil}
   Returns last child of @code{node}, or nil."
  (let* ((c (%children node))	   ;VECTOR or NIL, but not arbitrary list
	 (l (length c)))
    (when (plusp l)
      (elt c (1- l)))))

(defun previous-sibling (node)
  "@arg[node]{a @class{node}}
   @return{a @class{node} or nil}
   @short{Returns the child preceding @code{node} in the child list of its
     parent.}

   Signals an error if @code{node} has no parent or is the first child of its
   parent."
  (let ((p (parent node)))
    (unless p
      (stp-error "node has no parent"))
    (let ((idx (1- (child-position node p))))
      (when (minusp idx)
	(stp-error "node has no previous sibling"))
      (nth-child idx p))))

(defun next-sibling (node)
  "@arg[node]{a @class{node}}
   @return{a @class{node} or nil}
   @short{Returns the child following @code{node} in the child list of its
     parent.}

   Signals an error if @code{node} has no parent or is the last child of its
   parent."
  (let ((p (parent node)))
    (unless p
      (stp-error "node has no parent"))
    (let ((idx (1+ (child-position node p)))
	  (c (%children p)))
      (when (eql idx (length c))
	(stp-error "node has no next sibling"))
      (nth-child idx p))))

(defun number-of-children (parent)
  "@arg[parent]{a @class{node}}
   @return{the number of child nodes}
   Returns the number of @code{parent}'s child nodes.
   @see{count-children}"
  (length (%children parent)))

(defun count-children
    (value parent &rest args &key from-end (start 0) end key test)
  "@arg[value]{an object}
   @arg[parent]{a @class{node}}
   @arg[from-end]{a generalized boolead}
   @arg[start, end]{bounding index designators for @code{parent}'s child list}
   @arg[key]{a designator for a function of one argument, or nil}
   @arg[test]{a designator for a function of two arguments, or nil}
   @return{a non-negative integer less than or equal to the number of
     child nodes}
   Counts (and returns the number of) @code{parent}'s child nodes satisfying
   the test.
   @see{number-of-children}
   @see{count-children-if}"
  (declare (ignore from-end start end key test))
  (apply #'count value (%children parent) args))

(defun count-children-if
    (predicate parent &rest args &key from-end (start 0) end key)
  "@arg[predicate]{a designator for a function of one argument that returns
     a generalized boolean}
   @arg[parent]{a @class{node}}
   @arg[from-end]{a generalized boolead}
   @arg[start, end]{bounding index designators for @code{parent}'s child list}
   @arg[key]{a designator for a function of one argument, or nil}
   @return{a non-negative integer less than or equal to the number of
     child nodes}
   Counts (and returns the number of) @code{parent}'s child nodes satisfying
   @code{predicate}.
   @see{number-of-children}
   @see{count-children}"
  (declare (ignore from-end start end key))
  (apply #'count-if predicate (%children parent) args))

(defun find-child
    (value parent &rest args &key from-end (start 0) end key test)
  "@arg[value]{an object}
   @arg[parent]{a @class{node}}
   @arg[from-end]{a generalized boolead}
   @arg[start, end]{bounding index designators for @code{parent}'s child list}
   @arg[key]{a designator for a function of one argument, or nil}
   @arg[test]{a designator for a function of two arguments, or nil}
   @return{a @class{node} or nil}
   Searches for a child node of @code{parent} that satisfies the @code{test}
   and returns it.

   @see{find-child-if}"
  (declare (ignore from-end start end key test))
  (apply #'find value (%children parent) args))

(defun find-child-if
    (predicate parent &rest args &key from-end (start 0) end key)
  "@arg[predicate]{a designator for a function of one argument that returns
     a generalized boolean}
   @arg[parent]{a @class{node}}
   @arg[from-end]{a generalized boolead}
   @arg[start, end]{bounding index designators for @code{parent}'s child list}
   @arg[key]{a designator for a function of one argument, or nil}
   @return{a @class{node} or nil}
   Searches for a child node of @code{parent} that satisfies @code{predicate}
   and returns it.

   @see{find-child}"
  (declare (ignore from-end start end key))
  (apply #'find-if predicate (%children parent) args))

(defun child-position
    (value parent &rest args &key from-end (start 0) end key test)
  "@arg[value]{an object}
   @arg[parent]{a @class{node}}
   @arg[from-end]{a generalized boolead}
   @arg[start, end]{bounding index designators for @code{parent}'s child list}
   @arg[key]{a designator for a function of one argument, or nil}
   @arg[test]{a designator for a function of two arguments, or nil}
   @return{a @class{node} or nil}
   Searches for a child node of @code{parent} that satisfies the @code{test}
   and returns its position.

   @see{child-position-if}"
  (declare (ignore from-end start end key test))
  (apply #'position value (%children parent) args))

(defun child-position-if
    (predicate parent &rest args &key from-end (start 0) end key)
  "@arg[predicate]{a designator for a function of one argument that returns
     a generalized boolean}
   @arg[parent]{a @class{node}}
   @arg[from-end]{a generalized boolead}
   @arg[start, end]{bounding index designators for @code{parent}'s child list}
   @arg[key]{a designator for a function of one argument, or nil}
   @arg[test]{a designator for a function of two arguments, or nil}
   @return{a @class{node} or nil}
   Searches for a child node of @code{parent} that satisfies the @code{test}
   and returns its position.

   @see{child-position}"
  (declare (ignore from-end start end key))
  (apply #'position-if predicate (%children parent) args))

(defun filter-children
    (predicate parent &rest args &key from-end (start 0) end count key)
  "@arg[predicate]{a designator for a function of one argument that returns
     a generalized boolean}
   @arg[parent]{a @class{node}}
   @arg[from-end]{a generalized boolead}
   @arg[start, end]{bounding index designators for @code{parent}'s child list}
   @arg[key]{a designator for a function of one argument, or nil}
   @arg[test]{a designator for a function of two arguments, or nil}
   @arg[count]{an integer or nil}
   @return{a sequence containing nodes}
   @short{Return a list of child nodes of @code{parent} from which nodes that
     do not satisfy @code{predicate} have been removed.}

   This function returns the same list as @code{remove-if-not} on the result
   of @fun{list-children}."
  (declare (ignore from-end start end count key))
  (apply #'remove-if-not predicate (list-children parent) args))

(defun map-recursively (fn node)
  "@arg[fn]{a designator for a function of one argument}
   @arg[node]{a @class{node}}
   @return{nil}
   Applies @code{fn} to successive descendants of @code{node} in
   pre-order."
  (funcall fn node)
  (map nil
       (lambda (c) (map-recursively fn c))
       (%children node)))

(defmacro do-recursively ((var node &optional result) &body body)
  "@arg[var]{symbol, a variable name}
   @arg[node]{a @class{node}}
   @arg[result]{a form}
   @return{the result of evaluating @code{result}}
   Executes @code{bode} with @code{var} bound to successive descendants of
   @code{node} in pre-order."
  `(block nil
     (map-recursively (lambda (,var) ,@body) ,node)
     (let (,var)
       (declare (ignorable ,var))
       ,result)))

(defun find-recursively (item node &key key test)
  "@arg[item]{an object}
   @arg[node]{a @class{node}}
   @arg[key]{a designator for a function of one argument, or nil}
   @arg[test]{a designator for a function of two arguments, or nil}
   @return{a @class{node} or nil}
   Searches in pre-order for the first descendant of @code{node} that
   satisfies the @code{test} and returns it.

   @see{find-child-if}"
  (setf key (or key #'identity))
  (setf test (or test #'eql))
  (do-recursively (child node)
    (when (funcall test item (funcall key child))
      (return child))))

(defun find-recursively-if (predicate node &key key)
  "@arg[test]{a designator for a function of one argument that returns
     a generalized boolean}
   @arg[node]{a @class{node}}
   @arg[key]{a designator for a function of one argument, or nil}
   @return{a @class{node} or nil}
   Searches in pre-order for the first descendant of @code{node} that
   satisfies the @code{test} and returns it.

   @see{find-child-if}"
  (setf key (or key #'identity))
  (do-recursively (child node)
    (when (funcall predicate (funcall key child))
      (return child))))

(defun filter-recursively (test node &key key)
  "@arg[test]{a designator for a function of one argument that returns
     a generalized boolean}
   @arg[node]{a @class{node}}
   @arg[key]{a designator for a function of one argument, or nil}
   @arg[test]{a designator for a function of two arguments, or nil}
   @return{a sequence containing nodes}
   Return a list of descendant nodes of @code{node} in pre-order, from which
   nodes that do not satisfy @code{predicate} have been removed."
  (setf key (or key #'identity))
  (setf test (or test #'eql))
  (let ((result '()))
    (do-recursively (child node)
      (when (funcall test (funcall key child))
	(push child result)))
    (nreverse result)))


;;; tbd

;;; (defun query (node xpath)
;;;   ;; fixme
;;;   )


;;;; PRINT-OBJECT

(defgeneric slots-for-print-object (node)
  (:method-combination append))

(defun maybe-uri->string (thing)
  (if (puri:uri-p thing)
      (princ-to-string thing)
      (non-empty-string thing)))

(defmethod slots-for-print-object append ((node parent-node))
  '((:base-uri %base-uri maybe-uri->string)
    (:children list-children identity)))

(defmethod print-object ((object node) stream)
  (when (and *print-readably* (not *read-eval*))
    (error "cannot print STP nodes readably without *read-eval*"))
  ;; zzz pretty printing on clisp introduces spurious closing parens
  (if (and *print-pretty* #+clisp nil)
      (pretty-print-node object stream)
      (ugly-print-node object stream)))

(defun pretty-print-node (node stream)
  (let* ((slots (mapcan (lambda (spec)
			  (destructuring-bind (key fn &optional test) spec
			    (let ((value (funcall fn node)))
			      (when (or (null test) (funcall test value))
				(list (list key value))))))
			(slots-for-print-object node)))
	 (constructor
	  (intern (symbol-name (class-name (class-of node))) :cxml-stp-impl))
	 (level *print-level*)
	 (length *print-length*)
	 (*print-level* nil)
	 (*print-length* nil))
    (pprint-logical-block (stream nil :prefix "#.(" :suffix ")")
      (write constructor :stream stream)
      (when (parent node)
	(write-char #\space stream)
	(pprint-newline :linear stream)
	(pprint-pop)
	(format stream "#| ~S of type ~A |#"
		:parent
		(type-of (parent node))))
      (let ((remaining-slots slots))
        (when remaining-slots
          (write-char #\space stream)
          (pprint-newline :linear stream)
          (loop
	     (pprint-pop)
	     (destructuring-bind (key value) (pop remaining-slots)
	       (write key :stream stream)
	       (write-char #\space stream)
	       (pprint-newline :miser stream)
	       (let ((*print-level* level)
		     (*print-length* length))
		 (unless (typep value '(or string null))
		   (write-char #\' stream))
		 (write value :stream stream))
	       (when (null remaining-slots)
		 (return))
	       (write-char #\space stream)
	       (pprint-newline :linear stream))))))))

(defun ugly-print-node (node stream)
  (let* ((slots (mapcan (lambda (spec)
			  (destructuring-bind (key fn &optional test) spec
			    (let ((value (funcall fn node)))
			      (when (or (null test) (funcall test value))
				(list (list key value))))))
			(slots-for-print-object node)))
	 (constructor
	  (intern (symbol-name (class-name (class-of node))) :cxml-stp-impl))
	 (level *print-level*)
	 (length *print-length*)
	 (*print-level* nil)
	 (*print-length* nil))
    (write-string "#.(" stream)
    (write constructor :stream stream)
    (let ((remaining-slots slots))
      (when remaining-slots
	(write-char #\space stream)
	(loop
	   (destructuring-bind (key value) (pop remaining-slots)
	     (write key :stream stream)
	     (write-char #\space stream)
	     (let ((*print-level* level)
		   (*print-length* length))
	       (unless (typep value '(or string null))
		 (write-char #\' stream))
	       (write value :stream stream))
	     (when (null remaining-slots)
	       (return))
	     (write-char #\space stream)))))
    (write-string ")" stream)))

(defgeneric reconstruct (node &key &allow-other-keys)
  (:method-combination progn))

(defmacro defreader (name (&rest args) &body body)
  (let ((fn (intern (symbol-name name) :cxml-stp-impl)))
    `(progn
       (defun ,fn (&rest keys)
	 "@unexport{}"
	 (let ((result (make-instance ',name)))
	   (apply #'reconstruct result keys)
	   result))
       (defmethod reconstruct
	   progn
	   ((this ,name)
	    &key ,@(loop
		      for arg in args
		      collect (if (symbolp arg)
				  `(,arg (error "slot ~A missing in printed representation"
						',arg))
				  arg))
	    &allow-other-keys)
	 ,@body))))
