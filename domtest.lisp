#+(or)
(defpackage :domtest
  (:use :cl :xml)
  (:alias (:string-dom :dom)))
(defpackage :domtest-tests
  (:use))
(in-package :domtest)

(defparameter *directory* "~/src/2001/DOM-Test-Suite/")


;;;; allgemeine Hilfsfunktionen

(defmacro string-case (keyform &rest clauses)
  (let ((key (gensym "key")))
    `(let ((,key ,keyform))
       (declare (ignorable ,key))
       (cond
	 ,@(loop
	       for (keys . forms) in clauses
	       for test = (etypecase keys
			    (string `(string= ,key ,keys))
			    (sequence `(find ,key ,keys :test 'string=))
			    ((eql t) t))
	       collect
		 `(,test ,@forms))))))

(defun rcurry (function &rest args)
  (lambda (&rest more-args)
    (apply function (append more-args args))))

(defmacro for ((&rest clauses) &rest body-forms)
  `(%for ,clauses (progn ,@body-forms)))

(defmacro for* ((&rest clauses) &rest body-forms)
  `(%for* ,clauses (progn ,@body-forms)))

(defmacro %for ((&rest clauses) body-form &rest finally-forms)
  (for-aux 'for clauses body-form finally-forms))

(defmacro %for* ((&rest clauses) body-form &rest finally-forms)
  (for-aux 'for* clauses body-form finally-forms))

(defmacro for-finish ()
  '(loop-finish))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun for-aux (kind clauses body-form finally-forms)
    ` (loop ,@ (loop for firstp = t then nil
		   for %clauses = clauses then (rest %clauses)
		   for clause = (first %clauses) then (first %clauses)
		   while (and %clauses (listp clause))
		   append (cons (ecase kind
				  (for (if firstp 'as 'and))
				  (for* 'as))
				(if (= 2 (length clause))
				    (list (first clause) '= (second clause))
				    clause))
		   into result
		   finally (return (append result %clauses)))
	  do (progn ,body-form)
	  finally (progn ,@finally-forms))))


;;;; spezielle Hilfsfunktionen

(defmacro with-attributes ((&rest attributes) element &body body)
  (let ((e (gensym "element")))
    `(let* ((,e ,element)
            ,@(mapcar (lambda (var)
                       `(,var (dom:get-attribute ,e ,(symbol-name var))))
                     attributes))
       ,@body)))

(defun map-child-elements (result-type fn element &key name)
  (remove '#1=#:void
          (map result-type
            (lambda (node)
              (if (and (eq (dom:node-type node) :element)
                       (or (null name)
                           (equal (dom:tag-name node) name)))
                  (funcall fn node)
                  '#1#))
            (dom:child-nodes element))))

(defmacro do-child-elements ((var element &key name) &body body)
  `(block nil
     (map-child-elements nil (lambda (,var) ,@body) ,element :name ,name)))

(defun find-child-element (name element)
  (do-child-elements (child element :name name)
    (return child)))

(defun %intern (name)
  (intern name :domtest-tests))

(defun replace-studly-caps (str)
  ;; s/([A-Z][a-z])/-\1/
  (with-output-to-string (out)
    (with-input-from-string (in str)
      (for ((first = t :then nil)
            (c = (read-char in nil nil))
            (next = (peek-char nil in nil nil))
            :while c)
          (when (and (not first) (upper-case-p c) next (lower-case-p next))
            (write-char #\- out))
        (write-char (char-downcase c) out)))))

(defun intern-dom (name)
  (intern (replace-studly-caps name) :dom))

(defun child-elements (element)
  (map-child-elements 'list #'identity element))

(defun parse-java-literal (str)
  (cond
    ((null str) nil)                    ;?
    ((equal str "true")
      t)
    ((equal str "false")
      nil)
    ((digit-char-p (char str 0))
      (parse-integer str))
    ((char= (char str 0) #\")
      (let ((end (1- (length str))))
        (assert (char= (char str end) #\"))
        (subseq str 1 end)))
    (t
      (%intern str))))

(defmacro maybe-setf (place form)
  (if place
      `(setf ,place ,form)
      form))


;;;; dom1-interfaces.xml auslesen

(defvar *methods* '())
(defvar *fields* '())

(defun read-members (&optional (directory *directory*))
  (let* ((pathname (merge-pathnames "patches/dom1-interfaces.xml" directory))
         (library (dom:document-element (xml:parse-file pathname)))
         (methods '())
         (fields '()))
    (do-child-elements (interface library :name "interface")
      (do-child-elements (method interface :name "method")
        (let ((parameters (find-child-element "parameters" method)))
          (push (cons (dom:get-attribute method "name")
                      (map-child-elements 'list
                                          (rcurry #'dom:get-attribute "name")
                                          parameters
                                          :name "param"))
                methods)))
      (do-child-elements (attribute interface :name "attribute")
        (push (dom:get-attribute attribute "name") fields)))
    (values methods fields)))


;;;; Conditions uebersetzen

(defun translate-condition (element)
  (string-case (dom:tag-name element)
    ("equals" (translate-equals element))
    ("contentType" (translate-content-type element))
    ("implementationAttribute" (assert-have-implementation-attribute element))
    ("isNull" (translate-is-null element))
    ("not" (translate-is-null element))
    ("notNull" (translate-not-null element))
    ("same" (translate-same element))
    (t (error "unknown condition: ~A" element))))

(defun translate-equals (element)
  (with-attributes (|actual| |expected| |ignoreCase|) element
    `(,(if (parse-java-literal |ignoreCase|) 'string-equal 'string=)
         ,(%intern actual)
         ,(parse-java-literal expected))))

(defun translate-same (element)
  (with-attributes (|actual| |expected|) element
    `(eql ,(%intern actual) ,(parse-java-literal expected))))

(defun translate-instance-of (element)
  (with-attributes (|obj| |type|) element
    `(typep ,(%intern |obj|) ,(intern-dom |type|))))

(defun translate-is-null (element)
  (with-attributes (|obj|) element
    `(null ,(%intern |obj|))))

(defun translate-not-null (element)
  (with-attributes (|obj|) element
    (%intern |obj|)))

(defun translate-content-type (element) ;XXX verstehe ich nicht
  (with-attributes (|type|) element 
   `(equal ,(parse-java-literal |type|) "text/xml")))

(defun translate-uri-equals (element)
  (with-attributes
      (|actual|
       |scheme| |path| |host| |file| |name| |query| |fragment| |isAbsolute|)
      element
    |isAbsolute|
   `(let ((uri ,(%intern |actual|)))
      (and (string-equalp ,|scheme| (net.uri:uri-scheme uri))
           (equal ,|host| (net.uri:uri-host uri))
           (equal ,|path| (net.uri:uri-path uri))
           (equal ,|file| "???")
           (equal ,|name| "???")
           (equal ,|query| (net.uri:uri-query uri))
           (equal ,|fragment| (net.uri:uri-fragment uri))
           ;; isabsolute
           nil))))


;;;; Statements uebersetzen

(defun translate-statement (element)
  (string-case (dom:tag-name element)
    ("append" (translate-append element))
    ("assertDOMException" (translate-assert-domexception element))
    ("assertEquals"	(translate-assert-equals element))
    ("assertNotNull"	(translate-assert-not-null element))
    ("assertInstanceOf"	(translate-assert-instance-of element))
    ("assertNull"	(translate-assert-null element))
    ("assertSame"	(translate-assert-same element))
    ("assertSize"	(translate-assert-size element))
    ("assertTrue"	(translate-assert-true element))
    ("assertFalse"	(translate-assert-true element))
    ("assertURIEquals"	(translate-assert-uri-equals element))
    ("for-each"		(translate-for-each element))
    ("fail"		(translate-fail element))
    ("if"		(translate-if element))
    ("increment"	(translate-unary-assignment '+ element))
    ("decrement"	(translate-unary-assignment '- element))
    ("load"		(translate-load element))
    ("plus"		(translate-binary-assignment '+ element))
    ("try"		(translate-try element))
    ("while"		(translate-while element))
    (t			(translate-member element))))

(defun translate-binary-assignment (fn element)
  (with-attributes (|var| |op1| |op2|) element
    `(maybe-setf ,(%intern |var|) (,fn ,(%intern |op1|) ,(%intern |op2|)))))

(defun translate-unary-assignment (fn element)
  (with-attributes (|var| |value|) element
    `(maybe-setf ,(%intern |var|)
                 (,fn ,(%intern |var|) ,(parse-java-literal |value|)))))

(defun translate-load (load)
  (with-attributes (|var| |href| |willBeModified|) load
    `(maybe-setf ,(%intern |var|)
                 (load-file ,|href| ,(parse-java-literal |willBeModified|)))))

(defun translate-call (call method)
  (let ((name (car method))
        (args (mapcar (lambda (name)
                        (parse-java-literal (dom:get-attribute call name)))
                      (cdr method))))
    (with-attributes (|var| |obj|) call
      `(maybe-setf ,(%intern |var|) (,(intern-dom name) ,|obj| ,@args)))))

(defun translate-get (call name)
  (with-attributes (|var| |obj|) call
    `(maybe-setf ,(%intern |var|) (,(intern-dom name) ,|obj|))))

(defun translate-fail (element)
  (declare (ignore element))
  `(error "failed"))

(defun translate-member (element)
  (let* ((name (dom:tag-name element))
         (method (find name *methods* :key #'car :test #'equal))
         (field (find name *fields* :test #'equal)))
    (cond
      (method (translate-call element method))
      (field (translate-get element field))
      (t (error "unknown element ~A" element)))))

(defun translate-assert-equals (element)
  `(assert ,(translate-equals element)))

(defun translate-assert-same (element)
  `(assert ,(translate-same element)))

(defun translate-assert-null (element)
  (with-attributes (|actual|) element
    `(assert (null ,(%intern |actual|)))))

(defun translate-assert-not-null (element)
  (with-attributes (|actual|) element
    `(assert ,(%intern |actual|))))

(defun translate-assert-size (element)
  (with-attributes (|collection| |size|) element
    `(assert (eql (length ,(%intern |collection|)) ,(%intern |size|)))))

(defun translate-assert-instance-of (element)
  `(assert ,(translate-instance-of element)))

(defun translate-if (element)
  (destructuring-bind (condition &rest rest)
      (child-elements element)
    (let (then else)
      (dolist (r rest)
        (when (equal (dom:tag-name r) "else")
          (setf else (child-elements r))
          (return))
        (push r then))
      `(cond
         (,(translate-condition condition)
           ,@(mapcar #'translate-statement (reverse then)))
         (t
           ,@(mapcar #'translate-statement else))))))

(defun translate-while (element)
  (destructuring-bind (condition &rest body)
      (child-elements element)
    `(loop
         while ,(translate-condition condition)
         do (progn ,@(mapcar #'translate-statement body)))))

(defun translate-assert-domexception (element)
  (do-child-elements (c element)
    (unless (equal (dom:tag-name c) "metadata")
      (return
        `(progn
           ,@(translate-body c)
           ;; XXX haben noch keine Exceptions
           (error "expected exception ~A" (dom:tag-name element)))))))

(defun translate-try (element)
  (map-child-elements 'list
                      (lambda (c)
                        (if (equal (dom:tag-name c) "catch")
                            nil
                            (translate-statement c)))
                      element)
  ;; XXX haben noch keine Exceptions
  )

(defun translate-append (element)
  (with-attributes (|collection| |item|) element
    (let ((c (%intern |collection|))
          (i (%intern |item|)))
      `(maybe-setf ,c (append ,c (list ,i))))))

(defun translate-assert-true (element)
  (with-attributes (|actual|) element
    `(assert ,(%intern |actual|))))

(defun translate-assert-false (element)
  (with-attributes (|actual|) element
    `(assert (not ,(%intern |actual|)))))

(defun translate-assert-uri-equals (element)
  `(assert ,(translate-uri-equals element)))


;;;; Tests uebersetzen

(defun translate-body (element)
  (map-child-elements 'list #'translate-statement element))

(defun translate-for-each (element)
  (with-attributes (|collection| |member|) element
    `(dolist (,(%intern |member|) ,(%intern |collection|))
       ,@(translate-body element))))

(defun test (name &optional (directory *directory*))
  (let* ((test-directory (merge-pathnames "tests/level1/core/" directory)))
    (slurp-test
     (make-pathname :name name :type "xml" :defaults test-directory))))

(defun assert-have-implementation-attribute (element)
  (string-case (dom:get-attribute element "name")
    (t
      (warn "implementationAttribute ~A not supported, skipping test"
            (dom:get-attribute element "name"))
      (throw 'give-up nil))))

(defun slurp-test (pathname)
  (unless *fields*
    (multiple-value-setq (*methods* *fields*) (read-members *directory*)))
  (catch 'give-up
    (let* ((test (dom:document-element (xml:parse-file pathname)))
           title
           (variables '())
           (code '()))
      (do-child-elements (e test)
        (string-case (dom:tag-name e)
          ("metadata"
            (let ((title-element (find-child-element "title" e)))
              (setf title (dom:data (dom:first-child title-element)))))
          ("var"
            (push (%intern (dom:get-attribute e "name")) variables))
          ("implementationAttribute"
            (assert-have-implementation-attribute e))
          (t
            (push (translate-statement e) code))))
      `(defun ,(%intern (concatenate 'string "test-" title)) ()
         (let (,@variables)
           ,@(reverse code))))))

(defun test2 (&optional verbose)
  (let* ((test-directory (merge-pathnames "tests/level1/core/" *directory*))
         (suite
          (dom:document-element
           (xml:parse-file (merge-pathnames "alltests.xml" test-directory))))
         (n 0)
         (i 0))
    (do-child-elements (member suite)
      (declare (ignore member))
      (incf n))
    (do-child-elements (member suite)
      (let ((href (dom:get-attribute member "href")))
        (format t "~&~D/~D ~A~%" i n href)
        (let ((lisp (slurp-test (merge-pathnames href test-directory))))
          (when verbose
            (print lisp))))
      (incf i))))

#+(or)
(test "attrname")

#+(or)
(read-methods)

#+(or)
(test2)
