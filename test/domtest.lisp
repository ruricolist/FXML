(defpackage :domtest
  (:use :cl :xml)
  (:export #:run-all-tests))
(defpackage :domtest-tests
  (:use))
(in-package :domtest)


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
			    (sequence `(find ,key ',keys :test 'string=))
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

(defun tag-name (elt)
  (runes:rod-string (dom:tag-name elt)))

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
                           (equal (tag-name node) name)))
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
  (unless (stringp name)
    (setf name (runes:rod-string name)))
  (if (zerop (length name))
      nil
      (intern name :domtest-tests)))

(defun replace-studly-caps (str)
  (unless (stringp str)
    (setf str (runes:rod-string str)))
  ;; s/([A-Z][a-z])/-\1/
  (with-output-to-string (out)
    (with-input-from-string (in str)
      (for ((c = (read-char in nil nil))
            (previous = nil then c)
            (next = (peek-char nil in nil nil))
            :while c)
        (when (and previous
                   (upper-case-p c) next (lower-case-p next)
                   (not (lower-case-p previous)))
          (write-char #\- out))
        (write-char (char-downcase c) out)
        (when (and (lower-case-p c) next (upper-case-p next))
          (write-char #\- out))))))

(defun intern-dom (name)
  (intern (replace-studly-caps name) :dom))

(defun child-elements (element)
  (map-child-elements 'list #'identity element))

(defun parse-java-literal (str)
  (unless (stringp str)
    (setf str (runes:rod-string str)))
  (cond
    ((zerop (length str)) nil)
    ((equal str "true")
      t)
    ((equal str "false")
      nil)
    ((digit-char-p (char str 0))
      (parse-integer str))
    ((char= (char str 0) #\")
      (runes:rod
       (with-output-to-string (out)
         (with-input-from-string (in str)
           (read-char in)
           (for ((c = (read-char in))
                 :until (char= c #\"))
               (if (char= c #\\)
                   (ecase (read-char in)
                     ;; ...
                     (#\n (write-char #\newline out)))
                   (write-char c out)))))))
    (t
      (%intern str))))

(defun maybe-setf (place form)
  (if place
      `(setf ,place ,form)
      form))

(defun nullify (str)
  (if (zerop (length str)) nil str))


;;;; dom1-interfaces.xml auslesen

(defvar *methods* '())
(defvar *fields* '())

(declaim (special *directory*))

(defun read-members ()
  (let* ((pathname (merge-pathnames "patches/dom1-interfaces.xml" *directory*))
         (builder (dom:make-dom-builder))
         (library (dom:document-element (xml:parse-file pathname builder)))
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
  (string-case (tag-name element)
    ("equals" (translate-equals element))
    ("contentType" (translate-content-type element))
    ("hasFeature" (translate-has-feature element))
    ("implementationAttribute" (assert-have-implementation-attribute element))
    ("isNull" (translate-is-null element))
    ("not" (translate-is-null element))
    ("notNull" (translate-not-null element))
    ("or" (translate-or element))
    ("same" (translate-same element))
    (t (error "unknown condition: ~A" element))))

(defun equalsp (a b test)
  (when (typep a 'dom-impl::named-node-map)
    (setf a (dom:items a)))
  (when (typep b 'dom-impl::named-node-map)
    (setf b (dom:items b)))
  (if (and (typep a 'sequence) (typep b 'sequence))
      (null (set-exclusive-or (coerce a 'list) (coerce b 'list) :test test))
      (funcall test a b)))

(defun %equal (a b)
  (or (equal a b) (and (runes::rodp a) (runes::rodp b) (runes:rod= a b))))

(defun %equalp (a b)
  (or (equalp a b) (and (runes::rodp a) (runes::rodp b) (runes:rod-equal a b))))

(defun translate-equals (element)
  (with-attributes (|actual| |expected| |ignoreCase|) element
    `(equalsp ,(%intern actual)
              ,(parse-java-literal expected)
              ',(if (parse-java-literal |ignoreCase|) '%equal '%equal))))

(defun translate-same (element)
  (with-attributes (|actual| |expected|) element
    `(eql ,(%intern actual) ,(parse-java-literal expected))))

(defun translate-or (element)
  `(or ,@(map-child-elements 'list #'translate-condition element)))

(defun translate-instance-of (element)
  (with-attributes (|obj| |type|) element
    `(eq (dom:node-type ,(%intern |obj|))
         ',(string-case (runes:rod-string |type|)
             ("Document" :document)
             ("DocumentFragment" :document-fragment)
             ("Text" :text)
             ("Comment" :comment)
             ("CDATASection" :cdata-section)
             ("Attr" :attribute)
             ("Element" :element)
             ("DocumentType" :document-type)
             ("Notation" :notation)
             ("Entity" :entity)
             ("EntityReference" :entity-reference)
             ("ProcessingInstruction" :processing-instruction)
             (t (error "unknown interface: ~A" |type|))))))

(defun translate-is-null (element)
  (with-attributes (|obj|) element
    `(null ,(%intern |obj|))))

(defun translate-not-null (element)
  (with-attributes (|obj|) element
    (%intern |obj|)))

(defun translate-content-type (element) ;XXX verstehe ich nicht
  (with-attributes (|type|) element 
   `(equal ,|type| "text/xml")))

(defun translate-uri-equals (element)
  (with-attributes
      (|actual|
       |scheme| |path| |host| |file| |name| |query| |fragment| |isAbsolute|)
      element
    |isAbsolute|
   `(let ((uri (net.uri:parse-uri (runes:rod-string ,(%intern |actual|)))))
      (flet ((uri-directory (path)
               (namestring
                (make-pathname :directory (pathname-directory path))))
             (uri-file (path)
               (namestring (make-pathname :name (pathname-name path)
                                          :type (pathname-type path))))
             (uri-name (path)
               (pathname-name path))
             (maybe-equal (expected actual)
               (if expected
                   (%equal (runes::rod expected) (runes::rod actual))
                   t)))
        (and (maybe-equal ,(parse-java-literal |scheme|)
                          (net.uri:uri-scheme uri))
             (maybe-equal ,(parse-java-literal |host|)
                          (net.uri:uri-host uri))
             (maybe-equal ,(parse-java-literal |path|)
                          (uri-directory (net.uri:uri-path uri)))
             (maybe-equal ,(parse-java-literal |file|)
                          (uri-file (net.uri:uri-path uri)))
             (maybe-equal ,(parse-java-literal |name|)
                          (uri-name (net.uri:uri-path uri)))
             (maybe-equal ,(parse-java-literal |query|)
                          (net.uri:uri-query uri))
             (maybe-equal ,(parse-java-literal |fragment|)
                          (net.uri:uri-fragment uri)))))))


;;;; Statements uebersetzen

(defun translate-statement (element)
  (string-case (tag-name element)
    ("append" (translate-append element))
    ("assertDOMException" (translate-assert-domexception element))
    ("assertEquals"	(translate-assert-equals element))
    ("assertNotNull"	(translate-assert-not-null element))
    ("assertInstanceOf"	(translate-assert-instance-of element))
    ("assertNull"	(translate-assert-null element))
    ("assertSame"	(translate-assert-same element))
    ("assertSize"	(translate-assert-size element))
    ("assertTrue"	(translate-assert-true element))
    ("assertFalse"	(translate-assert-false element))
    ("assertURIEquals"	(translate-assert-uri-equals element))
    ("for-each"		(translate-for-each element))
    ("fail"		(translate-fail element))
    ("hasFeature" (translate-has-feature element))
    ("if"		(translate-if element))
    ("increment"	(translate-unary-assignment '+ element))
    ("decrement"	(translate-unary-assignment '- element))
    ("length"		(translate-length element))
    ("load"		(translate-load element))
    ("nodeType"		(translate-node-type element))
    ("plus"		(translate-binary-assignment '+ element))
    ("try"		(translate-try element))
    ("while"		(translate-while element))
    (t			(translate-member element))))

(defun translate-binary-assignment (fn element)
  (with-attributes (|var| |op1| |op2|) element
    (maybe-setf (%intern |var|)
                `(,fn ,(parse-java-literal |op1|)
                      ,(parse-java-literal |op2|)))))

(defun translate-unary-assignment (fn element)
  (with-attributes (|var| |value|) element
    (maybe-setf (%intern |var|)
                `(,fn ,(%intern |var|) ,(parse-java-literal |value|)))))

(defun translate-load (load)
  (with-attributes (|var| |href| |willBeModified|) load
    (maybe-setf (%intern |var|)
                `(load-file ,|href| ,(parse-java-literal |willBeModified|)))))

(defun translate-length (load)
  ;; XXX Soweit ich sehe unterscheiden die Tests nicht zwischen
  ;; der Laenge von DOMString und der length()-Methode der uebrigen
  ;; Interfaces.  Also unterscheiden wir das erstmal manuell.
  (with-attributes (|var| |obj|) load
    (let ((obj (%intern |obj|)))
      (maybe-setf (%intern |var|)
                  `(if (typep ,obj 'sequence)
                       (length ,obj)
                       (dom:length ,obj))))))

(defun translate-call (call method)
  (let ((name (car method))
        (args (mapcar (lambda (name)
                        (parse-java-literal (dom:get-attribute call name)))
                      (cdr method))))
    (with-attributes (|var| |obj|) call
      (maybe-setf (%intern |var|)
                  `(,(intern-dom name) ,(%intern |obj|) ,@args)))))

(defun translate-get (call name)
  (with-attributes (|var| |value| |obj|) call
    (cond
      ((nullify |var|)                  ;get
        (maybe-setf (%intern |var|) `(,(intern-dom name) ,(%intern |obj|))))
      ((nullify |value|)                ;set
        `(setf (,(intern-dom name) ,(%intern |obj|))
               ,(parse-java-literal |value|)))
      (t
        (error "oops")))))

(defun translate-has-feature (element)
  (with-attributes (|var| |feature| |version|) element
    (maybe-setf (%intern |var|)
                `(and (runes:rod-equal ,(parse-java-literal |feature|) #"XML")
                      (or (zerop (length ,(parse-java-literal |version|)))
                          (runes:rod-equal ,(parse-java-literal |version|) #"1.0"))))))

(defun translate-fail (element)
  (declare (ignore element))
  `(error "failed"))

(defun translate-node-type (element)
  ;; XXX Das muessten eigentlich ints sein, sind aber Keywords in CXML.
  (with-attributes (|var| |obj|) element
    (maybe-setf (%intern |var|)
                `(ecase (dom:node-type ,(%intern |obj|))
                   (:element                1)
                   (:attribute              2)
                   (:text                   3)
                   (:cdata-section          4)
                   (:entity-reference       5)
                   (:entity                 6)
                   (:processing-instruction 7)
                   (:comment                8)
                   (:document               9)
                   (:document-type          10)
                   (:document-fragment      11)
                   (:notation               12)))))

(defun translate-member (element)
  (let* ((name (dom:tag-name element))
         (method (find name *methods* :key #'car :test #'runes:rod=))
         (field (find name *fields* :test #'runes:rod=)))
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
    `(let ((collection ,(%intern |collection|)))
       (when (typep collection 'dom-impl::named-node-map)
         (setf collection (dom:items collection)))
       (assert (eql (length collection) ,(parse-java-literal |size|))))))

(defun translate-assert-instance-of (element)
  `(assert ,(translate-instance-of element)))

(defun translate-if (element)
  (destructuring-bind (condition &rest rest)
      (child-elements element)
    (let (then else)
      (dolist (r rest)
        (when (equal (tag-name r) "else")
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
    (unless (equal (tag-name c) "metadata")
      (return
        `(block assert-domexception
           (handler-bind
               ((dom-impl::dom-exception
                 (lambda (c)
                   (when (eq (dom-impl::dom-exception-key c)
                             ,(intern (tag-name c) :keyword))
                     (return-from assert-domexception)))))
             ,@(translate-body c)
             (error "expected exception ~A" ,(tag-name c))))))))

(defun translate-catch (catch return)
  `(lambda (c)
     ,@(map-child-elements
        'list
        (lambda (exception)
          `(when (eq (dom-impl::dom-exception-key c)
                     ,(intern (runes:rod-string (dom:get-attribute exception "code"))
                              :keyword))
             ,@(translate-body exception)
             ,return))
        catch)))

(defun translate-try (element)
  `(block try
     (handler-bind
         ((dom-impl::dom-exception
           ,(translate-catch
             (do-child-elements (c element :name "catch") (return c))
             '(return-from try))))
       ,@(map-child-elements 'list
                             (lambda (c)
                               (if (equal (tag-name c) "catch")
                                   nil
                                   (translate-statement c)))
                             element))))

(defun translate-append (element)
  (with-attributes (|collection| |item|) element
    (let ((c (%intern |collection|))
          (i (%intern |item|)))
      (maybe-setf c `(append ,c (list ,i))))))

(defun translate-assert-true (element)
  (with-attributes (|actual|) element
    `(assert ,(if (nullify |actual|)
                  (%intern |actual|)
                  (translate-condition
                   (do-child-elements (c element) (return c)))))))

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
    `(let ((collection ,(%intern |collection|)))
       (when (typep collection 'dom-impl::named-node-map)
         (setf collection (dom:items collection)))
       (map nil (lambda (,(%intern |member|)) ,@(translate-body element))
            collection))))

(defun assert-have-implementation-attribute (element)
  (let ((attribute (runes:rod-string (dom:get-attribute element "name"))))
    (string-case attribute
      (t
        (format t "~&implementationAttribute ~A not supported, skipping test~%"
                attribute)
        (throw 'give-up nil)))))

(defun slurp-test (pathname)
  (unless *fields*
    (multiple-value-setq (*methods* *fields*) (read-members *directory*)))
  (catch 'give-up
    (let* ((builder (dom:make-dom-builder))
           (test (dom:document-element (xml:parse-file pathname builder)))
           title
           (bindings '())
           (code '()))
      (declare (ignore title))
      (do-child-elements (e test)
        (string-case (tag-name e)
          ("metadata"
            (let ((title-element (find-child-element "title" e)))
              (setf title (dom:data (dom:first-child title-element)))))
          ("var"
            (push (list (%intern (dom:get-attribute e "name"))
                        (string-case (runes:rod-string
                                       (dom:get-attribute e "type"))
                          (("byte" "short" "int" "long") 0)
                          (t nil)))
                  bindings)
            (do-child-elements (member e :name "member") e
              (push `(setf ,(%intern (dom:get-attribute e "name"))
                           (append ,(%intern (dom:get-attribute e "name"))
                                   (list
                                    ,(parse-java-literal
                                      (dom:data
                                       (dom:item
                                        (dom:child-nodes member)
                                        0))))))
                    code)))
          ("implementationAttribute"
            (assert-have-implementation-attribute e))
          (t
            (push (translate-statement e) code))))
      `(lambda ()
         (let (,@bindings)
           (declare (ignorable ,@(mapcar #'car bindings)))
           ,@(reverse code))))))

(defun load-file (name &optional will-be-modified-p)
  (declare (ignore will-be-modified-p))
  (setf name (runes:rod-string name))
  (let* ((directory (merge-pathnames "tests/level1/core/files/" *directory*))
         (document
          (xml:parse-file
           (make-pathname :name name :type "xml" :defaults directory)
           (dom:make-dom-builder))))
    document))

(defparameter *bad-tests*
    '("hc_elementnormalize2.xml" "hc_nodereplacechildnewchildexists.xml"))

(defun run-all-tests (*directory* &optional verbose)
  (let* ((xml::*redefinition-warning* nil)
         (test-directory (merge-pathnames "tests/level1/core/" *directory*))
         (all-tests (merge-pathnames "alltests.xml" test-directory))
         (builder (dom:make-dom-builder))
         (suite (dom:document-element (xml:parse-file all-tests builder)))
         (n 0)
         (i 0)
         (ntried 0)
         (nfailed 0))
    (do-child-elements (member suite)
      (unless
          (member (runes:rod-string (dom:get-attribute member "href"))
                  *bad-tests*
                  :test 'equal)
        (incf n)))
    (do-child-elements (member suite)
      (let ((href (runes:rod-string (dom:get-attribute member "href"))))
        (unless (member href *bad-tests* :test 'equal)
          (format t "~&~D/~D ~A~%" i n href)
          (let ((lisp (slurp-test (merge-pathnames href test-directory))))
            (when verbose
              (print lisp))
            (when lisp
              (incf ntried)
              (with-simple-restart (skip-test "Skip this test")
                (handler-case
                    (funcall (compile nil lisp))
                  (serious-condition (c)
                    (incf nfailed)
                    (warn "test failed: ~A" c))))))
          (incf i))))
    (format t "~&~D/~D tests failed; ~D test~:P were skipped"
            nfailed ntried (- n ntried))))

(defun run-test (*directory* href)
  (let* ((test-directory (merge-pathnames "tests/level1/core/" *directory*))
         (lisp (slurp-test (merge-pathnames href test-directory))))
    (print lisp)
    (when lisp
      (funcall (compile nil lisp)))))

#+(or)
(run-all-tests "~/src/2001/DOM-Test-Suite/")
