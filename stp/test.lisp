;;; -*- show-trailing-whitespace: t; indent-tabs: nil -*-

;;; Copyright (c) 2007 David Lichteblau. All rights reserved.
;;; (mostly transcribed from nu/xom/tests/*)

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

(defpackage :cxml-stp-test
  (:use :cl :rt :stp)
  #+openmcl (:shadow #:check-type))

(in-package :cxml-stp-test)

#+openmcl
(defmacro check-type (place type)
  `(assert (typep ,place ',type)))

(defmethod xmlconf::serialize-document ((document node))
  (serialize document (cxml:make-octet-vector-sink :canonical 2)))

(defun stp-test (filename handler &rest args)
  (declare (ignore handler))
  (apply #'cxml:parse-file
	 filename
	 (read-from-string "#.(cxml-stp:make-builder)")
	 :recode t
	 args))


#+(or)
(let ((cxml-stp::*serialize-canonical-notations-only-p* t))
  (xmlconf::run-all-tests 'xmlconf::stp-test
			  "/home/david/2001/XML-Test-Suite/xmlconf/"))


(defun assert-equal (a b)
  (unless (equal a b)
    (error "assertion failed: ~S and ~S are not EQUAL" a b)))

(defun assert-node= (a b)
  (unless (node= a b)
    (error "assertion failed: ~S and ~S are not NODE=" a b)))

(defmacro expect-condition (form type &optional data)
  `(handler-case
       (progn
	 ,form
	 (error "expected a condition of type ~A in:~%~A~@[~%for value ~A~]"
		',type
		',form
		,data))
     (,type ())))

(defun serialize-to-string (node)
  (let ((sink (cxml:make-string-sink)))
    (serialize node sink)
    (sax:end-document sink)))

(defmacro define-condition-test (name form type)
  `(deftest ,name
       (progn
	 (expect-condition ,form ,type)
	 (values))))

(defun child-count (node)
  (count-children-if (constantly t) node))

(defun named-node-= (a b)
  (and (equal (namespace-uri a) (namespace-uri b))
       (equal (namespace-prefix a) (namespace-prefix b))
       (equal (local-name a) (local-name b))))

(defun parent-node-= (e f)
  (and (eql (child-count e)
	    (child-count f))
       (every #'node= (list-children e) (list-children f))))

(defmethod node= ((e element) (f element))
  (and (named-node-= e f)
       (parent-node-= e f)
       (null
	(set-exclusive-or (list-attributes e) (list-attributes f)
			  :test #'node=))
       (flet ((collect-namespaces (elt)
		(let ((result ()))
		  (map-extra-namespaces (lambda (k v) (push (cons k v) result))
					elt)
		  result)))
	 (null
	  (set-exclusive-or (collect-namespaces e) (collect-namespaces f)
			    :test #'equal)))))

(defmethod node= ((a node) (b node))
  nil)

(defmethod node= ((e document) (f document))
  (parent-node-= e f))

(defmethod node= ((a attribute) (b attribute))
  (and (named-node-= a b)
       (equal (value a) (value b))))

(defmethod node= ((a comment) (b comment))
  (equal (data a) (data b)))

(defmethod node= ((a text) (b text))
  (equal (data a) (data b)))

(defmethod node= ((a processing-instruction) (b processing-instruction))
  (and (equal (data a) (data b))
       (equal (target a) (target b))))

(defmethod node= ((a document-type) (b document-type))
  (and (equal (root-element-name a) (root-element-name b))
       (equal (public-id a) (public-id b))
       (equal (system-id a) (system-id b))
       (equal (internal-subset a) (internal-subset b))))

(rem-all-tests)


;;;; TEXT

(deftest text.constructor
    (data (make-text "test"))
  "test")

(deftest text.legal
    (let ((text (make-text "name")))
      (dolist (str '("Hello"
		     "hello there"
		     "  spaces on both ends  "
		     " quotes \" \" quotes"
		     " single \'\' quotes"
		     " both double and single \"\'\"\' quotes"
		     " angle brackets <  > <<<"
		     #.(format nil " carriage returns ~C~C"
			(code-char 13) (code-char 13))
		     #.(format nil " newlines ~C~C"
			(code-char 10) (code-char 10))
		     #.(format nil " both ~C~C"
			(code-char 13) (code-char 10))
		     #.(format nil " tab ~C foo"
			(code-char 9))
		     " CDATA end: ]]>"
		     " <![CDATA[ CDATA end: ]]>"
		     " &amp; "
		     " ampersands & &&& &name; "))
	(setf (data text) str)
	(assert-equal (data text) str)
	(assert-equal (string-value text) str))
      (values)))

(deftest text.nil
    (let ((text (make-text "name")))
      (setf (data text) nil)
      (data text))
  "")

(define-condition-test text.illegal
    (let ((text (make-text "name")))
      (setf (data text) (format nil "test ~C test" (code-char 0))))
  stp-error)

(deftest text.serialize
    (let ((text (make-text "name"))
	  (pairs '("Hello"
		   "hello there"
		   "  spaces on both ends  "
		   ;; zzz CXML traditionally escapes quotes without good
		   ;; reason:
		   (" quotes \" \" quotes"
		    " quotes &quot; &quot; quotes")
		   (" both double and single \"\'\"\' quotes"
		    " both double and single &quot;\'&quot;\' quotes")
		   " single \'\' quotes"
		   ("<>" "&lt;&gt;")
		   ("&amp;" "&amp;amp;")
		   ("]]>" "]]&gt;")
		   (#.(string (code-char 13)) "&#13;")
		   "=,.!@#$%^*()_-'[]{}+/?;:`|\\")))
      (loop
	 for (in out) in (mapcar (lambda (x) (if (listp x) x (list x x)))
				 pairs)
	 do
	   (setf (data text) in)
	   (assert-equal (serialize-to-string text) out))
      (values)))

(deftest text.copy
    (let* ((c1 (make-text "test"))
	   (c2 (copy c1)))
      (assert (not (eq c1 c2)))
      (assert-equal (data c1) (data c2))
      (assert-equal nil (parent c2))
      (assert-equal (type-of c2) 'text)
      (values)))

;;; zzz surrogate testing is going to be a bit more work, because cxml
;;; supports both Lisps with 16 bit and with 21 bit characters.
;;;   - testSurrogates
;;;   - testNonBMPText
;;;   - testEndOfBMP
;;;   - testHighSurrogateWithNoLowSurrogate

(deftest text.leaf-node
    (let ((c1 (make-text "data")))
      (assert-equal 0 (child-count c1))
      (expect-condition (nth-child 0 c1) error)
      (assert-equal nil (parent c1))
      (let ((e (make-element "test")))
	(append-child e c1)
	(assert-equal e (parent c1))
	(assert-equal c1 (nth-child 0 e))
	(delete-child c1 e)
	(assert-equal 0 (child-count e)))
      (values)))

(deftest text.print-object
    (let ((n (make-text "heyho")))
      (assert-node= n (read-from-string (write-to-string n)))
      (values)))


;;;; COMMENT

(deftest comment.constructor
    (data (make-comment "test"))
  "test")

(deftest comment.constructor2
    (data (make-comment ""))
  "")

(deftest comment.constructor3
    (data (make-comment "- - "))
  "- - ")

(deftest comment.copy
    (let* ((c1 (make-comment "test"))
	   (c2 (copy c1)))
      (assert (not (eq c1 c2)))
      (assert-equal (data c1) (data c2))
      (assert-equal nil (parent c2))
      (assert-equal (type-of c2) 'comment)
      (values)))

(deftest comment.serialize
    (let ((c (make-comment "0123456789012345678901234567890123456789")))
      (assert-equal (serialize-to-string c)
		    "<!--0123456789012345678901234567890123456789-->")
      (values)))

;;; zzz das pruefen wir nicht
;; (define-condition-test comment.cr
;;     (make-comment (format nil "foo ~C bar" (code-char 13)))
;;   stp-error)

(deftest comment.setf
    (let ((c (make-comment "test")))
      (setf (data c) "legal")
      (assert-equal (data c) "legal")
      (assert-equal (string-value c) "legal")
      (expect-condition (setf (data c) "test -- test") stp-error)
      (expect-condition (setf (data c) "test-") stp-error)
      (setf (data c) nil)
      (assert-equal (data c) "")
      (values)))

;;; zzz
;;;   - testSurrogates
;;;   - testForbidUnmatchedSurrogatesInComments

(deftest comment.leaf-node
    (let ((c1 (make-comment "data")))
      (assert-equal 0 (child-count c1))
      (expect-condition (nth-child 0 c1) error)
      (assert-equal nil (parent c1))
      (let ((e (make-element "test")))
	(append-child e c1)
	(assert-equal e (parent c1))
	(assert-equal c1 (nth-child 0 e))
	(delete-child c1 e)
	(assert-equal 0 (child-count e)))
      (values)))

(deftest comment.document
    (let ((c1 (make-comment "data"))
	  (root (make-element "root")))
      (assert-equal nil (document c1))
      (append-child root c1)
      (assert-equal nil (document c1))
      (let ((document (make-document root)))
	(assert-equal document (document c1)))
      (values)))

(deftest comment.funny-characters-allowed
    (assert-equal (serialize-to-string (make-comment "<test>&amp;&greater;"))
		  "<!--<test>&amp;&greater;-->")
  nil)

(define-condition-test comment.only-char-allowed
    (make-comment (format nil " ~C " (code-char 1)))
  stp-error)

(deftest comment.print-object
    (let ((n (make-comment "heyho")))
      (assert-node= n (read-from-string (write-to-string n)))
      (values)))


;;;; PROCESSING-INSTRUCTION

(deftest pi.constructor.1
    (let ((p-i (make-processing-instruction "abc" "def")))
      (assert-equal (target p-i) "abc")
      (assert-equal (data p-i) "def")
      (values)))

(deftest pi.constructor.2
    (data (make-processing-instruction "abc" ""))
  "")

(deftest pi.constructor.3
    (data (make-processing-instruction "abc" nil))
  "")

(deftest pi.constructor.4
    (target (make-processing-instruction "abc123" nil))
  "abc123")

(deftest pi.constructor.illegal
    (progn
      (expect-condition (make-processing-instruction "test:test" "test")
			stp-error)
      (expect-condition (make-processing-instruction "" "test")
			stp-error)
      (expect-condition (make-processing-instruction nil "test")
			stp-error)
      (expect-condition (make-processing-instruction "12345" "test")
			stp-error)
      (values)))

(deftest pi.serialize
    (serialize-to-string (make-processing-instruction "abc" "def"))
  "<?abc def?>")

(deftest pi.serialize.2
    (serialize-to-string (make-processing-instruction "abc" ""))
  "<?abc?>")

(deftest pi.serialize.3
    (serialize-to-string
     (make-processing-instruction "target" "<test>&amp;&greater;"))
  "<?target <test>&amp;&greater;?>")

(deftest pi.copy
    (let* ((c1 (make-processing-instruction "target" "data"))
	   (c2 (copy c1)))
      (assert (not (eq c1 c2)))
      (assert-equal (data c1) (data c2))
      (assert-equal (target c1) (target c2))
      (assert-equal nil (parent c2))
      (assert-equal (type-of c2) 'processing-instruction)
      (values)))

(deftest pi.setf
    (let* ((p-i (make-processing-instruction "target" "data")))
      (expect-condition (setf (data p-i) "?>") stp-error)
      (expect-condition (setf (data p-i) "uhesta ?>") stp-error)
      (expect-condition (setf (data p-i) "uhesta ?> hst") stp-error)
      (setf (data p-i) nil)
      (assert-equal (data p-i) "")
      (dolist (str '("<html></html>"
		     "name=value"
		     "name='value'"
		     "name=\"value\""
		     "salkdhsalkjhdkjsadhkj sadhsajkdh"
		     "<?"
		     "? >"
		     "--"))
	(setf (data p-i) str)
	(assert-equal (data p-i) str))
      (values)))

(deftest pi.setf.2
    (let* ((p-i (make-processing-instruction "target" "data")))
      (expect-condition (setf (data p-i) (string (code-char 0))) stp-error)
      (assert-equal (data p-i) "data")
      (values)))

;;; zzz testCorrectSurrogates
;;; zzz testSurrogates

(deftest pi.leaf-node
    (let ((c1 (make-processing-instruction "target" "data")))
      (assert-equal 0 (child-count c1))
      (expect-condition (nth-child 0 c1) error)
      (assert-equal nil (parent c1))
      (let ((e (make-element "test")))
	(append-child e c1)
	(assert-equal e (parent c1))
	(assert-equal c1 (nth-child 0 e))
	(delete-child c1 e)
	(assert-equal 0 (child-count e)))
      (values)))

;;; zzz das pruefen wir nicht
;; (define-condition-test pi.cr
;;     (make-processing-instruction "target" (format nil "foo ~C bar" (code-char 13)))
;;   stp-error)

(deftest pi.invalid
    (dolist (str (list "  initial spaces"
		       (format nil "~Cinitial tab" (code-char 9))
		       (format nil "~Cinitial newline" (code-char 10))
		       (format nil "~Cinitial cr" (code-char 13)))
	     (values))
      (expect-condition (make-processing-instruction "target" str) stp-error)))

(deftest pi.invalid.xml
    (dolist (str (list "xml" "XML" "Xml")
	     (values))
      (expect-condition (make-processing-instruction str "data") stp-error)))

(deftest pi.invalid.colon
    (dolist (str (list "pre:target" "pre:" ":target")
	     (values))
      (expect-condition (make-processing-instruction str "data") stp-error)))

(deftest pi.string-value
    (let ((n (make-processing-instruction "target" "data")))
      (string-value n))
  "data")

(deftest pi.print-object
    (let ((n (make-processing-instruction "target" "data")))
      (assert-node= n (read-from-string (write-to-string n)))
      (values)))


;;;; DOCUMENT-TYPE

(defparameter +name+ "Ottokar")
(defparameter +sysid+ "http://www.w3.org/TR/some.dtd")
(defparameter +pubid+ "-//Me//some public ID")

(deftest doctype.constructor.1
    (let ((doctype (make-document-type +name+ +sysid+ +pubid+)))
      (assert-equal (root-element-name doctype) +name+)
      (assert-equal (system-id doctype) +sysid+)
      (assert-equal (public-id doctype) +pubid+)
      (values)))

(deftest doctype.constructor.2
    (let ((doctype (make-document-type +name+ +sysid+)))
      (assert-equal (root-element-name doctype) +name+)
      (assert-equal (system-id doctype) +sysid+)
      (assert-equal (public-id doctype) nil)
      (values)))

(deftest doctype.constructor.3
    (let ((doctype (make-document-type +name+)))
      (assert-equal (root-element-name doctype) +name+)
      (assert-equal (system-id doctype) nil)
      (assert-equal (public-id doctype) nil)
      (values)))

(deftest doctype.constructor.3a
    (let ((doctype (make-document-type "try:name")))
      (assert-equal (root-element-name doctype) "try:name")
      (assert-equal (system-id doctype) nil)
      (assert-equal (public-id doctype) nil)
      (values)))

(define-condition-test doctype.constructor.4
    (make-document-type "try name")
  stp-error)

(define-condition-test doctype.constructor.5
    (make-document-type nil)
  error)

(define-condition-test doctype.constructor.6
    (make-document-type "")
  error)

(define-condition-test doctype.constructor.7
    (make-document-type ":try")
  stp-error)

(deftest doctype.constructor.8
    (let* ((root (make-element "root"))
	   (document (make-document root))
	   (new-root (make-element "new-root")))
      (assert-equal root (document-element document))
      (assert-equal 1 (child-count document))
      ;; change root
      (setf (document-element document) new-root)
      (assert-equal new-root (document-element document))
      (assert-equal 1 (child-count document))
      ;; append comment
      (append-child document (make-comment "test"))
      (assert-equal 2 (child-count document))
      ;; prepend comment
      (prepend-child document (make-comment "prolog comment"))
      (assert-equal 3 (child-count document))
      (check-type (nth-child 0 document) comment)
      (check-type (nth-child 1 document) element)
      (check-type (nth-child 2 document) comment)
      ;; insert PI
      (insert-child document (make-processing-instruction "t" "d") 1)
      (check-type (nth-child 0 document) comment)
      (check-type (nth-child 1 document) processing-instruction)
      (check-type (nth-child 2 document) element)
      (check-type (nth-child 3 document) comment)
      ;; insert PI
      (insert-child document (make-processing-instruction "epilog" "d") 3)
      (check-type (nth-child 0 document) comment)
      (check-type (nth-child 1 document) processing-instruction)
      (check-type (nth-child 2 document) element)
      (check-type (nth-child 3 document) processing-instruction)
      (check-type (nth-child 4 document) comment)
      ;; null root
      (expect-condition (make-document nil) type-error)
      (values)))
(deftest doctype.serialize.1
    (let ((name "Ottokar")
	  (sysid "http://www.w3.org/TR/some.dtd")
	  (pubid "-//Me//some public ID"))
      (assert-equal (serialize-to-string (make-document-type name sysid pubid))
		    (format nil "<!DOCTYPE ~A PUBLIC \"~A\" \"~A\">~%"
			    name pubid sysid))
      (assert-equal (serialize-to-string (make-document-type name sysid))
		    (format nil "<!DOCTYPE ~A SYSTEM \"~A\">~%" name sysid))
      (assert-equal (serialize-to-string (make-document-type name))
		    (format nil "<!DOCTYPE ~A>~%" name))
      (values)))

(deftest doctype.serialize.2
    (let* ((str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE test [
<!ELEMENT test #PCDATA>
]>
<test/>")
	   (d (cxml:parse str (make-builder) :validate t)))
      (assert-equal (serialize-to-string d) str)
      (values)))

(deftest doctype.serialize.3
    (let* ((subset "  <!--comment-->
  <!ELEMENT test #PCDATA>
  <!--comment-->
")
	   (expected
	    (format nil
		    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE test [
~A]>
<test/>"
		    subset))
	   (test (make-element "test"))
	   (d (make-document test))
	   (doctype (make-document-type "test")))
      (prepend-child d doctype)
      (setf (internal-subset doctype) subset)
      (assert-equal (serialize-to-string d) expected)
      (values)))

(deftest doctype.setf
    (let ((doctype (make-document-type "root")))
      (setf (root-element-name doctype) "newval")
      (assert-equal (root-element-name doctype) "newval")
      (setf (root-element-name doctype) "new:val")
      (assert-equal (root-element-name doctype) "new:val")
      (expect-condition (setf (root-element-name doctype) ":newval")
			stp-error)
      (expect-condition (setf (root-element-name doctype) "new val")
			stp-error)
      (values)))

(deftest doctype.setf.internal-subset.1
    (let ((doctype (make-document-type "root")))
      (setf (internal-subset doctype) "")
      (assert-equal (internal-subset doctype) "")
      (values)))

(deftest doctype.setf.internal-subset.2
    (let ((doctype (make-document-type "root")))
      (setf (internal-subset doctype) nil)
      (assert-equal (internal-subset doctype) "")
      (values)))

(deftest doctype.setf.internal-subset.3
    (let ((doctype (make-document-type "root")))
      (setf (internal-subset doctype) #1="<!ELEMENT test (PCDATA)>")
      (internal-subset doctype))
  #1#)

;;; FIXME: sollen wir das nun pruefen oder nicht?
;; (deftest doctype.setf.internal-subset.4
;;     (let ((doctype (make-document-type "root")))
;;       (setf (internal-subset doctype)
;; 	    #1="<!ENTITY % test SYSTEM 'http://www.example.com/notexists.dtd'>
;; %test;\n")
;;       (internal-subset doctype))
;;   #1#)

(define-condition-test doctype.setf.internal-subset.5
    (let ((doctype (make-document-type "root")))
      (setf (internal-subset doctype) "<!ELEMENT test (PCDATA>"))
  stp-error)

(deftest doctype.leaf-node
    (list-children (make-document-type "root"))
  nil)

(deftest doctype.pubid
    (labels ((legal (pubid)
	       (let ((pubid
		      (etypecase pubid
			(string pubid)
			(integer (string (code-char pubid)))
			(character (string pubid)))))
		 (assert-equal
		  (public-id (make-document-type
			      "name"
			      "http://www.w3.org/TR/some.dtd"
			      pubid))
		  pubid)))
	     (illegal (pubid)
	       (expect-condition (legal pubid) stp-error pubid)))
      (loop for i from 0 to 9 do (illegal i))
      (illegal 11)
      (illegal 12)
      (loop for i from 14 below 32 do (illegal i))
      (loop for i from 126 below 1000 do (illegal i))
      (map nil #'illegal "<>`^&\"[]{}|\\~")
      (map nil #'legal "-'()+,./:=?;!*#@$_%")
      (loop for i from (char-code #\a) to (char-code #\z) do (legal i))
      (loop for i from (char-code #\A) to (char-code #\Z) do (legal i))
      (loop for i from (char-code #\0) to (char-code #\9) do (legal i))
      (legal "foo bar")
      #+(or)
      (progn				;sehe ich nicht ein
	(illegal " foo")
	(illegal "foo ")
	(illegal "foo  bar")
	(illegal (format nil "foo~Cbar" (code-char 10)))
	(illegal (format nil "foo~Cbar" (code-char 13)))))
  nil)

(deftest doctype.sysid
    (labels ((legal (sysid)
	       (let ((sysid
		      (etypecase sysid
			(string sysid)
			(integer (string (code-char sysid)))
			(character (string sysid)))))
		 (assert-equal
		  (system-id (make-document-type
			      "name"
			      sysid))
		  sysid)))
	     (illegal (sysid)
	       (expect-condition (legal sysid) stp-error sysid)))
      (legal "http://www.example.com/test$red/limit,data.xml")
      (legal "smb://domain;user:pass@server/share/path/to/file")
      (illegal "http://www.example.com/index.html#test")
      (illegal "http://www.example.com/index.html#")
      (illegal #xa9)
      (illegal #xc0)
      (illegal "both \" and '"))
  nil)

(deftest doctype.copy
    (let* ((name "Ottokar")
	   (sysid "http://www.w3.org/TR/some.dtd")
	   (pubid "-//Me//some public ID")
	   (c1 (make-document-type name sysid pubid))
	   (c2 (copy c1)))
      (assert-equal (root-element-name c1) (root-element-name c2))
      (assert-equal (public-id c1) (public-id c2))
      (assert-equal (system-id c1) (system-id c2))
      (assert-equal (internal-subset c1) (internal-subset c2))
      (assert (not (eq c1 c2)))
      (values)))

(define-condition-test doctype.pubid-needs-sysid
    (setf (public-id (make-document-type "Ottokar")) "-//Me//some public ID")
  stp-error)

(deftest doctype.remove
    (let* ((name "Ottokar")
	   (sysid "http://www.w3.org/TR/some.dtd")
	   (pubid "-//Me//some public ID")
	   (doctype (make-document-type name sysid pubid)))
      (setf (public-id doctype) nil)
      (assert-equal nil (public-id doctype))
      (setf (public-id doctype) pubid)
      (assert-equal pubid (public-id doctype))
      (expect-condition (setf (system-id doctype) nil) stp-error)
      (setf (public-id doctype) nil)
      (assert-equal nil (public-id doctype))
      (setf (system-id doctype) nil)
      (assert-equal nil (system-id doctype))
      (values)))

(deftest doctype.print-object
    (let* ((name "Ottokar")
	   (sysid "http://www.w3.org/TR/some.dtd")
	   (pubid "-//Me//some public ID")
	   (n (make-document-type name sysid pubid)))
      (assert-node= n (read-from-string (write-to-string n)))
      (values)))

(deftest doctype.string-value
    (let* ((name "Ottokar")
	   (sysid "http://www.w3.org/TR/some.dtd")
	   (pubid "-//Me//some public ID")
	   (n ))
      (string-value (make-document-type name sysid pubid)))
  "")

(deftest doctype.setf.public-id.nil
    (let* ((name "Ottokar")
	   (sysid "http://www.w3.org/TR/some.dtd")
	   (pubid "-//Me//some public ID")
	   (n (make-document-type name sysid pubid)))
      (setf (public-id n) "")
      (assert-equal sysid (system-id n))
      (assert-equal nil (public-id n))
      (values)))

(deftest doctype.setf.system-id.nil
    (let* ((name "Ottokar")
	   (sysid "http://www.w3.org/TR/some.dtd")
	   (pubid "-//Me//some public ID")
	   (n (make-document-type name sysid pubid)))
      (setf (public-id n) nil)
      (assert-equal sysid (system-id n))
      (setf (system-id n) "")
      (assert-equal nil (system-id n))
      (values)))


;;;; DOCUMENT

(deftest document.insertion
    (let* ((root (make-element "root"))
	   (document (make-document root))
	   (doctype (make-document-type "root")))
      (expect-condition (insert-child document doctype 1) stp-error)
      (insert-child document doctype 0)
      (assert-equal (document-type document) doctype)
      (let ((doctype2 (make-document-type "test")))
	(expect-condition (insert-child document doctype2 1) stp-error)
	(assert-equal (document-type document) doctype)
	(assert-equal (first-child document) doctype)
	(assert-equal nil (parent doctype2))
	;; install doctype2
	(setf (document-type document) doctype2)
	(assert-equal (document-type document) doctype2)
	(assert-equal (first-child document) doctype2)
	(assert-equal nil (parent doctype))
	;; once again, noop
	(setf (document-type document) doctype2)
	(assert-equal (document-type document) doctype2)
	(assert-equal (first-child document) doctype2)
	(assert-equal nil (parent doctype))
	;; nil not allowed
	(expect-condition (setf (document-type document) nil) type-error)
	(assert-equal (document-type document) doctype2)
	;; two parents not allowed
	(let ((document2 (make-document (make-element "root"))))
	  (expect-condition (setf (document-type document2) doctype2)
			    stp-error)
	  (assert-equal (parent doctype2) document)))
      (values)))

(deftest document.base-uri
    (let* ((root (make-element "root"))
	   (document (make-document root)))
      (assert-equal (base-uri document) "")
      (dolist (url '("http://www.example.com/index.xml"
		     "http://www.example.com/index.xml"
		     "file:///home/elharo/XO%4D/data/test.xml"))
	(setf (base-uri document) url)
	(assert-equal (base-uri document) url))
      (values)))

(define-condition-test document.second-root
    (let* ((root (make-element "root"))
	   (document (make-document root)))
      (insert-child document (make-element "root2") 0))
  stp-error)

(deftest document.setf.document-element
    (let* ((root (make-element "root"))
	   (document (make-document root))
	   (new-root (make-element "new-root")))
      ;; change
      (setf (document-element document) new-root)
      (assert-equal (document-element document) new-root)
      (assert-equal nil (parent root))
      (expect-condition (setf (document-element document) nil) type-error)
      ;; no multiple parents
      (let ((top (make-element "top"))
	    (child (make-element "child")))
	(append-child top child)
	(expect-condition (setf (document-element document) child) stp-error))
      ;; once again, noop
      (setf (document-element document) new-root)
      (assert-equal (document-element document) new-root)
      (assert-equal nil (parent root))
      (values)))

(deftest document.setf.document-element.regression.1
    (let* ((root (make-element "root"))
	   (document (make-document root))
	   (new-root (make-element "new-root")))
      (stp:prepend-child document (make-comment "test"))
      ;; change
      (setf (document-element document) new-root)
      (assert-equal (document-element document) new-root)
      (assert-equal nil (parent root))
      (expect-condition (setf (document-element document) nil) type-error)
      ;; no multiple parents
      (let ((top (make-element "top"))
	    (child (make-element "child")))
	(append-child top child)
	(expect-condition (setf (document-element document) child) stp-error))
      ;; once again, noop
      (setf (document-element document) new-root)
      (assert-equal (document-element document) new-root)
      (assert-equal nil (parent root))
      (values)))

;; like document.setf.document-element, but using replace-child instead
(deftest document.setf.replace-child
    (let* ((root (make-element "root"))
	   (document (make-document root))
	   (new-root (make-element "new-root")))
      ;; change
      (replace-child document root new-root)
      (assert-equal (document-element document) new-root)
      (assert-equal nil (parent root))
      (expect-condition (setf (document-element document) nil) type-error)
      ;; no multiple parents
      (let ((top (make-element "top"))
	    (child (make-element "child")))
	(append-child top child)
	(expect-condition (replace-child document child new-root) stp-error))
      ;; once again, noop
      (replace-child document new-root new-root)
      (assert-equal (document-element document) new-root)
      (assert-equal nil (parent root))
      (values)))

(deftest document.insertion-allowed
    (let* ((root (make-element "root"))
	   (document (make-document root))
	   (original (make-comment "original"))
	   (c2 (make-comment "new comment"))
	   (temp (make-element "temp")))
      (prepend-child document original)
      (append-child temp c2)
      (expect-condition (replace-child document original c2) stp-error)
      (assert-equal (list-children document) (list original root))
      (values)))

(deftest document.replace-doctype.1
    (let* ((root (make-element "root"))
	   (document (make-document root))
	   (new (make-document-type "new"))
	   (old (make-document-type "old")))
      (setf (document-type document) old)
      (replace-child document old new)
      (assert-equal new (document-type document))
      (assert-equal nil (parent old))
      (assert-equal document (parent new))
      (values)))

(deftest document.replace-doctype.2
    (let* ((root (make-element "root"))
	   (document (make-document root))
	   (new (make-document-type "new"))
	   (old (make-document-type "old"))
	   (temp (make-document (make-element "root"))))
      (setf (document-type temp) new)
      (setf (document-type document) old)
      (expect-condition (setf (document-type document) new) stp-error)
      (assert-equal old (document-type document))
      (assert-equal document (parent old))
      (assert-equal new (document-type temp))
      (assert-equal temp (parent new))
      (values)))

(deftest document.replacement-allowed.1
    (let* ((root (make-element "root"))
	   (document (make-document root))
	   (comment (make-comment "c")))
      (expect-condition (replace-child document root comment) stp-error)
      (assert-equal root (document-element document))
      (assert-equal document (parent root))
      (assert-equal nil (parent comment))
      (values)))

(deftest document.replacement-allowed.2
    (let* ((document (make-document (make-element "root")))
	   (comment (make-comment "not a doctype"))
	   (doctype (make-document-type "new")))
      (prepend-child document comment)
      (replace-child document comment doctype)
      (assert-equal doctype (document-type document))
      (assert-equal document (parent doctype))
      (assert-equal nil (parent comment))
      (values)))

(deftest document.detach
    (let* ((document (make-document (make-element "root")))
	   (comment (make-comment "c")))
      (append-child document comment)
      (assert-equal document (parent comment))
      (detach comment)
      (assert-equal nil (parent comment))
      (values)))

(deftest document.document
    (let ((document (make-document (make-element "root"))))
      (assert-equal document (document document))
      (values)))

(deftest document.root
    (let ((document (make-document (make-element "root"))))
      (assert-equal document (root document))
      (values)))

(deftest document.copy
    (let* ((root (make-element "root"))
	   (document (make-document root)))
      (prepend-child document (make-comment "text"))
      (insert-child document (make-processing-instruction "text" "data") 1)
      (insert-child document (make-document-type "text") 2)
      (append-child root (make-comment "after"))
      (append-child document (make-processing-instruction "text" "after"))
      (assert-node= document (copy document))
      (values)))

(deftest document.append-child
    (let* ((root (make-element "root"))
	   (document (make-document root)))
      (expect-condition (append-child document (make-text "test")) stp-error)
      (expect-condition (append-child document (make-text "   ")) stp-error)
      (append-child document (make-comment "foo"))
      (expect-condition (append-child document (make-element "test"))
			stp-error)
      (expect-condition (insert-child document (make-element "foo") 0)
			stp-error)
      (values)))

(deftest document.delete-child
    (let* ((root (make-element "root"))
	   (document (make-document root)))
      (expect-condition (detach root) stp-error)
      (expect-condition (delete-child root document) stp-error)
      (expect-condition (delete-child-if #'identity document :start 0 :count 1)
			stp-error)
      (append-child document (make-comment "test"))
      (delete-child-if #'identity document :start 1 :count 1)
      (assert-equal 1 (child-count document))
      (let ((test (make-comment "test")))
	(append-child document test)
	(delete-child test document)
	(assert-equal 1 (child-count document)))
      (delete-child (make-comment "sd") document)
      (expect-condition
       (delete-child-if #'identity document :start 20 :count 1)
       stp-error)
      (values)))

(deftest document.delete-child.2
    (let* ((root (make-element "root"))
	   (document (make-document root))
	   (a (make-element "a"))
	   (b (make-element "b"))
	   (c (make-element "c"))
	   (d (make-element "d")))
      (append-child root a)
      (append-child root b)
      (append-child root c)
      (append-child root d)
      (delete-child-if #'identity root :count 1 :start 1 :end 3)
      (assert-equal (list a c d) (list-children root))
      (values)))

(deftest document.delete-child.3
    (let* ((root (make-element "root"))
	   (document (make-document root))
	   (a (make-element "a"))
	   (b (make-element "b"))
	   (c (make-element "c"))
	   (d (make-element "d")))
      (append-child root a)
      (append-child root b)
      (append-child root c)
      (append-child root d)
      (delete-child-if #'identity root :count 1 :start 1 :end 3 :from-end t)
      (assert-equal (list a b d) (list-children root))
      (values)))

(deftest document.serialize
    (let* ((root (make-element "root"))
	   (document (make-document root)))
      (serialize-to-string document))
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<root/>")

(deftest document.string-value
    (let* ((root (make-element "root"))
	   (document (make-document root)))
      (append-child root (make-text "frob"))
      (string-value document))
  "frob")

(deftest document.print-object
    (let ((n (make-document (make-element "root"))))
      (assert-node= n (read-from-string (write-to-string n)))
      (values)))


;;;; ELEMENT

(defmacro with-element-test ((&optional) &body body)
  `(let* ((child1 (make-element "test"))
	  (child2 (make-text "test2"))
	  (child3 (make-comment "test3"))
	  (child4 (make-element "pre:test" "http://www.example.com"))
	  (child5 (make-element "test" "http://www.example.com"))
	  (element (make-element "name")))
     (append-child element child1)
     (append-child element child2)
     (append-child element child3)
     (append-child element child4)
     (append-child element child5)
     (let ((str (format nil "  ~C~C" (code-char 13) (code-char 10))))
       (append-child element (make-text str)))
     ,@body))

(deftest element.of-name.1
    (with-element-test ()
      (length (filter-children (of-name nil "http://www.example.com") element)))
  2)

(deftest element.of-name.2
    (with-element-test ()
      (length (filter-children (of-name nil) element)))
  1)

(deftest element.find-if
    (with-element-test ()
      (assert-equal child1 (find-child-if (of-name "test") element))
      (assert-equal child4
		    (find-child-if (of-name "test" "http://www.example.com")
				   element))
      (assert-equal nil (find-child-if (of-name "none") element))
      (expect-condition (of-name "pre:test") stp-error)
      (assert-equal nil
		    (find-child-if (of-name "none" "http://www.example.com")
				   element))
      (values)))

(deftest element.xmlns-name
    (let* ((name "xmlns")
	   (e (make-element name)))
      (assert-equal name (local-name e))
      (assert-equal name (qualified-name e))
      (assert-equal "" (namespace-prefix e))
      (assert-equal "" (namespace-uri e))
      (values)))

(define-condition-test element.xmlns-prefix
    (make-element "xmlns:foo" "http://www.example.org/")
  stp-error)

(deftest element.constructor.1
    (let* ((name "Jethro")
	   (e (make-element name)))
      (assert-equal name (local-name e))
      (assert-equal name (qualified-name e))
      (assert-equal "" (namespace-prefix e))
      (assert-equal "" (namespace-uri e))
      (values)))

(deftest element.constructor.2
    (let* ((name "sakjdhjhd")
	   (uri "http://www.something.com/")
	   (e (make-element name uri)))
      (assert-equal name (local-name e))
      (assert-equal name (qualified-name e))
      (assert-equal "" (namespace-prefix e))
      (assert-equal uri (namespace-uri e))
      (values)))

(deftest element.constructor.3
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.something.com/")
	   (e (make-element name uri)))
      (assert-equal "sakjdhjhd" (local-name e))
      (assert-equal name (qualified-name e))
      (assert-equal "red" (namespace-prefix e))
      (assert-equal uri (namespace-uri e))
      (values)))

(deftest element.emptyns.1
    (let* ((name "sakjdhjhd")
	   (uri "http://www.something.com/")
	   (e (make-element name uri)))
      (setf (namespace-uri e) "")
      (assert-equal "" (namespace-uri e))
      (values)))

(deftest element.emptyns.2
    (let* ((name "sakjdhjhd")
	   (uri "http://www.something.com/")
	   (e (make-element name uri)))
      (serialize-to-string e))
  "<sakjdhjhd xmlns=\"http://www.something.com/\"/>")

(deftest element.emptyns.3
    (let ((e (make-element "e")))
      (add-attribute e (make-attribute "en"
				       "xml:lang"
				       "http://www.w3.org/XML/1998/namespace"))
      (serialize-to-string e))
  "<e xml:lang=\"en\"/>")

(deftest element.doctype
    (let* ((name "sakjdhjhd")
	   (uri "http://www.something.com/")
	   (e (make-element name uri)))
      (expect-condition (append-child e (make-document-type name)) stp-error)
      (values)))

(deftest element.xml-namespace
    (let* ((name "sakjdhjhd")
	   (uri "http://www.something.com/")
	   (e (make-element name uri))
	   (xml "http://www.w3.org/XML/1998/namespace"))
      (assert-equal xml (find-namespace "xml" e))
      (expect-condition (add-extra-namespace e "xml" "http://www.yahoo.com/")
			stp-error)
      (assert-equal xml (find-namespace "xml" e))
      (add-extra-namespace e "xml" xml)
      (assert-equal xml (find-namespace "xml" e))
      (values)))

(deftest element.undeclare-default
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (child (make-element name uri))
	   (parent (make-element "parent" "http://www.example.com/")))
      (assert-equal "http://www.example.com/" (find-namespace "" parent))
      (append-child parent child)
      (add-extra-namespace child "" "")
      (assert-equal "" (find-namespace "" child))
      (assert-equal "http://www.example.com/" (find-namespace "" parent))
      (let ((child2 (make-element "name" "http://www.default.com")))
	(append-child parent child2)
	(expect-condition (add-extra-namespace child2 "" "") stp-error))
      (values)))

(deftest element.setf-namespace-uri.1
    (let* ((name "sakjdhjhd")
	   (uri "http://www.red.com/")
	   (element (make-element name uri)))
      (add-attribute element (make-attribute "test" "attribute"))
      (setf (namespace-uri element) "")
      (assert-equal "" (find-namespace "" element))
      (values)))

(deftest element.setf-namespace-uri.2
    (let* ((name "sakjdhjhd")
	   (uri "http://www.red.com/")
	   (element (make-element name uri)))
      (add-attribute element (make-attribute "test" "red:attribute" uri))
      (setf (namespace-uri element) uri)
      (assert-equal uri (namespace-uri element))
      (assert-equal uri (find-namespace "red" element))
      (values)))

(deftest element.setf-namespace-uri.3
    (let* ((name "a")
	   (uri "http://www.w3.org/1999/xhtml")
	   (element (make-element name)))
      (add-attribute element (make-attribute "http://www.elharo.com" "href"))
      (setf (namespace-uri element) uri)
      (assert-equal uri (namespace-uri element))
      (values)))

(deftest element.setf-namespace-uri.4
    (let* ((name "a")
	   (uri "http://www.w3.org/1999/xhtml")
	   (element (make-element name)))
      (add-attribute element (make-attribute "http://www.elharo.com"
					     "html:href"
					     uri))
      (setf (namespace-uri element) "http://www.example.com")
      (setf (namespace-prefix element) "pre")
      (setf (namespace-uri element) uri)
      (setf (namespace-prefix element) "html")
      (assert-equal uri (namespace-uri element))
      (assert-equal "html" (namespace-prefix element))
      (values)))

(deftest element.serialize.1
    (let ((element (make-element "test")))
      (add-attribute element
		     (make-attribute "preserve"
				     "xml:space"
				     "http://www.w3.org/XML/1998/namespace"))
      (add-attribute element
		     (make-attribute "preserve"
				     "zzz:zzz"
				     "http://www.example.org"))
      (serialize-to-string element))
  "<test xmlns:zzz=\"http://www.example.org\" zzz:zzz=\"preserve\" xml:space=\"preserve\"/>")

(deftest element.xml-prefix
    (let ((element (make-element "xml:test"
				 "http://www.w3.org/XML/1998/namespace")))
      (map-extra-namespaces (lambda (k v) (error "bogus extra namespace"))
			    element)
      (assert-equal "<xml:test/>" (serialize-to-string element))
      (values)))

(deftest element.namespaces-mappings
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (e (make-element name uri)))
      (add-extra-namespace e "blue" "http://www.blue.com/")
      (add-extra-namespace e "green" "http://www.green.com/")
      (let ((a1 (make-attribute "test" "test"))
	    (a2 (make-attribute "data" "pre1:green" "http://www.green.com/"))
	    (a3 (make-attribute "data" "yellow:sfs" "http://www.yellow.com/")))
	(add-attribute e a1)
	(add-attribute e a2)
	(add-attribute e a3))
      (assert-equal "http://www.red.com/" (find-namespace "red" e))
      (assert-equal "http://www.green.com/" (find-namespace "green" e))
      (assert-equal "http://www.blue.com/" (find-namespace "blue" e))
      (assert-equal "http://www.green.com/" (find-namespace "pre1" e))
      (assert-equal "http://www.yellow.com/" (find-namespace "yellow" e))
      (let ((e2 (make-element "mauve:child" "http://www.mauve.com/")))
	(append-child e e2)
	(assert-equal "http://www.red.com/" (find-namespace "red" e2))
	(assert-equal "http://www.green.com/" (find-namespace "green" e2))
	(assert-equal "http://www.blue.com/" (find-namespace "blue" e2))
	(assert-equal "http://www.green.com/" (find-namespace "pre1" e2))
	(assert-equal "http://www.yellow.com/" (find-namespace "yellow" e2))
	(assert-equal nil (find-namespace "head" e2)))
      (expect-condition (add-extra-namespace e "pre1" "http://www.blue2.com")
			stp-error)
      (let ((a (make-attribute "data" "pre1:mauve" "http://www.sadas.com/")))
	(expect-condition (add-attribute e a) stp-error))
      (let ((a (make-attribute "data" "pre1:green" "http://www.example.com/")))
	(expect-condition (add-attribute e a) stp-error))
      (remove-extra-namespace e "green")
      (assert-equal nil (find-namespace "green" e))
      (add-extra-namespace e "green" "http://www.green2.com/")
      (assert-equal "http://www.green2.com/" (find-namespace "green" e))
      (values)))

(deftest element.attributes
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (e (make-element name uri))
	   (a1 (make-attribute "simple" "name"))
	   (a2 (make-attribute "data" "pre1:green" "http://www.green.com/")))
      (add-attribute e a1)
      (add-attribute e a2)
      (assert-equal a2 (find-attribute-named e "green" "http://www.green.com/"))
      (assert-equal a1 (find-attribute-named e "name"))
      (assert-equal a1 (find-attribute-named e "name" ""))
      (assert-equal e (parent a1))
      (assert-equal "simple" (value (find-attribute-named e "name")))
      (detach a1)
      (assert-equal nil (parent a1))
      (assert-equal nil (find-attribute-named e "name"))
      (assert-equal a2 (remove-attribute e a2))
      (assert-equal nil (parent a2))
      (assert-equal nil (find-attribute-named e "green" "http://www.green.com/"))
      (values)))

(deftest element.remove-attribute.1
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (e (make-element name uri))
	   (a1 (make-attribute "simple" "name"))
	   (a2 (make-attribute "data" "pre1:green" "http://www.green.com/")))
      (add-attribute e a1)
      (add-attribute e a2)
      (expect-condition (remove-attribute e nil) type-error)
      (values)))

(deftest element.remove-attribute.2
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (e (make-element name uri))
	   (a (make-attribute "simple" "name")))
      (add-attribute e (make-attribute "value" "name"))
      (expect-condition (remove-attribute e a) stp-error)
      (values)))

(deftest element.string-value
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (e (make-element name uri)))
      (assert-equal (string-value e) "")
      (append-child e (make-text "data"))
      (assert-equal (string-value e) "data")
      (append-child e (make-text " moredata"))
      (assert-equal (string-value e) "data moredata")
      (append-child e (make-comment " more data"))
      (assert-equal (string-value e) "data moredata")
      (append-child e (make-processing-instruction "target" "more data"))
      (assert-equal (string-value e) "data moredata")
      (let ((e2 (make-element "child")))
	(append-child e e2)
	(assert-equal (string-value e) "data moredata")
	(append-child e2 (make-text "something"))
	(assert-equal (string-value e) "data moredatasomething"))
      (values)))

(deftest element.setf-local-name
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (e (make-element name uri)))
      (assert-equal (local-name e) "sakjdhjhd")
      (dolist (x '("dude" "digits__" "digits1234" "digits-z"))
	(assert-equal x (setf (local-name e) x))
	(assert-equal (local-name e) x)) 
      (expect-condition (setf (local-name e) "spaces ") stp-error)
      (expect-condition (setf (local-name e) "digits:test") stp-error)
      (expect-condition (setf (local-name e) "digits!test") stp-error)
      (expect-condition
       (setf (local-name e) (format nil "digits~Ctest" (code-char 0)))
       stp-error)
      (values)))

(deftest element.setf-namespace-prefix
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (e (make-element name uri)))
      (assert-equal (namespace-prefix e) "red")
      (dolist (x '("dude" "digits__" "digits1234" "digits-z" ""))
	(assert-equal x (setf (namespace-prefix e) x))
	(assert-equal (namespace-prefix e) x)) 
      (dolist (y '("spaces "
		   "digits:test"
		   "digits!test"
		   #.(format nil "digits~Ctest" (code-char 0))))
	(expect-condition (setf (namespace-prefix e) y) stp-error))
      (values)))

(defparameter *legal-uris*
  '("http://www.is.edu/sakdsk#sjadh"
    "http://www.is.edu/sakdsk?name=value&name=head"
    "uri:isbn:0832473864"
    "http://www.examples.com:80"
    "http://www.examples.com:80/"
    "http://www.is.edu/%20sakdsk#sjadh"))

;;; we don't actually check URI syntax, but we still have to prevent
;;; URIs from slipping in that aren't even make up of XML characters
(defparameter *very-illegal-uris*
  (list (string (code-char 0))
	(string (code-char 128))))

(deftest element.setf-namespace-uri.5
    (let* ((name "a")
	   (uri "http://www.w3.org/1999/xhtml")
	   (element (make-element name uri)))
      (assert-equal (namespace-uri element) uri)
      (dolist (legal *legal-uris*)
	(setf (namespace-uri element) legal)
	(assert-equal (namespace-uri element) legal))
      (let ((prev (namespace-uri element)))
	(dolist (illegal *very-illegal-uris*)
	  (expect-condition (setf (namespace-uri element) illegal) stp-error))
	(assert-equal (namespace-uri element) prev))
      (values)))

(deftest element.setf-namespace-uri.6
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (element (make-element name uri)))
      (add-extra-namespace element "red" "http://www.red.com/")
      (expect-condition (setf (namespace-uri element) "http://www.example.com")
			stp-error)
      (values)))

(deftest element.setf-namespace-uri.7
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (element (make-element name uri))
	   (a (make-attribute  "value" "red:test" "http://www.red.com/")))
      (add-attribute element a)
      (expect-condition (setf (namespace-uri element) "http://www.example.com")
			stp-error)
      (values)))

(deftest element.setf.namespace-uri.8
    (let ((e (make-element "prefix:name" "http://www.foo.com/")))
      (expect-condition (setf (namespace-uri e) "") stp-error)
      (expect-condition (setf (namespace-uri e) nil) stp-error)
      (values)))

(deftest element.setf.namespace-prefix.1
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (element (make-element name uri)))
      (add-extra-namespace element "blue" "http://www.foo.com/")
      (add-attribute element (make-attribute
			      "value" "green:money" "http://www.example.com/"))
      (add-extra-namespace element "purple" uri)
      (add-attribute element (make-attribute "value" "mauve:money" uri))
      (expect-condition (setf (namespace-prefix element) "blue")
			stp-error)
      (expect-condition (setf (namespace-prefix element) "green")
			stp-error)
      (setf (namespace-prefix element) "purple")
      (assert-equal "purple" (namespace-prefix element))
      (setf (namespace-prefix element) "mauve")
      (assert-equal "mauve" (namespace-prefix element))
      (values)))

(deftest element.setf.namespace-prefix.2
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (element (make-element name uri)))
      (setf (namespace-prefix element) nil)
      (assert-equal "" (namespace-prefix element))
      (values)))

(deftest element.setf.namespace-prefix.3
    (let ((element (make-element "sakjdhjhd")))
      (expect-condition (setf (namespace-prefix element) "foo") stp-error)
      (values)))

(deftest element.add-extra-namespace
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (element (make-element name uri)))
      (dolist (legal *legal-uris*)
	(remove-extra-namespace element "prefix")
	(add-extra-namespace element "prefix" legal)
	(assert-equal legal (find-namespace "prefix" element)))
      (dolist (illegal *very-illegal-uris*)
	(remove-extra-namespace element "prefix")
	(expect-condition (add-extra-namespace element "prefix" illegal)
			  stp-error))
      (values)))

(deftest element.add-extra-namespace.2
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (element (make-element name uri)))
      (add-extra-namespace element "xmlns" "")
      (remove-extra-namespace element "xmlns")
      (expect-condition 
       (add-extra-namespace element "xmlns" "http://foo")
       stp-error)
      (values)))

(deftest element.add-extra-namespace.3
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (element (make-element name uri)))
      (add-extra-namespace element
			   "xml"
			   "http://www.w3.org/XML/1998/namespace")
      (remove-extra-namespace element "xml")
      (expect-condition 
       (add-extra-namespace element
			    "foo"
			    "http://www.w3.org/XML/1998/namespace")
       stp-error)
      (values)))

(deftest element.add-extra-namespace.4
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (element (make-element name uri)))
      (expect-condition (add-extra-namespace element "foo" "hoppla") warning)
      (values)))

(deftest element.add-extra-namespace.5
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (element (make-element name uri)))
      (add-extra-namespace element nil nil)
      (block nil
	(map-extra-namespaces (lambda (prefix uri)
				(assert-equal prefix "")
				(assert-equal uri "")
				(return t))
			      element)
	(error "extra namespace not found"))
      (values)))

(deftest element.add-extra-namespace.6
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (element (make-element name uri)))
      (expect-condition
       (add-extra-namespace element "foo" (string (code-char 0)))
       stp-error)
      (values)))

(deftest element.insert-child.nil
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (element (make-element name uri)))
      (expect-condition (insert-child element nil 0) error)
      (values)))

(deftest element.append-child.nil
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (element (make-element name uri)))
      (expect-condition (append-child element 0) error)
      (values)))

(deftest element.insert-child.1
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (e (make-element name uri))
	   (e2 (make-element "mv:child" "http://www.mauve.com"))
	   (e3 (make-element "mv:child" "http://www.mauve.com"))
	   (e4 (make-element "mv:child" "http://www.mauve.com")))
      (insert-child e e2 0)
      (insert-child e e3 0)
      (insert-child e3 e4 0)
      (assert-equal e3 (nth-child 0 e))
      (let* ((root (make-element "root"))
	     (doc (make-document root)))
	(expect-condition (insert-child e doc 0) stp-error))
      (expect-condition (insert-child e e2 0) stp-error)
      (expect-condition (insert-child e e4 0) stp-error)
      (expect-condition (insert-child e nil 0) error)
      (expect-condition (insert-child e (make-comment "test") 20) error)
      (expect-condition (insert-child e (make-comment "test") -20) error)
      (values)))

(deftest element.filter-children.1
    (with-element-test ()
      (let ((children (filter-children (alexandria:of-type 'element) element)))
	(assert-equal 3 (length children))
	(assert-equal child1 (elt children 0))
	(assert-equal child4 (elt children 1))
	(assert-equal child5 (elt children 2)))
      (let ((children (filter-children (of-name "nonesuch") element)))
	(assert-equal 0 (length children)))
      (let ((children (filter-children (of-name "test") element)))
	(assert-equal 1 (length children))
	(assert-equal child1 (elt children 0)))
      (let ((children
	     (filter-children (of-name "test" "http://www.example.com")
			      element)))
	(assert-equal 2 (length children))
	(assert-equal child4 (elt children 0))
	(assert-equal child5 (elt children 1)))
      (values)))

(deftest element.add-attribute.1
    (let ((element (make-element "name"))
	  (a1 (make-attribute "name" "value"))
	  (a2 (make-attribute "simple"
			      "xlink:type"
			      "http://www.w3.org/TR/1999/xlink")))
      (add-attribute element a1)
      (add-attribute element a2)
      (assert-equal 2 (length (list-attributes element)))
      (let ((element2 (make-element "name")))
	(expect-condition (add-attribute element2 a1) stp-error))
      (detach a1)
      (let ((funky (make-element "xlink:funky" "http://www.funky.org")))
	(expect-condition (add-attribute funky a2) stp-error))
      (detach a2)
      (let ((notasfunky
	     (make-element "prefix:funky" "http://www.w3.org/TR/1999/xlink")))
	(add-attribute notasfunky a2))
      (let ((a3 (make-attribute "simple"
				"xlink:type"
				"http://www.w3.org/TR/1999/xlink"))
	    (a4 (make-attribute "simple"
				"xlink:href"
				"http://www.w3.org/1998/xlink"))
	    (test (make-element "test")))
	(add-attribute test a3)
	(expect-condition (add-attribute test a4) stp-error))
      (let ((a5 (make-attribute "simple"
				"xlink:type"
				"http://www.w3.org/TR/1999/xlink"))
	    (a6 (make-attribute "simple"
				"xlink:type"
				"http://www.w3.org/1998/xlink"))
	    (test2 (make-element "test")))
	(add-attribute test2 a5)
	(expect-condition (add-attribute test2 a6) stp-error))
      (values)))

(deftest element.add-attribute.2
    (let ((element (make-element "name")))
      (add-extra-namespace element "xlink" "http://www.w3.org/TR/1999/xlink")
      (add-extra-namespace element "pre" "http://www.example.com")
      (let ((a1 (make-attribute "values" "name"))
	    (a2 (make-attribute "simple"
				"xlink:type" 
				"http://www.w3.org/TR/1999/xlink")))
	(add-attribute element a1)
	(add-attribute element a2)
	(assert-equal 2 (length (list-attributes element))))
      (expect-condition
       (add-attribute element
		      (make-attribute "value"
				      "pre:att"
				      "ftp://example.com/"))
       stp-error)
      (add-attribute element
		     (make-attribute "value"
				     "ok:att"
				     "ftp://example.com/"))
      (assert-equal 3 (length (list-attributes element)))
      (expect-condition
       (add-extra-namespace element "ok" "http://www.example.net")
       stp-error)
      (assert-equal "ftp://example.com/" (find-namespace "ok" element))
      (assert-equal "http://www.w3.org/TR/1999/xlink"
		    (find-namespace "xlink" element))
      (assert-equal "http://www.example.com" (find-namespace "pre" element))
      (values)))

(deftest element.add-attribute.3
    (let ((element (make-element "pre:name" "http://www.example.com")))
      (expect-condition
       (setf (attribute-value element "pre:a" "http://different") "value")
       stp-error)
      (values)))

(deftest element.add-attribute.4
    (let ((element (make-element "pre:name" "http://www.example.com")))
      (expect-condition
       (add-attribute element
		      (make-attribute"value" "pre:a" "http://different"))
       stp-error)
      (values)))

(deftest element.triple
    (serialize-to-string
     (copy
      (document-element
       (cxml:parse #1="<b><c1/><c2/></b>" (make-builder)))))
  #1#)

(deftest element.copy.1
    (let ((parent (make-element "parent"))
	  (child (make-element "child")))
      (append-child parent child)
      (assert-node= child (copy child))
      (values)))

(deftest element.copy.2
    (let ((parent (make-element "parent"))
	  (child (make-element "child")))
      (append-child parent child)
      (assert-node= parent (copy parent))
      (values)))

(deftest element.copy.3
    (let ((parent (make-element "parent"))
	  (a (make-attribute "value" "name")))
      (add-attribute parent a)
      (let ((copy (copy parent)))
	(assert-node= parent copy)
	(let ((copied (car (list-attributes copy))))
	  (assert-node= copied a)
	  (assert-equal copy (parent copied))))
      (values)))

(deftest element.copy.4
    (let ((parent (make-element "parent")))
      (assert-node= parent (copy parent))
      (values)))

(deftest element.copy.5
    (let* ((root (make-element "parent"))
	   (d (make-document root)))
      (assert-node= d (copy d))
      (values)))

(deftest element.copy.6
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (base-uri "http://www.example.com/")
	   (e (make-element name uri)))
      (add-extra-namespace e "blue" "http://www.blue.com")
      (add-extra-namespace e "green" "http://www.green.com")
      (let ((a1 (make-attribute "test" "test"))
	    (a2 (make-attribute "data" "pre1:green" "http://www.green.com"))
	    (a3 (make-attribute "data"
				"yellow:sfsdadf"
				"http://www.yellow.com/")))
	(add-attribute e a1)
	(add-attribute e a2)
	(add-attribute e a3))
      (append-child e (make-element "mv:child" "http://www.mauve.com"))
      (let ((e3 (make-element "mv:child" "http://www.mauve.com")))
	(prepend-child e e3)
	(append-child e3 (make-element "mv:child" "http://www.mauve.com")))
      (setf (base-uri e) base-uri)
      (let ((copy (copy e)))
	(assert-equal (find-namespace "red" e) (find-namespace "red" copy))
	(assert-equal (find-namespace "blue" e) (find-namespace "blue" copy))
	(assert-equal (string-value e) (string-value copy))
	(let ((ea (find-attribute-named e "test"))
	      (ca (find-attribute-named copy "test")))
	  (assert-equal (value ea) (value ca)))
	(assert-equal (base-uri e) (base-uri copy)))
      (values)))

(deftest element.copy.7
    (let* ((top (make-element "e"))
	   (parent top))
      (loop
	 for parent = top then child
	 for i from 0 below 100
	 for child = (make-element (format nil "e~D" i))
	 do (append-child parent child))
      (assert-node= top (copy top))
      (values)))

(deftest element.delete-children.1
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (parent (make-element name uri))
	   (a1 (make-attribute "test" "test")))
      (add-attribute parent a1)
      (let ((child1 (make-element "mv:child" "http://www.mauve.com"))
	    (child2 (make-element "mv:child" "http://www.mauve.com"))
	    (grandchild (make-element "mv:child" "http://www.mauve.com")))
	(append-child parent child1)
	(append-child parent child2)
	(append-child child2 grandchild)
	(assert-equal child2 (parent grandchild))
	(assert-equal parent (parent child1))
	(assert-equal parent (parent child2))
	(delete-children parent)
	(assert-equal nil (list-children parent))
	(assert-equal nil (parent child1))
	(assert-equal nil (parent child2))
	(assert-equal child2 (parent grandchild))
	(assert-equal parent (parent a1)))
      (values)))

(deftest element.delete-children.2
    (let ((base "http://www.example.com/")
	  (parent (make-element "parent"))
	  (child (make-element "child")))
      (setf (base-uri parent) base)
      (append-child parent child)
      (delete-children parent)
      (assert-equal base (base-uri child))
      (values)))

(deftest element.delete-children.3
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (parent (make-element name uri))
	   (a1 (make-attribute "test" "test")))
      (add-attribute parent a1)
      (let ((child1 (make-text "http://www.mauve.com"))
	    (child2 (make-processing-instruction
		     "child" "http://www.mauve.com"))
	    (child3 (make-comment "http://www.mauve.com")))
	(append-child parent child1)
	(append-child parent child2)
	(append-child parent child3)
	(assert-equal parent (parent child3))
	(assert-equal parent (parent child1))
	(assert-equal parent (parent child2))
	(delete-children parent)
	(assert-equal nil (list-children parent))
	(assert-equal nil (parent child1))
	(assert-equal nil (parent child2))
	(assert-equal nil (parent child3))
	(assert-equal parent (parent a1))
	(values))))

(deftest element.attribute-value
    (let* ((name "sakjdhjhd")
	   (e (make-element name)))
      (assert-equal nil (attribute-value e "test"))
      (assert-equal
       nil
       (attribute-value e "base" "http://www.w3.org/XML/1998/namespace"))
      (add-attribute e (make-attribute "value" "test"))
      (add-attribute e (make-attribute
			"http://www.example.com/"
			"xml:base"
			"http://www.w3.org/XML/1998/namespace"))
      (assert-equal "value" (attribute-value e "test"))
      (assert-equal
       "http://www.example.com/"
       (attribute-value e "base" "http://www.w3.org/XML/1998/namespace"))
      ;; (assert-equal nil (attribute-value e "xml:base"))
      (assert-equal nil (attribute-value e "base"))
      (assert-equal
       nil
       (attribute-value e "test" "http://www.w3.org/XML/1998/namespace"))
      (values)))

(deftest element.setf.attribute-value
    (let* ((e (make-element "sakjdhjhd"))
	   (f (copy e))
	   (g (copy e)))
      (add-attribute f (make-attribute "1" "pre:foo" "http://pre"))
      (add-attribute g (make-attribute "2" "pre:foo" "http://pre"))
      ;; add attribute
      (setf (attribute-value e "pre:foo" "http://pre") "1")
      (assert-node= e f)
      ;; change existing attribute
      (setf (attribute-value e "pre:foo" "http://pre") "2")
      (assert-node= e g)
      (values)))

(deftest element.setf.attribute-value.2
    (let ((e (make-element "pre:sakjdhjhd" "http://pre")))
      (setf (attribute-value e "pre:flubba") "value")
      (assert-equal (namespace-uri (car (list-attributes e)))
		    "http://pre")
      (values)))

(deftest element.map-attributes
    (let* ((e (make-element "sakjdhjhd")))
      (add-attribute e (make-attribute "1" "pre:foo" "http://pre"))
      (add-attribute e (make-attribute "2" "pre:bar" "http://pre"))
      (assert-equal (list-attributes e) (map-attributes 'list #'identity e))
      (assert-equal (mapcar #'qualified-name (list-attributes e))
		    (map-attributes 'list #'qualified-name e))
      (assert (equalp (map 'vector #'qualified-name (list-attributes e))
		      (map-attributes 'vector #'qualified-name e)))
      (values)))

(deftest element.with-attributes
     (let* ((e (make-element "sakjdhjhd")))
       (add-attribute e (make-attribute "1" "pre:foo" "http://pre"))
       (add-attribute e (make-attribute "2" "bar"))
      (add-attribute e (make-attribute "gorilla" "ape"))
       (with-attributes ((foo "pre:foo" "http://pre")
                        (bar "bar")
                       (baz "pre:baz"  "http://pre")
                        moose
                        ape)
          e
        (setf foo (format nil "<~A>" foo))
        (setf bar (string #\newline))
       (setf baz "pre:xyz")
        (setf moose "mangy")
        (assert-equal ape "gorilla"))
       (assert-equal (attribute-value e "foo" "http://pre") "<1>")
       (assert-equal (attribute-value e "bar") (string #\newline))
       (assert-equal (attribute-value e "baz" "http://pre") "pre:xyz")
      (assert-equal (attribute-value e "moose") "mangy")
       (values)))

 (deftest element.find-attribute-named
    (let* ((name "sakjdhjhd")
	   (e (make-element name)))
      (assert-equal nil (find-attribute-named e "test"))
      (assert-equal
       nil
       (find-attribute-named e "base" "http://www.w3.org/XML/1998/namespace"))
      (let ((a1 (make-attribute "value" "test"))
	    (a2 (make-attribute
		 "http://www.example.com/"
		 "xml:base"
		 "http://www.w3.org/XML/1998/namespace")))
	(add-attribute e a1)
	(add-attribute e a2)
	(assert-equal a1 (find-attribute-named e "test"))
	(assert-equal
	 a2
	 (find-attribute-named e
			       "base"
			       "http://www.w3.org/XML/1998/namespace")))
      (values)))

(deftest element.find-namespace.empty.1
    (find-namespace "" (make-element "sakjdhjhd"))
  "")

(deftest element.find-namespace.empty.2
    (find-namespace nil (make-element "sakjdhjhd"))
  "")

(deftest element.namespace-prefix.1
    (namespace-prefix (make-element "html"))
  "")

(deftest element.namespace-prefix.2
    (let ((test (make-element
		 "xml:base"
		 "http://www.w3.org/XML/1998/namespace")))
      (assert-equal "xml" (namespace-prefix test))
      (assert-equal "http://www.w3.org/XML/1998/namespace"
		    (namespace-uri test))
      (assert-equal "xml:base" (qualified-name test))
      (values)))

(define-condition-test element.namespace-prefix.3
    (make-element "xml:base" "http://www.example.org/")
  stp-error)

(define-condition-test element.namespace-prefix.4
    (make-element "test:base" "http://www.w3.org/XML/1998/namespace")
  stp-error)

(define-condition-test element.name.1
    (make-element "")
  stp-error)

(define-condition-test element.name.2
    (make-element "1Kelvin")
  stp-error)

(define-condition-test element.name.3
    (make-element nil)
  type-error)

(deftest element.print-object
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (base-uri "http://www.example.com/")
	   (e (make-element name uri)))
      (add-extra-namespace e "blue" "http://www.blue.com")
      (add-extra-namespace e "green" "http://www.green.com")
      (let ((a1 (make-attribute "test" "test"))
	    (a2 (make-attribute "data" "pre1:green" "http://www.green.com"))
	    (a3 (make-attribute "data"
				"yellow:sfsdadf"
				"http://www.yellow.com/")))
	(add-attribute e a1)
	(add-attribute e a2)
	(add-attribute e a3))
      (append-child e (make-element "mv:child" "http://www.mauve.com"))
      (let ((e3 (make-element "mv:child" "http://www.mauve.com")))
	(prepend-child e e3)
	(append-child e3 (make-element "mv:child" "http://www.mauve.com")))
      (setf (base-uri e) base-uri)
      (assert-node= e (read-from-string (write-to-string e)))
      (values)))

(deftest element.map-extra-namespaces
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (blue "http://www.blue.com")
	   (green "http://www.green.com")
	   (e (make-element name uri)))
      (add-extra-namespace e "blue" blue)
      (add-extra-namespace e "green" green)
      (setf (attribute-value e "pre1:green" green) "data")
      (let ((bluep nil)
	    (greenp nil))
	(map-extra-namespaces
	 (lambda (prefix uri)
	   (cond
	     ((equal prefix "blue")
	      (assert (not bluep))
	      (setf bluep t)
	      (assert-equal uri blue))
	     ((equal prefix "green")
	      (assert (not greenp))
	      (setf greenp t)
	      (assert-equal uri green))
	     (t
	      (error "bogus namespace"))))
	 e))
      (values)))

(deftest element.base-uri
    (let* ((root (make-element "root"))
	   (child (make-element "child"))
	   (document (make-document root)))
      (append-child root child)
      (assert-equal (base-uri document) "")
      (setf (base-uri root) "file://etc")
      (setf (base-uri child) "passwd")
      (assert (puri:uri= (puri:parse-uri "file://etc/passwd")
			 (base-uri child)))
      (values)))

(deftest element.root
    (let* ((de (make-element "root"))
	   (child (make-element "child")))
      (append-child de child)
      (assert-equal de (root child))
      (assert-equal de (root de))
      (let ((document (make-document de)))
	(assert-equal document (root de))
	(assert-equal document (root child))
	(assert-equal document (root document)))
      (values)))


;;;; ATTRIBUTE

(defmacro with-ATTRIBUTE-test ((&optional) &body body)
  `(let* ((a1 (make-attribute "value" "test"))
	  (a2 (make-attribute "  value  " "test")))
     ,@body))

(deftest attribute.count-children
    (with-attribute-test ()
      (child-count a1))
  0)

(define-condition-test attribute.nth-child
    (with-attribute-test ()
      (nth-child 0 a1))
  error)

(deftest attribute.make-attribute
    (with-attribute-test ()
      (assert-equal "test" (local-name a1))
      (assert-equal "test" (qualified-name a1))
      (assert-equal "" (namespace-prefix a1))
      (assert-equal "" (namespace-uri a1))
      (assert-equal "value" (value a1))
      (assert-equal "  value  " (value a2))
      (values)))

(deftest attribute.setf.local-name
    (let ((a (make-attribute "value" "name")))
      (setf (local-name a) "newname")
      (assert-equal "newname" (local-name a))
      (expect-condition (setf (local-name a) "pre:a") stp-error)
      (values)))

(deftest attribute.setf.local-name.2
    (let ((a (make-attribute "value" "pre:name" "http://www.example.org")))
      (setf (local-name a) "newname")
      (assert-equal "newname" (local-name a))
      (assert-equal "pre:newname" (qualified-name a))
      (values)))

(define-condition-test attribute.xmlns.1
    (make-attribute "http://www.w3.org/TR" "xmlns")
  stp-error)

(define-condition-test attribute.xmlns.2
    (make-attribute "http://www.w3.org/TR" "xmlns:prefix")
  stp-error)

(define-condition-test attribute.xmlns.3
    (make-attribute "http://www.w3.org/"
		    "xmlns"
		    "http://www.w3.org/2000/xmlns/")
  stp-error)

(define-condition-test attribute.xmlns.4
    (make-attribute "http://www.w3.org/"
		    "xmlns:pre"
		    "http://www.w3.org/2000/xmlns/")
  stp-error)

(deftest attribute.xml-base
    (let* ((xml-namespace "http://www.w3.org/XML/1998/namespace")
	   (a1 (make-attribute "http://www.w3.org/" "xml:base" xml-namespace)))
      (assert-equal "base" (local-name a1))
      (assert-equal "xml:base" (qualified-name a1))
      (assert-equal xml-namespace (namespace-uri a1))
      (dolist (str '("http://www.example.com/>"
		     "http://www.example.com/<"
		     #.(format nil "http://www.example.com/~C"
			(code-char #x00FE))))
	(setf (value a1) str)
	(assert-equal (value a1) str))
      (values)))

(deftest attribute.xml-prefix
    (progn
      (expect-condition (make-attribute "http://www.w3.org/" "xml:base")
			stp-error)
      (expect-condition (make-attribute "preserve" "xml:space")
			stp-error)
      (expect-condition (make-attribute "fr-FR" "xml:lang")
			stp-error)
      (let* ((xml-namespace "http://www.w3.org/XML/1998/namespace")
	     (a1 (make-attribute "http://www.w3.org/"
				 "xml:base"
				 xml-namespace)))
	(assert-equal "base" (local-name a1))
	(assert-equal "xml:base" (qualified-name a1))
	(assert-equal xml-namespace (namespace-uri a1))
	(let ((a2 (make-attribute "preserve"
				  "xml:space"
				  xml-namespace)))
	  (assert-equal "space" (local-name a2))
	  (assert-equal "xml:space" (qualified-name a2))
	  (assert-equal xml-namespace (namespace-uri a2)))
	(let ((a3 (make-attribute "en-UK"
				  "xml:lang"
				  xml-namespace)))
	  (assert-equal "lang" (local-name a3))
	  (assert-equal "xml:lang" (qualified-name a3))
	  (assert-equal xml-namespace (namespace-uri a3)))
	(expect-condition
	 (make-attribute "http://www.w3.org/"
			 "xml:base"
			 "http://www.notTheXMLNamespace")
	 stp-error)
	(values))))

(deftest attribute.xml-lang
    (let* ((xml-namespace "http://www.w3.org/XML/1998/namespace")
	   (a1 (make-attribute "" "xml:lang" xml-namespace)))
      (assert-equal "" (value a1))
      (values)))

(define-condition-test attribute.xml-uri
    (make-attribute "value" "test:base" "http://www.w3.org/XML/1998/namespace")
  stp-error)

(deftest attribute.serialize.1
    (let ((e (make-element "test")))
      (add-attribute e (make-attribute "<" "a1"))
      (add-attribute e (make-attribute ">" "a2"))
      (add-attribute e (make-attribute "\"" "a3"))
      (add-attribute e (make-attribute "'" "a4"))
      (add-attribute e (make-attribute "&" "a5"))
      (serialize-to-string e))
  "<test a5=\"&amp;\" a4=\"'\" a3=\"&quot;\" a2=\"&gt;\" a1=\"&lt;\"/>")

(deftest attribute.serialize.2
    (let ((e (make-element "test")))
      (add-attribute e (make-attribute (string (code-char 32)) "a1"))
      (add-attribute e (make-attribute (string (code-char 10)) "a2"))
      (add-attribute e (make-attribute (string (code-char 13)) "a3"))
      (add-attribute e (make-attribute (string (code-char 9)) "a4"))
      (serialize-to-string e))
  "<test a4=\"&#9;\" a3=\"&#13;\" a2=\"&#10;\" a1=\" \"/>")

(deftest attribute.serialize.3
    (let ((e (make-element "test")))
      (add-attribute e (make-attribute "=,.!@#$%^*()_-'[]{}+/?;:`|\\" "a"))
      (serialize-to-string e))
"<test a=\"=,.!@#$%^*()_-'[]{}+/?;:`|\\\"/>")

(deftest attribute.setf.value
    (let ((a (make-attribute "test" "test")))
      (dolist (legal '("Hello"
		       "hello there"
		       "  spaces on both ends  "
		       " quotes \" \" quotes"
		       " single \'\' quotes"
		       " both double and single \"\'\"\' quotes" 
		       " angle brackets <  > <<<"
		       " carriage returns \r\r\r"
		       " ampersands & &&& &name; "))
	(setf (value a) legal)
	(assert-equal (value a) legal))
      (expect-condition (setf (value a) (string (code-char 0))) stp-error)
      (values)))

(deftest attribute.names
    (let* ((prefix "testPrefix")
	   (name "testName")
	   (uri "http://www.elharo.com/")
	   (value "  here's some data")
	   (qname (concatenate 'string prefix ":" name))
	   (a1 (make-attribute value qname uri)))
      (assert-equal name (local-name a1))
      (assert-equal qname (qualified-name a1))
      (assert-equal uri (namespace-uri a1))
      (values)))

(deftest attribute.copy.1
    (let* ((c1 (make-attribute "data" "test"))
	   (c2 (copy c1)))
      (assert-equal (value c1) (value c2))
      (assert-equal (local-name c1) (local-name c2))
      (assert-equal (qualified-name c1) (qualified-name c2))
      (assert (not (eql c1 c2)))
      (assert-equal nil (parent c2))
      (values)))

;;; fixme: testSurrogates

(deftest attribute.rename-attribute.1
    (let ((a (make-attribute "data" "red:prefix" "http://www.example.com")))
      (rename-attribute a nil nil)
      (assert-equal "" (namespace-uri a))
      (assert-equal "" (namespace-prefix a))
      (values)))

(deftest attribute.rename-attribute.2
    (let ((a (make-attribute "data" "red:prefix" "http://www.example.com"))
	  (e (make-element "pre:test" "http://www.example.org/")))
      (add-attribute e a)
      (rename-attribute a "pre" "http://www.example.org/")
      (assert-equal "http://www.example.org/" (namespace-uri a))
      (assert-equal "pre" (namespace-prefix a))
      (assert-equal "http://www.example.org/" (namespace-uri e))
      (assert-equal "pre" (namespace-prefix e))
      (values)))

(deftest attribute.rename-attribute.3
    (let* ((name "red:sakjdhjhd")
	   (uri "http://www.red.com/")
	   (prefix "red")
	   (a (make-attribute "" name uri)))
      (assert-equal uri (namespace-uri a))
      (dolist (legal *legal-uris*)
	(rename-attribute a prefix legal)
	(assert-equal legal (namespace-uri a)))
      (dolist (illegal *very-illegal-uris*)
	(expect-condition (rename-attribute a prefix illegal) stp-error))
      (values)))

(deftest attribute.rename-attribute.4
    (let ((a (make-attribute "value" "name")))
      (expect-condition (rename-attribute a "pre" "") stp-error)
      (expect-condition (rename-attribute a "" "http://www.example.com")
			stp-error)
      (values)))

(deftest attribute.rename-attribute.5
    (let ((element (make-element "test"))
	  (a (make-attribute "bar" "pre:foo" "http://pre.com"))
	  (b (make-attribute "bar" "post:bar" "http://post.com")))
      (add-attribute element a)
      (add-attribute element b)
      (expect-condition (rename-attribute b "pre" "http://post.com")
			stp-error)
      (values)))

(deftest attribute.node-properties
    (let ((a (make-attribute "data" "test")))
      (assert-equal nil (parent a))
      (let ((element (make-element "test")))
	(add-attribute element a)
	(assert-equal element (parent a))
	(assert-equal a (find-attribute-named element "test"))
	(remove-attribute element a)
	(assert-equal nil (find-attribute-named element "test")))
      (values)))

(deftest attribute.constraints
    (let ((element (make-element "test"))
	  (a1 (make-attribute "valueFoo" "foo:data" "http://www.example.com"))
	  (a2 (make-attribute "valueBar" "bar:data" "http://www.example.com"))
	  (a3 (make-attribute "valueFoo" "data"))
	  (a4 (make-attribute "valueBar" "data")))
      ;; add initial attribute
      (add-attribute element a1)
      (assert-equal "valueFoo"
		    (attribute-value element "data" "http://www.example.com"))
      (assert-equal (list a1) (list-attributes element))
      ;; replace it
      (add-attribute element a2)
      (assert-equal "valueBar"
		    (attribute-value element "data" "http://www.example.com"))
      (assert-equal (list a2) (list-attributes element))
      ;; add a different one
      (add-attribute element a3)
      (assert-equal "valueFoo" (attribute-value element "data"))
      (assert-equal "valueBar"
		    (attribute-value element "data" "http://www.example.com"))
      (assert-equal 2 (length (list-attributes element)))
      ;; replace
      (add-attribute element a4)
      (assert-equal "valueBar" (attribute-value element "data"))
      (assert-equal "valueBar"
		    (attribute-value element "data" "http://www.example.com"))
      (assert-equal 2 (length (list-attributes element)))
      ;; different prefix
      (let ((a5 (make-attribute "valueRed" "red:ab" "http://www.example.org"))
	    (a6 (make-attribute
		 "valueGreen" "green:cd" "http://www.example.org")))
	(add-attribute element a5)
	(add-attribute element a6)
	(assert-equal "valueRed"
		      (attribute-value element "ab" "http://www.example.org"))
	(assert-equal "valueGreen"
		      (attribute-value element "cd" "http://www.example.org")))
      (values)))

(deftest attribute.double-add
    (let ((element (make-element "test"))
	  (a (make-attribute "bar" "foo")))
      (add-attribute element a)
      (remove-attribute element a)
      (let ((copy (copy element)))
	(add-attribute copy (make-attribute "newvalue" "a"))
	(assert-equal 1 (length (list-attributes copy))))
      (values)))

(deftest attribute.string-value
    (string-value (make-attribute "bar" "foo"))
  "bar")

(define-condition-test attribute.serialize
    (serialize (make-attribute "bar" "foo") nil)
  stp-error)

(deftest attribute.print-object
    (let ((a (make-attribute "bar" "pre:foo" "http://uri")))
      (assert-node= a (read-from-string (write-to-string a)))
      (values)))


;;;; PARENT-NODE

(defmacro with-parent-node-test ((&optional) &body body)
  `(let* ((empty (make-element "empty"))
	  (not-empty (make-element "not-empty"))
	  (child (make-comment "Hello")))
     (append-child not-empty child)
     ,@body))

(deftest parent-node.detach
    (with-parent-node-test ()
      (let ((text (make-text "This will be attached then detached")))
	(append-child empty text)
	(assert-equal empty (parent text))
	(detach text)
	(assert-equal nil (parent text))
	(values))))

(deftest parent-node.append-child
    (with-parent-node-test ()
      (let ((child (make-element "test")))
	(append-child empty child)
	(assert-equal (parent child) empty)
	(assert-equal (list-children empty) (list child))
	(detach child)
	(append-child not-empty child)
	(assert (not (eql (nth-child 0 not-empty) child)))
	(assert-equal (nth-child 1 not-empty) child)
	(values))))

(define-condition-test parent-node.append-child.2
    (let ((child (make-element "test")))
      (append-child child child))
  stp-error)

(deftest parent-node.append-child.3
    (let ((a (make-element "test"))
	  (b (make-element "test")))
      (append-child a b)
      (expect-condition (append-child b a) stp-error)
      (values)))

(deftest parent-node.insert-child
    (let ((parent (make-element "parent"))
	  (child1 (make-element "child"))
	  (child2 (make-element "child2"))
	  (child3 (make-element "child3"))
	  (child4 (make-element "child4"))
	  (child5 (make-element "child5")))
      ;; into empty
      (insert-child parent child1 0)
      (assert (plusp (child-count parent)))
      (assert-equal 0 (child-position child1 parent))
      ;; at beginning
      (insert-child parent child2 0)
      (assert-equal 0 (child-position child2 parent))
      (assert-equal 1 (child-position child1 parent))
      ;; in middle
      (insert-child parent child3 1)
      (assert-equal 0 (child-position child2 parent))
      (assert-equal 1 (child-position child3 parent))
      (assert-equal 2 (child-position child1 parent))
      ;; at beginning with children
      (insert-child parent child4 0)
      (assert-equal 0 (child-position child4 parent))
      (assert-equal 1 (child-position child2 parent))
      (assert-equal 2 (child-position child3 parent))
      (assert-equal 3 (child-position child1 parent))
      ;; at end with children
      (insert-child parent child5 4)
      (assert-equal 0 (child-position child4 parent))
      (assert-equal 1 (child-position child2 parent))
      (assert-equal 2 (child-position child3 parent))
      (assert-equal 3 (child-position child1 parent))
      (assert-equal 4 (child-position child5 parent))
      ;; nil
      (expect-condition (insert-child parent nil 0) error)
      (values)))

(define-condition-test parent-node.append-child.4
    (with-parent-node-test ()
      (append-child empty (make-document not-empty)))
  stp-error)

(define-condition-test parent-node.append-child.5
    (with-parent-node-test ()
      (append-child empty child))
  stp-error)

(deftest parent-node.replace-child
    (with-parent-node-test ()
      (let ((old1 (make-element "old1"))
	    (old2 (make-element "old2"))
	    (old3 (make-element "old3"))
	    (new1 (make-element "new1"))
	    (new2 (make-element "new2"))
	    (new3 (make-element "new3")))
	(append-child empty old1)
	(append-child empty old2)
	(append-child empty old3)
	(replace-child empty old1 new1)
	(replace-child empty old3 new3)
	(replace-child empty old2 new2)
	(assert-equal (list new1 new2 new3) (list-children empty))
	(expect-condition (replace-child empty new1 nil) error)
	(expect-condition (replace-child empty old1 nil) error)
	(let ((new4 (make-element "new4")))
	  (expect-condition (replace-child empty new4 (make-element "test"))
			    stp-error))
	(replace-child empty new1 new1)
	(assert-equal new1 (nth-child 0 empty))
	(assert-equal empty (parent new1))
	(expect-condition (replace-child empty new1 new2) stp-error))
      (values)))

(deftest parent-node.child-position
    (with-parent-node-test ()
      (let ((child1 (make-element "old1"))
	    (child2 (make-text "old2"))
	    (child3 (make-comment "old3")))
	(assert-equal nil (child-position child1 empty))
	(append-child empty child1)
	(append-child empty child2)
	(append-child empty child3)
	(assert-equal 0 (child-position child1 empty))
	(assert-equal 1 (child-position child2 empty))
	(assert-equal 2 (child-position child3 empty))
	(assert-equal nil (child-position empty empty))
	(assert-equal nil (child-position (make-text "test") empty)))
      (values)))

(deftest parent-node.nth-child
    (with-parent-node-test ()
      (let ((old1 (make-element "old1"))
	    (old2 (make-element "old2"))
	    (old3 (make-comment "old3")))
	(expect-condition (nth-child 0 empty) error)
	(append-child empty old1)
	(append-child empty old2)
	(append-child empty old3)
	(assert-equal old1 (nth-child 0 empty))
	(assert-equal old2 (nth-child 1 empty))
	(assert-equal old3 (nth-child 2 empty))
	(expect-condition (nth-child 5 empty) error))
      (values)))

(deftest parent-node.delete-child
    (with-parent-node-test ()
      (expect-condition (delete-nth-child 0 empty) error)
      (let ((old1 (make-element "old1"))
	    (old2 (make-element "old2"))
	    (old3 (make-element "old3")))
	(expect-condition (delete-child old1 empty) error)
	(append-child empty old1)
	(append-child empty old2)
	(append-child empty old3)
	(delete-nth-child 1 empty)
	(assert-equal old1 (nth-child 0 empty))
	(assert-equal old3 (nth-child 1 empty))
	(delete-nth-child 1 empty)
	(delete-nth-child 0 empty)
	(assert-equal nil (parent old2))
	(assert-equal nil (list-children empty))
	(append-child empty old1)
	(append-child empty old2)
	(append-child empty old3)
	(assert-equal (list old1 old2 old3) (list-children empty))
	(delete-child old3 empty)
	(delete-child old1 empty)
	(delete-child old2 empty)
	(assert-equal nil (list-children empty))
	(assert-equal nil (parent old1)))
      (values)))

(deftest parent-node.replace-child.2
    (with-parent-node-test ()
      (let ((old1 (make-element "old1"))
	    (old2 (make-element "old2"))
	    (old3 (make-element "old3"))
	    (new1 (make-element "new1"))
	    (new3 (make-element "new3")))
	(append-child empty old1)
	(append-child empty old2)
	(expect-condition (replace-child empty old3 new3) stp-error)
	(expect-condition (replace-child empty old1 nil) error)
	(expect-condition (replace-child empty nil new1) error))
      (values)))

(deftest parent-node.replace-child.3
    (with-parent-node-test ()
      (let ((test1 (make-element "test1"))
	    (test2 (make-element "test2")))
	(expect-condition (replace-child empty test1 test2) stp-error))
      (values)))

(deftest parent-node.replace-child.4
    (with-parent-node-test ()
      (let ((parent (make-element "parent"))
	    (test1 (make-element "test1"))
	    (test2 (make-element "test2")))
	(append-child parent test1)
	(append-child parent test2)
	(expect-condition (replace-child parent test1 test2) stp-error)
	(assert-equal (list test1 test2) (list-children parent)))
      (values)))

(deftest parent-node.insert-child.2
    (with-parent-node-test ()
      (let ((parent (make-element "parent"))
	    (test1 (make-element "test1"))
	    (test2 (make-element "test2")))
	(append-child parent test1)
	(append-child parent test2)
	(expect-condition (insert-child parent test2 0) stp-error)
	(expect-condition (insert-child parent test2 1) stp-error)
	(assert-equal (list test1 test2) (list-children parent)))
      (values)))

(deftest parent-node.replace-child.4
    (with-parent-node-test ()
      (let ((parent (make-element "parent"))
	    (child (make-element "child")))
	(append-child parent child)
	(expect-condition
	 (replace-child parent child (make-document-type "root"))
	 stp-error)
	(let ((e (make-element "e"))
	      (child2 (make-text "child2")))
	  (append-child e child2)
	  (expect-condition (replace-child parent child child2) stp-error)))
      (values)))

(deftest node.insert-child-before.1
    (let ((parent (make-element "parent"))
	  (a (make-element "child"))
	  (b (make-text "text"))
	  (new1 (make-text "new"))
	  (new2 (make-text "new2")))
      (expect-condition (insert-child-before parent new1 a) stp-error)
      (expect-condition (insert-child-before parent new2 b) stp-error)
      (append-child parent a)
      (append-child parent b)
      (insert-child-before parent new1 a)
      (insert-child-before parent new2 b)
      (assert-equal (list new1 a new2 b) (list-children parent))
      (values)))

(deftest node.insert-child-after.1
    (let ((parent (make-element "parent"))
	  (a (make-element "child"))
	  (b (make-text "text"))
	  (new1 (make-text "new1"))
	  (new2 (make-text "new2")))
      (expect-condition (insert-child-after parent new1 a) stp-error)
      (expect-condition (insert-child-after parent new2 b) stp-error)
      (append-child parent a)
      (append-child parent b)
      (insert-child-after parent new1 a)
      (insert-child-after parent new2 b)
      (assert-equal (list a new1 b new2) (list-children parent))
      (values)))



;;;; NODE

(deftest node.first-child.1
    (let ((parent (make-element "parent"))
	  (a (make-element "child"))
	  (b (make-text "text")))
      (assert-equal nil (first-child parent))
      (append-child parent a)
      (append-child parent b)
      (assert-equal a (first-child parent))
      (detach a)
      (detach b)
      (assert-equal nil (first-child parent))
      (values)))

(deftest node.last-child.1
    (let ((parent (make-element "parent"))
	  (a (make-element "child"))
	  (b (make-text "text")))
      (assert-equal nil (last-child parent))
      (append-child parent a)
      (append-child parent b)
      (assert-equal b (last-child parent))
      (detach a)
      (detach b)
      (assert-equal nil (last-child parent))
      (values)))

(deftest node.previous-sibling.1
    (let ((parent (make-element "parent"))
	  (a (make-element "child"))
	  (b (make-text "text")))
      (expect-condition (previous-sibling a) stp-error)
      (expect-condition (previous-sibling b) stp-error)
      (append-child parent a)
      (append-child parent b)
      (expect-condition (previous-sibling a) stp-error)
      (assert-equal a (previous-sibling b))
      (values)))

(deftest node.next-sibling.1
    (let ((parent (make-element "parent"))
	  (a (make-element "child"))
	  (b (make-text "text")))
      (expect-condition (next-sibling a) stp-error)
      (expect-condition (next-sibling b) stp-error)
      (append-child parent a)
      (append-child parent b)
      (assert-equal b (next-sibling a))
      (expect-condition (next-sibling b) stp-error)
      (values)))

(defmacro with-sequence-test ((&optional) &body body)
  `(let ((e (make-element "test"))
	 (a (make-element "a"))
	 (a2 (make-element "a"))
	 (b (make-element "b"))
	 (s (make-text "foo"))
	 (x (make-element "x")))
     (declare (ignorable x))
     (append-child e a)
     (append-child e a2)
     (append-child e b)
     (append-child e s)
     ,@body))

(defun maybe-local-name (x)
  (typecase x
    (element (local-name x))
    (t nil)))

(deftest node.count-children.0
    (with-sequence-test ()
      (count-children x e))
  0)

(deftest node.count-children.1
    (with-sequence-test ()
      (count-children a e))
  1)

(deftest node.count-children.2
    (with-sequence-test ()
      (count-children a e :test #'eql))
  1)

(deftest node.count-children.3
    (with-sequence-test ()
      (count-children a e :test 'eql))
  1)

(deftest node.count-children.4
    (with-sequence-test ()
      (count-children "a" e :key #'maybe-local-name :test #'equal))
  2)

(deftest node.count-children.5
    (with-sequence-test ()
      (count-children "a" e :key 'maybe-local-name :test 'equal))
  2)

(deftest node.count-children.6
    (with-sequence-test ()
      (count-children (copy-seq "a") e :key 'maybe-local-name))
  0)

(deftest node.count-children.7
    (with-sequence-test ()
      (count-children a e :from-end t))
  1)

(deftest node.count-children.8
    (let ((seen '()))
      (with-sequence-test ()
	(prog1
	    (count-children a e :key (lambda (c) (push c seen) c))
	  (assert-equal seen (list s b a2 a)))))
  1)

(deftest node.count-children.9
    (let ((seen '()))
      (with-sequence-test ()
	(prog1
	    (count-children a e :from-end t :key (lambda (c) (push c seen) c))
	  (assert-equal seen (list a a2 b s)))))
  1)

(deftest node.count-children.10
    (with-sequence-test ()
      (count-children "a"
		      e
		      :key 'maybe-local-name
		      :test 'equal
		      :start 1))
  1)

(deftest node.count-children.11
    (with-sequence-test ()
      (count-children "a"
		      e
		      :key 'maybe-local-name
		      :test 'equal
		      :end 1))
  1)

(deftest node.count-children.12
    (with-sequence-test ()
      (count-children "a"
		      e
		      :key 'maybe-local-name
		      :test 'equal
		      :start 1 :end 3))
  1)

(deftest node.count-children.13
    (with-sequence-test ()
      (count-children "a"
		      e
		      :key 'maybe-local-name
		      :test 'equal
		      :start 1
		      :end nil))
  1)

(deftest node.count-children.14
    (with-sequence-test ()
      (count-children "a" e :key 'maybe-local-name :test 'equal :end nil))
  2)

(deftest node.count-children.15
    (with-sequence-test ()
      (count-children "a" e :test (constantly t) :start 1))
  3)

(deftest node.count-children-if.1
    (with-sequence-test ()
      (count-children-if #'identity e))
  4)

(deftest node.count-children-if.2
    (with-sequence-test ()
      (count-children-if (alexandria:of-type 'element) e))
  3)

(deftest node.count-children-if.3
    (with-sequence-test ()
      (count-children-if #'break a))
  0)

(deftest node.count-children-if.4
    (with-sequence-test ()
      (count-children-if (lambda (x) (equal x "a"))
			 e
			 :key 'maybe-local-name))
  2)

(deftest node.count-children-if.5
    (with-sequence-test ()
      (count-children-if 'identity e :key 'identity))
  4)

(deftest node.count-children-if.8
  (count-if #'identity '(a b nil c d nil e) :key 'not)
  2)

(deftest node.count-children-if.9
  (count-if #'evenp '(1 2 3 4 4 1 8 10 1))
  5)

(deftest node.count-children-if.10
  (count-if #'evenp '(1 2 3 4 4 1 8 10 1) :key #'1+)
  4)

(deftest node.count-children-if.11
    (let ((seen '()))
      (with-sequence-test ()
	(prog1
	    (count-children-if (constantly t)
			       e
			       :key (lambda (c) (push c seen) c))
	  (assert-equal seen (list b a2 a)))))
  4)

(deftest node.count-children-if.12
    (let ((seen '()))
      (with-sequence-test ()
	(prog1
	    (count-children-if (constantly t)
			       e
			       :from-end t
			       :key (lambda (c) (push c seen) c))
	  (assert-equal seen (list a a2 b)))))
  4)

(deftest node.count-children-if.10
    (with-sequence-test ()
      (count-children-if (lambda (x) (equal x "a"))
			 e
			 :key 'maybe-local-name
			 :start 1))
  1)

(deftest node.count-children-if.11
    (with-sequence-test ()
      (count-children-if (lambda (x) (equal x "a"))
			 e
			 :key 'maybe-local-name
			 :end 1))
  1)

(deftest node.count-children-if.12
    (with-sequence-test ()
      (count-children-if (lambda (x) (equal x "a"))
			 e
			 :key 'maybe-local-name
			 :start 1 :end 3))
  1)

(deftest node.count-children-if.13
    (with-sequence-test ()
      (count-children-if (lambda (x) (equal x "a"))
			 e
			 :key 'maybe-local-name
			 :start 1
			 :end nil))
  1)

;;;; FIXME: find-child-if, child-position, filter-children

(deftest node.map-recursively.1
    (let* ((root (make-element "foo"))
	   (child (make-element "bar"))
	   (text (make-text "bla"))
	   (document (make-document root)))
      (setf (attribute-value child "ignoreme") "value")
      (append-child root child)
      (append-child root text)
      (assert-equal
       (let ((seen nil))
	 (map-recursively (lambda (x) (push x seen)) document)
	 (nreverse seen))
       (list document root child text))
      (values)))

(deftest node.do-recursively.1
    (let* ((root (make-element "foo"))
	   (child (make-element "bar"))
	   (text (make-text "bla"))
	   (document (make-document root)))
      (setf (attribute-value child "ignoreme") "value")
      (append-child root child)
      (append-child root text)
      (assert-equal
       (let ((seen nil))
	 (do-recursively (x document (nreverse seen))
	   (push x seen)))
       (list document root child text))
      (values)))

(deftest node.find-recursively.1
    (let* ((root (make-element "foo"))
	   (child (make-element "bar"))
	   (text (make-text "bla"))
	   (document (make-document root)))
      (setf (attribute-value child "ignoreme") "value")
      (append-child root child)
      (append-child root text)
      (assert-equal (find-recursively 'text document :key #'type-of)
		    text)
      (assert-equal (find-recursively "element"
				      document
				      :test 'string-equal
				      :key #'type-of)
		    root)
      (values)))

(deftest node.filter-recursively.1
    (let* ((root (make-element "foo"))
	   (child (make-element "bar"))
	   (text (make-text "bla"))
	   (document (make-document root)))
      (setf (attribute-value child "ignoreme") "value")
      (append-child root child)
      (append-child root text)
      (assert-equal (filter-recursively (lambda (x) (eq x 'text))
					document
					:key #'type-of)
		    (list text))
      (assert-equal (filter-recursively (lambda (x) (string-equal x "element"))
					document
					:key #'type-of)
		    (list root child))
      (values)))


;;;; BUILDER

;;;; the XML Test suite is a good test for the builder, so we need only few
;;;; tests here

(deftest builder.extra-namespaces
    (serialize-to-string
     (document-element
      (cxml:parse #1="<b xmlns:extra=\"http://because-it.s-extra/\"/>"
		  (make-builder))))
  #1#)


(do-tests)

;; next: testRemoveNonElementChildren
