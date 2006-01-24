;;; XXX Die vielen verschiedenen Systeme hier sollten vielleicht
;;; Module eines grossen Systems CXML werden?

(defpackage :cxml-system
  (:use :asdf :cl))
(in-package :cxml-system)

(defclass closure-source-file (cl-source-file) ())

#+sbcl
(defmethod perform :around ((o compile-op) (s closure-source-file))
  ;; shut up already.  Correctness first.
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (let (#+sbcl (*compile-print* nil))
      (call-next-method))))

#-(or rune-is-character rune-is-integer)
(progn
  (format t "~&;;; Checking for wide character support...")
  (force-output)
  (pushnew (dotimes (x 65536
                      (progn
                        (format t " ok, characters have at least 16 bits.~%")
                        :rune-is-character))
             (unless (and (< x char-code-limit) (code-char x))
               (format t " no, reverting to octet strings.~%")
               (return :rune-is-integer)))
           *features*))

#-rune-is-character
(format t "~&;;; Building cxml with (UNSIGNED-BYTE 16) RUNES~%")

#+rune-is-character
(format t "~&;;; Building cxml with CHARACTER RUNES~%") 

(defsystem :cxml-runes
    :default-component-class closure-source-file
    :pathname (merge-pathnames
               "runes/"
               (make-pathname :name nil :type nil :defaults *load-truename*))
    :serial t
    :components
    ((:file "package")
     (:file "definline")
     (:file runes
            :pathname
             #-rune-is-character "runes"
             #+rune-is-character "characters")
     #+rune-is-integer (:file "utf8")
     (:file "syntax")
     (:file "encodings")
     (:file "encodings-data")
     (:file "xstream")
     (:file "ystream")))

(asdf:defsystem :cxml-xml
    :default-component-class closure-source-file
    :pathname (merge-pathnames
               "xml/"
               (make-pathname :name nil :type nil :defaults *load-truename*))
    :components
    ((:file "package")
     (:file "util"            :depends-on ("package"))
     (:file "sax-handler")
     (:file "characters"      :depends-on ("package"))
     (:file "xml-name-rune-p" :depends-on ("package"))
     (:file "split-sequence"  :depends-on ("package"))
     (:file "xml-parse"       :depends-on ("package" "util" "sax-handler" "split-sequence" "xml-name-rune-p" "characters"))
     (:file "unparse"         :depends-on ("xml-parse"))
     (:file "xmls-compat"     :depends-on ("xml-parse"))
     (:file "recoder"         :depends-on ("xml-parse"))
     (:file "xmlns-normalizer" :depends-on ("xml-parse"))
     (:file "space-normalizer" :depends-on ("xml-parse"))
     (:file "catalog"         :depends-on ("xml-parse"))
     (:file "sax-proxy"       :depends-on ("xml-parse")))
    :depends-on (:cxml-runes :puri :trivial-gray-streams))

(defclass utf8dom-file (closure-source-file) ((of)))

(defmethod output-files ((operation compile-op) (c utf8dom-file))
  (let* ((normal (car (call-next-method)))
	 (name (concatenate 'string (pathname-name normal) "-utf8"))
	 (of (make-pathname :name name :defaults normal)))
    (setf (slot-value c 'of) of)
    (list of)))

(defmethod perform ((o load-op) (c utf8dom-file))
  (load (slot-value c 'of)))

(defvar *utf8-runes-readtable*)

(defmethod perform ((operation compile-op) (c utf8dom-file))
  (let ((*features* (cons 'utf8dom-file *features*))
	(*readtable* *utf8-runes-readtable*))
    (call-next-method)))

(asdf:defsystem :cxml-dom
    :default-component-class closure-source-file
    :pathname (merge-pathnames
               "dom/"
               (make-pathname :name nil :type nil :defaults *load-truename*))
    :components
    ((:file "package")
     (:file rune-impl :pathname "dom-impl" :depends-on ("package"))
     (:file rune-builder :pathname "dom-builder" :depends-on (rune-impl))
     #+rune-is-integer
     (utf8dom-file utf8-impl :pathname "dom-impl" :depends-on ("package"))
     #+rune-is-integer
     (utf8dom-file utf8-builder :pathname "dom-builder" :depends-on (utf8-impl))
     (:file "dom-sax"         :depends-on ("package")))
    :depends-on (:cxml-xml))

(asdf:defsystem :cxml-test
    :default-component-class closure-source-file
    :pathname (merge-pathnames
               "test/"
               (make-pathname :name nil :type nil :defaults *load-truename*))
    :components ((:file "domtest") (:file "xmlconf"))
    :depends-on (:cxml-xml :cxml-dom))

(asdf:defsystem :cxml :components () :depends-on (:cxml-dom :cxml-test))
