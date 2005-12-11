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
    :components
    ((:file "package")
     (:file "definline" :depends-on ("package"))
     (:file runes
            :pathname
             #-rune-is-character "runes"
             #+rune-is-character "characters"
	    :depends-on ("package" "definline"))
     (:file "syntax" :depends-on ("package" "definline" runes))
     (:file "encodings" :depends-on ("package"))
     (:file "encodings-data" :depends-on ("package" "encodings"))
     (:file "xstream"
            :depends-on ("package" "definline" "syntax" "encodings-data"))))

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
     (:file "catalog"         :depends-on ("xml-parse"))
     (:file "sax-proxy"       :depends-on ("xml-parse")))
    :depends-on (:cxml-runes :puri :trivial-gray-streams))

(asdf:defsystem :cxml-dom
    :default-component-class closure-source-file
    :pathname (merge-pathnames
               "dom/"
               (make-pathname :name nil :type nil :defaults *load-truename*))
    :components
    ((:file "package")
     (:file "dom-impl"        :depends-on ("package"))
     (:file "dom-builder"     :depends-on ("dom-impl"))
     (:file "unparse"         :depends-on ("package"))
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
