(defpackage :cxml-system
  (:use :asdf :cl))
(in-package :cxml-system)

(defclass closure-source-file (cl-source-file) ())

#+sbcl
(defmethod perform :around ((o compile-op) (s closure-source-file))
  ;; shut up already.  Correctness first.
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (call-next-method)))

(unless (find-package :glisp)
  (defpackage :glisp))

(defsystem glisp
    :default-component-class closure-source-file
    :pathname (merge-pathnames
               "glisp/"
               (make-pathname :name nil :type nil :defaults *load-truename*))
    :components
    ((:file dependent
	    :pathname
	    #+CLISP                             "dep-clisp"
	    #+(AND :CMU (NOT :PTHREAD))         "dep-cmucl"
	    #+sbcl                              "dep-sbcl"
	    #+(AND :CMU :PTHREAD)               "dep-cmucl-dtc"
	    #+(and allegro allegro-v5.0)        "dep-acl5"
	    #+(and allegro (not allegro-v5.0))  "dep-acl"
	    #+GCL                               "dep-gcl"
	    #-(or sbcl CLISP CMU allegro GCL) #.(error "Configure!"))
     (:file "package"
	    :depends-on (dependent))
     (:file "runes"
	    :depends-on ("package" dependent))
     (:file "util"
	    :depends-on ("package" dependent "runes"))
     (:file "match"
	    :depends-on ("package" dependent "runes" "util"))))

(asdf:defsystem :cxml
    :default-component-class closure-source-file
    :pathname (merge-pathnames
               "cxml/"
               (make-pathname :name nil :type nil :defaults *load-truename*))
    :components
    ((:file "package")
     (:file "encodings"       :depends-on ("package"))
     (:file "encodings-data"  :depends-on ("package" "encodings"))
     (:file "sax-handler")
     (:file "dompack")
     (:file "dom-impl"        :depends-on ("dompack"))
     (:file "dom-builder"     :depends-on ("dom-impl" "sax-handler"))
     (:file "xml-stream"      :depends-on ("package"))
     (:file "xml-name-rune-p" :depends-on ("package"))
     (:file "xml-parse"       :depends-on ("package" "dompack" "sax-handler"))
     (:file "xml-canonic"     :depends-on ("package" "dompack" "xml-parse")))
    :depends-on (:glisp))
