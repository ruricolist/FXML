(defpackage :fxml-system
  (:use :asdf :cl))
(in-package :fxml-system)

(defsystem :fxml/runes
  :serial t
  :pathname "runes/"
  :components
  ((:file "package")
   (:file "definline")
   (:file "characters")
   (:file "syntax")
   (:file "encodings")
   (:file "encodings-data")
   (:file "xstream")
   (:file "ystream"))
  :depends-on (:babel
               :named-readtables
               :trivial-gray-streams))

(asdf:defsystem :fxml/xml
  :pathname "xml/"
  :components
  ((:file "package")
   (:file "util"            :depends-on ("package"))
   (:file "sax-handler")
   (:file "xml-name-rune-p" :depends-on ("package" "util"))
   (:file "xml-parse"       :depends-on ("package" "util" "sax-handler" "xml-name-rune-p"))
   (:file "unparse"         :depends-on ("xml-parse"))
   (:file "xmls-compat"     :depends-on ("xml-parse"))
   (:file "recoder"         :depends-on ("xml-parse"))
   (:file "xmlns-normalizer" :depends-on ("xml-parse"))
   (:file "space-normalizer" :depends-on ("xml-parse"))
   (:file "catalog"         :depends-on ("xml-parse"))
   (:file "sax-proxy"       :depends-on ("xml-parse"))
   (:file "atdoc-configuration" :depends-on ("package")))
  :depends-on (:fxml/runes
               :quri
               :flexi-streams
               :alexandria
               :serapeum
               :split-sequence))

(asdf:defsystem :fxml/dom
    :pathname "dom/"
    :components
    ((:file "package")
     (:file "dom-impl" :depends-on ("package"))
     (:file "dom-builder" :depends-on ("dom-impl"))
     (:file "dom-sax"         :depends-on ("package")))
    :depends-on (:fxml/xml))

(asdf:defsystem :fxml/klacks
    :pathname "klacks/"
    :serial t
    :components
    ((:file "package")
     (:file "klacks")
     (:file "klacks-impl")
     (:file "tap-source"))
    :depends-on (:fxml/xml))

(asdf:defsystem :fxml/test
  :pathname "test/"
  :serial t
  :perform (test-op (o c) (uiop:symbol-call :fxml.test :run-tests))
  :components ((:file "test")
               (:file "xmlconf")
               (:file "suite"))
  :depends-on (:uiop
               :fiveam
               :fxml/xml :fxml/klacks :fxml/dom
               :cxml :cxml-dom :cxml-klacks))

(asdf:defsystem :fxml
  :components ()
  :in-order-to ((test-op (test-op :fxml/test)))
  :depends-on (:fxml/dom :fxml/klacks))

(defsystem :fxml/stp
  :serial t
  :in-order-to ((test-op (test-op :fxml/stp/test)))
  :pathname "stp/"
  :components
  ((:file "package")
   (:file "classes")
   (:file "node")
   (:file "parent-node")
   (:file "leaf-node")
   (:file "document")
   (:file "element")
   (:file "document-fragment")
   (:file "attribute")
   (:file "document-type")
   (:file "comment")
   (:file "processing-instruction")
   (:file "text")
   (:file "builder")
   #+ () (:file "xpath")
   (:file "dom"))
  :depends-on (:fxml :alexandria :xpath))

(defsystem :fxml/stp/test
  :serial t
  :perform (test-op (o c) (uiop:symbol-call :fxml.stp.test :run-tests))
  :pathname "stp/"
  :components
  ((:file "test"))
  :depends-on (:fxml/stp :rt))

(asdf:defsystem :fxml/html5
  :serial t
  :description "Bridge HTML5 and FXML"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :pathname "html5/"
  :depends-on (:alexandria
               :serapeum
               :fset
               :fxml
               :fxml/stp
               :cl-html5-parser
               :quri)
  :components ((:file "package")
               (:file "html5-sax")
               (:file "sink")
               (:file "parse")))

(asdf:defsystem :fxml/css-selectors
  :description "Bridge css-selectors and FXML."
  :pathname "css/"
  :depends-on (:fxml :fxml/stp :css-selectors)
  :components ((:file "dom")
               (:file "stp")))

(asdf:defsystem :fxml/cxml
  :description "Bridge FXML and CXML."
  :pathname "cxml/"
  :depends-on (:fxml :cxml)
  :components ((:file "package")
               (:file "protocol" :depends-on ("package"))
               (:file "attributes" :depends-on ("package"))
               (:file "class" :depends-on ("package"))))

(defsystem :fxml/sanitize
  :serial t
  :description "Streaming HTML sanitizer"
  :author "Paul M. Rodriguez"
  :license "LLGPL"
  :depends-on (:alexandria :serapeum :fxml :quri)
  :pathname "sanitize/"
  :in-order-to ((test-op (test-op :fxml/sanitize/test)))
  :components ((:file "package")
               (:file "sax-sanitize")))

(defsystem :fxml/sanitize/test
  :depends-on (:fxml/sanitize :fiveam :fxml/html5)
  :pathname "sanitize/"
  :perform (test-op (o c) (uiop:symbol-call :fxml.sanitize.test :run-tests))
  :components ((:file "test")))

