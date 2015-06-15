(defpackage :cxml-stp-system
  (:use :asdf :cl))
(in-package :cxml-stp-system)

(defclass closure-source-file (cl-source-file) ())

#+sbcl
(defmethod perform :around ((o compile-op) (s closure-source-file))
  ;; shut up already.  Correctness first.
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (let ((*compile-print* nil))
      (call-next-method))))

(defsystem :cxml-stp
    :default-component-class closure-source-file
    :serial t
    :components
    ((:file "package")
     (:file "classes")
     (:file "node")
     (:file "parent-node")
     (:file "leaf-node")
     (:file "document")
     (:file "element")
     (:file "attribute")
     (:file "document-type")
     (:file "comment")
     (:file "processing-instruction")
     (:file "text")
     (:file "builder")
     (:file "xpath"))
    :depends-on (:cxml :alexandria :xpath))

(defsystem :cxml-stp-test
    :default-component-class closure-source-file
    :serial t
    :components
    ((:file "test"))
    :depends-on (:cxml-stp :rt))
