;;;; package.lisp

(defpackage #:fxml.html5
  (:use #:cl #:alexandria #:serapeum)
  (:export #:serialize-dom #:make-html5-sink #:close-sink
           #:xhtml #:dom #:stp #:xmls))
