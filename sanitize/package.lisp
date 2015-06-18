;;;; package.lisp

(defpackage #:fxml.sanitize
  (:use #:cl #:alexandria #:serapeum)
  (:export #:define-sanitize-mode
           #:wrap-sanitize
           #:mode
           #:default
           #:restricted
           #:basic
           #:relaxed
           #:sanitize))
