(in-package #:cl-user)

(defpackage #:cxml.test
  (:use #:cl #:5am))

(in-package #:cxml.test)

(macrolet ((def (name value)
             `(define-symbol-macro ,name (pathname ,value))))
  (def xml-dtd "dtd.xml")
  (def xml-external "external.xml")
  (def xml-external-file "external_file.xml")
  (def xml-quadratic "quadratic.xml")
  (def xml-simple "simple.xml")
  (def xml-simple-ns "simple-ns.xml")
  (def xml-bomb "xmlbomb.xml")
  (def xml-bomb2 "xmlbomb2.xml")
  (def xml-cyclic "cyclic.xml"))

(defun test-file-path (name)
  (asdf:system-relative-pathname
   :cxml
   (merge-pathnames name #p"test/xmltestdata/")))

(defun parse-test-file (name &rest args)
  (apply #'cxml:parse (test-file-path name) nil args))

(defun debug-test (test &key (error t) (failure t))
  "Run TEST, breaking on error or failure."
  (let ((5am:*debug-on-error* error)
        (5am:*debug-on-failure* failure))
    (run! test)))
