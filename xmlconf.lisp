(defpackage xmlconf
  (:use :cl)
  (:alias (:string-dom :dom)))
(in-package :xmlconf)

(defun test-xml-conformance (directory)
  (let ((xmlconf (xml:parse-file (merge-pathnames "xmlconf.xml" directory))))
    (dolist (test (dom:get-elements-by-tag-name xmlconf "test"))
      (when (equal (dom:get-attribute test "TYPE") "valid")
        (let* ((base (dom:get-attribute (dom:parent-node test) "xml:base"))
               (uri (dom:get-attribute test "URI")))
          (unless base
            (inspect test))
          (princ uri)
          (handler-case
              (progn
                (xml:parse-file
                 (merge-pathnames uri (merge-pathnames base directory)))
                (format t " ok~%"))
            (serious-condition (c)
              (format t " FAILED:~%  ~A~%[~A]~%"
                      c
                      (dom:data (car (dom:child-nodes test)))))))))))
