(defpackage xmlconf
  (:use :cl :runes)
  (:export #:run-all-tests))
(in-package :xmlconf)

(defun get-attribute (element name)
  (rod-string (dom:get-attribute element name)))

(defun relevant-test-p (test)
  (and (equal (get-attribute test "TYPE") "valid")
       (let ((version (get-attribute test "RECOMMENDATION")))
         (cond
           ((or (equal version "")      ;XXX
                (equal version "XML1.0"))
             (cond
               ((equal (get-attribute test "NAMESPACE") "no")
                 (format t "~A: test applies to parsers without namespace support, skipping~%"
                       (get-attribute test "URI"))
                 nil)
               (t
                 t)))
           ((equal version "XML1.1")
             ;; not supported
             nil)
           (t
             (warn "unrecognized RECOMMENDATION value: ~S" version)
             nil)))))

(defun test-pathnames (directory test)
  (let* ((sub-directory
          (loop
              for parent = test then (dom:parent-node parent)
              for base = (get-attribute parent "xml:base")
              until (plusp (length base))
              finally (return (merge-pathnames base directory))))
         (uri (get-attribute test "URI"))
         (output (get-attribute test "OUTPUT")))
    (values (merge-pathnames uri sub-directory)
            (when (plusp (length output))
              (merge-pathnames output sub-directory)))))

(defun serialize-document (document)
  (map 'vector #'char-code
       (with-output-to-string (s)
         (xml:unparse-document document s))))

(defun file-contents (pathname)
  (with-open-file (s pathname)
    (let ((result
           (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence result s )
      result)))

(defun run-all-tests (directory)
  (let* ((pathname (merge-pathnames "xmlconf.xml" directory))
         (builder (dom:make-dom-builder))
         (xmlconf (xml:parse-file pathname builder))
         (ntried 0)
         (nfailed 0)
         (nskipped 0))
    (dom:do-node-list (test (dom:get-elements-by-tag-name xmlconf "TEST"))
      (cond
        ((relevant-test-p test)
          (incf ntried)
          (multiple-value-bind (pathname output)
              (test-pathnames directory test)
            (princ pathname)
            (unless (probe-file pathname)
              (error "file not found: ~A" pathname))
            (with-simple-restart (skip-test "Skip this test")
              (handler-case
                  (progn
                    (mp:with-timeout (60)
                      (let ((document
                             (xml:parse-file pathname (dom:make-dom-builder))))
                        (cond
                          ((null output)
                            (format t " ok (output not checked)~%"))
                          ((equalp (file-contents output)
                                   (serialize-document document))
                            (format t " ok~%"))
                          (t
                            (let ((error-output
                                   (make-pathname :type "error" :defaults output)))
                              (with-open-file (s error-output
                                               :direction :output
                                               :if-exists :supersede)
                                (write-sequence (serialize-document document) s))
                              (error "well-formed, but output ~S not the expected ~S~%"
                                     error-output output)))))))
                ((and serious-condition (not excl:interrupt-signal)) (c)
                  (incf nfailed)
                  (format t " FAILED:~%  ~A~%[~A]~%"
                          c
                          (rod-string
                           (dom:data
                            (dom:item (dom:child-nodes test) 0)))))))))
        (t
          (incf nskipped))))
    (format t "~&~D/~D tests failed; ~D test~:P were skipped"
            nfailed ntried nskipped)))

#+(or)
(xmlconf::run-all-tests "/mnt/debian/space/xmlconf/")
