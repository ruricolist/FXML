(in-package #:fxml.test)

(def-suite fxml)

(in-suite fxml)

(test parse-rod
  (finishes (fxml:parse-rod "<div>hello <i>world</i></div>" nil)))

(def-suite defused-xml :in fxml)

(in-suite defused-xml)

(test simple-parse (finishes (parse-test-file xml-simple)))
(test simple-parse-ns (finishes (parse-test-file xml-simple-ns)))

(test entities-forbidden
  (flet ((forbidden (file)
           (signals fxml:entities-forbidden
             (parse-test-file file))))
    (forbidden xml-bomb)
    (forbidden xml-quadratic)
    (forbidden xml-external)))

(test entity-cycle
  (signals fxml:well-formedness-violation
    (parse-test-file xml-cyclic :forbid-entities nil)))

(test dtd-forbidden
  (flet ((forbidden (file)
           (signals fxml:dtd-forbidden
             (parse-test-file file :forbid-dtd t))))
    (forbidden xml-bomb)
    (forbidden xml-quadratic)
    (forbidden xml-external)
    (forbidden xml-dtd)))

(test dtd/external-ref
  (signals fxml:external-reference-forbidden
    (parse-test-file xml-dtd)))

(test external-ref
  (signals fxml:external-reference-forbidden
    (parse-test-file xml-external :forbid-entities nil)))

(test external-file-ref
  (signals fxml:external-reference-forbidden
    (parse-test-file xml-external-file :forbid-entities nil)))

(test allow-expansion
  (finishes (parse-test-file xml-bomb2 :forbid-entities nil)))

(in-suite fxml)

(test dtd-embedding
  ;; https://stackoverflow.com/questions/26738465/non-valid-output-of-broadcast-handler-in-common-lisp-closure-xml-package/28528117#28528117
  (let ((teste
          (with-output-to-string (out)
            (let ((h (make-instance 'fxml:sax-proxy :chained-handler (fxml:make-character-stream-sink out))))
              (fxml:parse (test-file-path xml-harem) h
                          :validate t
                          :forbid-external nil)))))
    (is (equal teste (read-file-into-string (test-file-path xml-teste))))))

(test modus
  (flet ((xml-test (path dtd-path)
           (flet ((resolver (pubid sysid)
                    (declare (ignorable pubid sysid))
                    (open dtd-path :element-type '(unsigned-byte 8))))
             (fxml.klacks:with-open-source
                 (s (fxml:make-source path :validate t :entity-resolver #'resolver
                                           :forbid-external nil))
               (loop for key = (fxml.klacks:peek s) while key do
                 (case key
                   (:start-element
                    (format t ":start-element ~a~%" (fxml.klacks:current-qname s)))
                   (:end-element
                    (format t ":end-element ~a~%" (fxml.klacks:current-qname s)))
                   (t
                    (format t "key: ~a~%" key)))
                 (fxml.klacks:consume s))))))
    (finishes
      (xml-test (test-file-path #p"modus/xml-test.xml")
                (test-file-path  #p"modus/xml-test.dtd")))))

(def-suite xpath :in fxml)

(in-suite xpath)

(test xpath-stp
  (let ((xpath-test:*dom-builder* (fxml.stp:make-builder))
        (xpath-test:*document-element* #'fxml.stp:document-element))
    (xpath-test:run-all-tests)))

(test xpath-dom
  (let ((xpath-test:*dom-builder* (fxml.rune-dom:make-dom-builder))
        (xpath-test:*document-element* #'fxml.dom:document-element))
    (xpath-test:run-all-tests)))

(def-suite relax-ng :in fxml)

(in-suite relax-ng)

(test rng-sax
  (finishes
    (fxml:parse (test-file-path "address-book.xml")
                (cxml-rng:make-validator
                 (cxml-rng:parse-schema
                  (test-file-path "address-book.rng"))
                 (fxml-dom:make-dom-builder)))))

(test rng-klacks
  (finishes
    (fxml.klacks:with-open-source
        (s (cxml-rng:make-validating-source
            (test-file-path "address-book.xml")
            (cxml-rng:parse-schema (test-file-path "address-book.rng"))))
      (loop for key = (fxml.klacks:peek-next s)
            while key
            count 1 into count
            finally (is (= 29 count))))))


(def-suite xmlconf :in fxml)

(in-suite xmlconf)

(defun intern* (name pkg)
  (intern (string name) pkg))

(defun run-xmlconf-suite (name impl)
  (let ((fxml.xmlconf::*debug-tests* *debug-on-error*))
    (ecase impl
      (:cxml
       (fxml.xmlconf:with-cxml ()
         (fxml.xmlconf:run-all-tests name)))
      (:fxml
       (fxml.xmlconf:with-fxml ()
         (fxml.xmlconf:run-all-tests name))))))

(defun compare-results (fn)
  (handler-bind ((warning #'muffle-warning))
    (let ((fxml-results (run-xmlconf-suite fn :fxml))
          (cxml-results (run-xmlconf-suite fn :cxml)))
      (is-true fxml-results)
      (is-true cxml-results)
      (assert (= (length fxml-results) (length cxml-results)))
      (loop for fxml-result in fxml-results
            for cxml-result in cxml-results
            do (unless
                   (or
                    ;; We don't care about cases where QURI is stricter than PURI.
                    (search "bad uri" fxml-result)
                    ;; Conversely, we also don't care about cases that
                    ;; include Windows paths that PURI balks at.
                    (search "bad uri" cxml-result)
                    ;; Ignore some CXML breakage for the moment.
                    (search "duplicate internal subset" cxml-result)
                    )
                 (is (equal fxml-result cxml-result)))))))

(test sax
  (compare-results 'fxml.xmlconf:sax-test))

(test klacks
  (compare-results 'fxml.xmlconf:klacks-test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test copied from Plexippus XPath

;;; Copyright (c) 2007 Ivan Shvedunov. All rights reserved.
;;; Copyright (c) 2007 David Lichteblau. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(test fxml.xmls
  (let ((xpath:*navigator* (fxml.xmls:make-xpath-navigator))
        (d
          '("foo" (("a" "b"))
            " "
            ("a" (("id" "1")))
            " " ("b" (("id" "2")))
            " " ("c" (("id" "3")))
            " " ("a" (("id" "4")))
            " " ("b" (("id" "5")))
            " " ("c" (("id" "6")))
            " " ("a" (("id" "7")))
            " " ("b" (("id" "8")))
            " " ("c" (("id" "9")))
            " " ("last" NIL))))
    (is (equal
         '(("a" (("id" "1"))) ("c" (("id" "6"))))
         (xpath:all-nodes (xpath:evaluate "//c[position()=2]|//a[@id='1']" d))))))
