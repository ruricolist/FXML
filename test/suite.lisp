(in-package #:fxml.test)

(def-suite fxml)

(in-suite fxml)

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
  (let ((fxml-results (run-xmlconf-suite fn :fxml))
        (cxml-results (run-xmlconf-suite fn :cxml)))
    (is-true fxml-results)
    (is-true cxml-results)
    (is (= (length fxml-results) (length cxml-results)))
    (loop for fxml-result in fxml-results
          for cxml-result in cxml-results
          do (is (equal fxml-result cxml-result)))))

(test sax
  (compare-results 'fxml.xmlconf:sax-test))

(test klacks
  (compare-results 'fxml.xmlconf:klacks-test))
