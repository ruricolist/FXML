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
