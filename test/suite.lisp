(in-package #:cxml.test)

(def-suite cxml)

(in-suite cxml)

(def-suite defused-xml :in cxml)

(in-suite defused-xml)

(test simple-parse (finishes (parse-test-file xml-simple)))
(test simple-parse-ns (finishes (parse-test-file xml-simple-ns)))

(test entities-forbidden
  (flet ((forbidden (file)
           (signals cxml:entities-forbidden
             (parse-test-file file))))
    (forbidden xml-bomb)
    (forbidden xml-quadratic)
    (forbidden xml-external)))

(test entity-cycle
  (signals cxml:well-formedness-violation
    (parse-test-file xml-cyclic :forbid-entities nil)))

(test dtd-forbidden
  (flet ((forbidden (file)
           (signals cxml:dtd-forbidden
             (parse-test-file file :forbid-dtd t))))
    (forbidden xml-bomb)
    (forbidden xml-quadratic)
    (forbidden xml-external)
    (forbidden xml-dtd)))

(test dtd/external-ref
  (signals cxml:external-reference-forbidden
    (parse-test-file xml-dtd)))

(test external-ref
  (signals cxml:external-reference-forbidden
    (parse-test-file xml-external :forbid-entities nil)))

(test external-file-ref
  (signals cxml:external-reference-forbidden
    (parse-test-file xml-external-file :forbid-entities nil)))

(test allow-expansion
  (finishes (parse-test-file xml-bomb2 :forbid-entities nil)))
