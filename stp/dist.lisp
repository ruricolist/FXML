(asdf:operate 'asdf:load-op :atdoc)
(asdf:operate 'asdf:load-op :cxml-stp)

(atdoc:generate-html-documentation
 '(:cxml-stp)
 (merge-pathnames
  "doc/"
  (asdf:component-relative-pathname (asdf:find-system :cxml-stp)))
 :heading "cxml-stp")
