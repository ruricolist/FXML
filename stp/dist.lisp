(asdf:operate 'asdf:load-op :atdoc)
(asdf:operate 'asdf:load-op :fxml.stp)

(atdoc:generate-html-documentation
 '(:fxml.stp)
 (merge-pathnames
  "doc/"
  (asdf:component-relative-pathname (asdf:find-system :fxml.stp)))
 :heading "fxml.stp")
