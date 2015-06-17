(in-package #:fxml.html5)

(defun parse (input &optional handler)
  (serialize-dom (html5-parser:parse-html5 input) handler))
