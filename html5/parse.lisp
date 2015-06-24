(in-package #:fxml.html5)

(defun parse (input handler &key fragment (encoding :utf-8))
  (let ((dom
          (if fragment
              (html5-parser:parse-html5-fragment input :encoding encoding)
              (html5-parser:parse-html5 input :encoding encoding))))
    (serialize-dom dom handler)))
