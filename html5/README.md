FXML.HTML5 is a replacement for [CHTML][chtml]. It bridges
[CL-HTML5-PARSER][html5] and FXML by taking the DOM returned by the
HTML5 parser and serializing it to SAX events.

     (defun html->xhtml (html)
       (fxml.html5:serialize-dom (html5-parser:parse-html5 html)
                                 (fxml:make-string-sink)))

HTML5-SAX understands namespaces, the `lang` attribute, and the `base`
element, and handles the corner cases where XML is more strict than
HTML.

It also replaces `chtml:make-string-sink` for serialization:

     (fxml.stp:serialize dom (fxml.html5:make-html5-sink))

This produces an HTML5 document with a UTF-8 charset declaration.

[html5]: https://github.com/copyleft/cl-html5-parser
[chtml]: http://common-lisp.net/project/closure/closure-html/
