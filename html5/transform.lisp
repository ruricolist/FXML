(in-package #:fxml.html5)

(defmethod html5-parser:transform-html5-dom ((to-type (eql 'xhtml)) node &key)
  (serialize-dom node (fxml:make-string-sink)))

(defmethod html5-parser:transform-html5-dom ((to-type (eql 'dom)) node &key)
  (serialize-dom node (fxml-dom:make-dom-builder)))

(defmethod html5-parser:transform-html5-dom ((to-type (eql 'stp)) node &key)
  (serialize-dom node (fxml.stp:make-builder)))

(defmethod html5-parser:transform-html5-dom ((to-type (eql 'xmls)) node &key)
  (serialize-dom node (fxml.xmls:make-xmls-builder)))
