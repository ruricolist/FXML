(in-package #:fxml.html5)

(defmethod html5-parser:transform-html5-dom ((to-type (eql 'xhtml)) node &key)
  (serialize-dom node (cxml:make-string-sink)))

(defmethod html5-parser:transform-html5-dom ((to-type (eql 'dom)) node &key)
  (serialize-dom node (cxml-dom:make-dom-builder)))

(defmethod html5-parser:transform-html5-dom ((to-type (eql 'stp)) node &key)
  (serialize-dom node (cxml-stp:make-builder)))

(defmethod html5-parser:transform-html5-dom ((to-type (eql 'xmls)) node &key)
  (serialize-dom node (cxml-xmls:make-xmls-builder)))
