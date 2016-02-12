(in-package :fxml.sax)

;;; Make FXML stop complaining about CXML handlers.

(defmethod deprecated-sax-default-method ((handler sax:abstract-handler) event)
  (declare (ignore event)))

;;; Make CXML stop complaining about FXML handlers.

(defclass abstract-handler (sax-parser-mixin sax:abstract-handler)
  ())
