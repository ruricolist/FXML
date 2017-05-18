(in-package :fxml.sax)

;;; Make FXML stop complaining about CXML handlers.

(defmethod deprecated-sax-default-method ((handler sax:abstract-handler) event)
  (declare (ignore event)))

;;; Make CXML stop complaining about FXML handlers.

(defmacro without-redefinition-warnings (&body body)
  `(serapeum:nest
    ;; Suppress redefinition warnings.
    #+ccl (let (#+ccl ccl::*warn-if-redefine*))
    #+sbcl (locally
               (declare (sb-ext:muffle-conditions
                         sb-ext:compiler-note
                         sb-ext::style-warning)))
    ,@body))

(without-redefinition-warnings
  (defclass abstract-handler (sax-parser-mixin sax:abstract-handler)
    ()))
