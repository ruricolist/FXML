(in-package :cxml)

(defun unparse-document-to-octets (doc &rest initargs)
  (let ((sink (apply #'make-octet-vector-sink initargs)))
    (dom:map-document sink doc :include-default-values t)))

(defun unparse-document (doc character-stream &rest initargs)
  (let ((sink (apply #'make-character-stream-sink character-stream initargs)))
    (dom:map-document sink doc :include-default-values t)))
