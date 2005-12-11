(in-package :cxml)

(defun %unparse-document (sink doc canonical)
  (when sax:*namespace-processing*
    (setf sink (cxml:make-namespace-normalizer sink)))
  (dom:map-document sink
		    doc
		    :include-doctype (if (and canonical (>= canonical 2))
					 :canonical-notations
					 nil)
		    :include-default-values t))

(defun unparse-document-to-octets (doc &rest initargs &key canonical)
  (%unparse-document (apply #'make-octet-vector-sink initargs)
		     doc
		     canonical))

(defun unparse-document (doc character-stream &rest initargs &key canonical)
  (%unparse-document
   (apply #'make-character-stream-sink character-stream initargs)
   doc
   canonical))
