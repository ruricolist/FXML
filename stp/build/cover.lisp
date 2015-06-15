(in-package :cl-user)

(make :cxml-stp)
(require :sb-cover)

(let ((d (slot-value (asdf:find-system :cxml-stp) 'asdf::relative-pathname)))
  (flet ((clean ()
	   (mapc #'delete-file
		 (directory
		  (make-pathname :name :wild :type "fasl" :defaults d)))))
    (clean)
    (declaim (optimize sb-cover:store-coverage-data))
    (make :cxml-stp-test)
    (sb-cover:report (merge-pathnames "report/" d))
    (declaim (optimize (sb-cover:store-coverage-data 0)))
    (clean)
    (make :cxml-stp)))
