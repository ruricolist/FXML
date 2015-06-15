(in-package :cl-user)

(make :fxml.stp)
(require :sb-cover)

(let ((d (slot-value (asdf:find-system :fxml.stp) 'asdf::relative-pathname)))
  (flet ((clean ()
	   (mapc #'delete-file
		 (directory
		  (make-pathname :name :wild :type "fasl" :defaults d)))))
    (clean)
    (declaim (optimize sb-cover:store-coverage-data))
    (make :fxml.stp-test)
    (sb-cover:report (merge-pathnames "report/" d))
    (declaim (optimize (sb-cover:store-coverage-data 0)))
    (clean)
    (make :fxml.stp)))
