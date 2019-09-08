(in-package #:fxml.cxml)

(defun extract-vars-for-call (lambda-list)
  (multiple-value-bind (required optional rest keywords)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (declare (ignore rest))
    (append required
            (mapcar #'car optional)
            (loop for ((keyword-name name) nil nil) in keywords
                  append `(,keyword-name ,name)))))

(defmacro define-translation (spec &key cxml-class fxml-class
                                        cxml-package fxml-package)
  (flet ((find-symbol* (name package)
           (multiple-value-bind (found status)
               (find-symbol (string name) package)
             (unless found
               (error "No such symbol as ~a in ~a" name package))
             (unless (eql status :external)
               (error "~s is not external in ~a" found package))
             found)))
    (destructuring-bind (name (instance . lambda-list)) spec
      (let ((args (extract-vars-for-call lambda-list))
            (cxml-name (find-symbol* name cxml-package))
            (fxml-name (find-symbol* name fxml-package)))
        `(progn
           (defmethod ,cxml-name ((,instance ,fxml-class) ,@lambda-list)
             (handler-bind ((warning #'muffle-warning))
               (,fxml-name ,instance ,@args)))
           (defmethod ,fxml-name ((,instance ,cxml-class) ,@lambda-list)
             (handler-bind ((warning #'muffle-warning))
               (,cxml-name ,instance ,@args))))))))
