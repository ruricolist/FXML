;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: GCL dependent stuff + fixups
;;;   Created: 1999-05-25 22:31
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1999 by Gilbert Baumann

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

(shadow '(make-pathname pathname-directory) :glisp)

(export '(glisp::defun 
          glisp::read-byte-sequence 
          glisp::read-char-sequence 
          glisp::define-compiler-macro 
          glisp::formatter
          glisp::destructuring-bind
          glisp::parse-macro
          glisp::loop
          glisp::*print-readably*
          glisp::compile-file-pathname
          glisp::ignore-errors
          glisp::pathname-directory
          glisp::make-pathname
          glisp::run-unix-shell-command)
        :glisp)

(defmacro glisp::defun (name args &body body)
  (cond ((and (consp name)
              (eq (car name) 'setf))
         (let ((fnam (intern (concatenate 'string "(SETF " (symbol-name (cadr name)) ")")
                             (symbol-package (cadr name)))))
           `(progn
              (defsetf ,(cadr name) (&rest ap) (new-value) (list* ',fnam new-value ap))
              (defun ,fnam ,args .,body))))
        (t
         `(defun ,name ,args .,body)) ))

(defun glisp::read-byte-sequence (sequence input &key (start 0) (end (length sequence)))
  (let (c (i start))
    (loop
      (cond ((= i end) (return i)))
      (setq c (read-byte input nil :eof))
      (cond ((eql c :eof) (return i)))
      (setf (aref sequence i) c)
      (incf i) )))

(defun glisp::read-char-sequence (sequence input &key (start 0) (end (length sequence)))
  (let (c (i start))
    (loop
      (cond ((= i end) (return i)))
      (setq c (read-char input nil :eof))
      (cond ((eql c :eof) (return i)))
      (setf (aref sequence i) c)
      (incf i) )))
  
(defmacro glisp::define-compiler-macro (&rest ignore)
  ignore
  nil)

(defun glisp::formatter (string)
  #'(lambda (sink &rest ap)
      (apply #'format sink string ap)))

(defmacro lambda (&rest x)
  `#'(lambda .,x))


(defun glisp::row-major-aref (array index)
  ;; Wir sollten hier wirklich was effizienteres haben
  (aref (make-array (array-total-size array)
                    :displaced-to array
                    :element-type (array-element-type array))
        index))

(glisp::defun (setf glisp::row-major-aref) (value array index)
  ;; Wir sollten hier wirklich was effizienteres haben
  (setf (aref (make-array (array-total-size array)
                          :displaced-to array
                          :element-type (array-element-type array))
              index)
    value))

(defun glisp::mp/make-lock (&key name)
  name
  nil)

(defmacro glisp::mp/with-lock ((lock) &body body)
  (declare (ignore lock))
  `(progn
     ,@body))

(defmacro glisp::with-timeout ((&rest ignore) &body body)
  (declare (ignore ignore))
  `(progn
     ,@body))

(defvar glisp::*print-readably* nil)

(defun glisp::g/make-string (length &rest options)
  (apply #'make-array length :element-type 'string-char options))

(defun parse-macro-lambda-list (name lambda-list whole &optional environment-value (real-whole whole))
  "The work horse for destructing-bind and parse-macro."
  (let ((orig-lambda-list lambda-list)
	required optionals rest-var keys aux-vars whole-var env-var
	allow-other-keys-p
	(my-lambda-list-keywords '(&OPTIONAL &REST &KEY &AUX &BODY)))

    (labels ((COLLECT (&optional on-keys-p)
	       (let (result)
		 (do ()
		     ((or (atom lambda-list) (member (car lambda-list) my-lambda-list-keywords))
		      (nreverse result))
		     (cond ((eq (car lambda-list) '&WHOLE)
			    (push (cadr lambda-list) whole-var)
			    (setf lambda-list (cddr lambda-list)))
			   ((eq (car lambda-list) '&ENVIRONMENT)
			    (push (cadr lambda-list) env-var)
			    (setf lambda-list (cddr lambda-list)))
			   ((eq (car lambda-list) '&ALLOW-OTHER-KEYS)
			    (unless on-keys-p
			      (cerror "Ignore this syntax restriction and set the allow-other-keys-p flag."
				      "In lambda list of macro ~S: &ALLOW-OTHER-KEYS may only be specified ~
                                       in the &KEYS section: ~S"
				      name orig-lambda-list))
			    (setq allow-other-keys-p T lambda-list (cdr lambda-list)))			   
			   (T (push (pop lambda-list) result)) ))) )
	     
	     (CHECK-ONLY-ONE (kind lst)
	       (unless (<= (length lst) 1)
		 (error "In lambda list of macro ~S: You may only specify one ~S parameter, but I got ~S.~%~
                         Lambda list: ~S."
			name kind lst orig-lambda-list))
	       (car lst)) )

      ;; Now collect the various elements of the lambda-list
      (setq required (collect))
      (when (and (consp lambda-list) (eq (car lambda-list) '&OPTIONAL)) (pop lambda-list) (setq optionals (collect)))
      (when (and (consp lambda-list) (member (car lambda-list) '(&REST &BODY))) (pop lambda-list) (setq rest-var (collect)))
      (when (and (consp lambda-list) (eq (car lambda-list) '&KEY)) (pop lambda-list) (setq keys (collect T)))
      (when (and (consp lambda-list) (eq (car lambda-list) '&AUX)) (pop lambda-list) (setq aux-vars (collect)))

      ;; Inspect the remaining value of lambda-list
      (cond ((consp lambda-list)
	     ;; Not all was parsed correctly ...
	     (error "In lambda list of macro ~S: Found lambda list keyword ~S out of order;~%~
                     The order must be &OPTIONAL, &REST/&BODY, &KEY, &AUX; &WHOLE and &ENVIRONMENT may apear anywhere.~%~
                     Lambda list: ~S."
		    name (car lambda-list) orig-lambda-list))
	    ((null lambda-list)) ; Everything is just fine.
	    ((symbolp lambda-list)
	     ;; Dotted with a symbol = specification of a rest-var
	     (push lambda-list rest-var))
	    (T
	     ;; List is odd-ly dotted.
	     (error "In lambda list of macro ~S: A lambda list may only be dotted with a symbol.~%~
                     Lambda list: ~S."
		    name orig-lambda-list)) )

      ;; Now check for rest-var, whole-var and env-var, which may all specify only one variable ...
      (setf rest-var (check-only-one '&REST rest-var))
      (setf whole-var (check-only-one '&WHOLE whole-var))
      (when (and env-var (not environment-value))
	(cerror "Ignore the &ENVIRONMENT parameter."
		"In lambda list of macro ~S: An &ENVIRONMENT parameter may only be specified on the top-level lambda list.~%~
                 Lambda list: ~S."
		name orig-lambda-list)
	(setq env-var nil))
      (setf env-var (check-only-one '&ENVIRONMENT env-var))

      (when (and (null rest-var) keys)
	(setf rest-var (gensym)))

      ;; Build up the bindings
      (let ((bindings nil) (constraints nil) (w whole))
	(labels ((add-one (x) (add (list x)))
		 (add-bind (spec val)
		    (if (consp spec)
			(let ((gsym (gensym)))
			  (add-one `(,gsym ,val))
			  (multiple-value-bind (bndngs cnstrnts) (parse-macro-lambda-list name spec gsym)
			     (add bndngs)
			     (setq contraints (nconc constraints cnstrnts))) )
		      (add-one `(,spec ,val))))
		 (add (x) (setf bindings (nconc bindings x))))

	  (when whole-var
	    (add-one `(,whole-var ,real-whole))
	    (when (eq whole real-whole) (setq w whole-var)))

	  ;; Calculate the constraints ...
	  (let ((min nil)
		(max nil))
	    (when (or required optionals rest-var) (setq min (length required)))
	    (when (and (null rest-var) (or required optionals))
	      (setq max (+ (length required) (length optionals))))
	    (cond ((and (null min) (null max)))
		  ((eql min max)
		   (push `(listp ,w) constraints)
		   (push `(= (length ,w) ,min) constraints))
		  (T
		   (push `(listp ,w) constraints)
		   (when (and min (> min 0)) (push `(>= (length ,w) ,min) constraints))
		   (when max (push `(<= (length ,w) ,max) constraints))) ))

	  (setq constraints (nreverse constraints))
	  
	  (dolist (spec required)
	    (add-bind spec `(CAR ,w))
	    (setf w (list 'cdr w)))

	  (dolist (spec optionals)
	    ;; CHECK
	    (cond ((consp spec)
		   (when (caddr spec)	;svar
		     (add-one `(,(caddr spec) (NOT (NULL ,w)))))
		   (add-bind (car spec) `(if (NOT (NULL ,w)) (CAR ,w) ,(cadr spec))))
		  (T
		   (add-one `(,spec (CAR ,w)))) )
	    (setf w (list 'cdr w)))

	  (when rest-var (add-one `(,rest-var ,w)))

	  (dolist (spec keys)
	    ;; CHECK
	    (let (kw var svar default)
	      (cond ((consp spec)
		     (setq var (car spec) default (cadr spec) svar (caddr spec))
		     (when (consp var) (setq kw (car var) var (cadr var))))
		    (T (setq var spec default nil svar nil)))
	      ;; SVAR
	      (unless kw (setq kw (intern (symbol-name var) :keyword)))
	      (add-bind var `(getf ,rest-var ,kw ,default)) ))

	  (dolist (spec aux-vars) (add-one spec))

	  (when env-var
	    (add-one `(,env-var ,environment-value)))
	  
	  (values bindings constraints env-var)) ))))

(defun glisp::parse-macro (name lambda-list body &optional env)
  "This is used to process  a macro definition in the  same way as defmacro  and
  macrolet. It  returns a lambda-expression that  accepts two  arguments, a form
  and an  environment. The name,  lambda-list, and  body arguments correspond to
  the parts of a defmacro or macrolet definition.

  The lambda-list argument  may inclue &environment and  &whole and  may include
  destructing. The name  argument  is used to   enclose the body in  an implicat
  block  and might  also be  used   for implementation-depend purposes (such  as
  including the name of the macro  in error messages if the  form does not match
  the lambda-list)."

  (let ((call (gensym)) (env (gensym)))
    (multiple-value-bind (bindings constraints)
			 (parse-macro-lambda-list name lambda-list `(CDR call) env call)
      `(lambda (,call ,env)
	 (block ,name
	   (let* ,bindings
	     (unless (and ,@constraints)
	       (error "Macro ~S called with wrong number/nesting of arguments: ~S"
		      ',name ,call))
	     ,@body))) )) )

(defmacro glisp::destructuring-bind (lambda-list expression &body body)
  "This macro binds the variables specified  in lambda-list to the corresponding
  values in the  tree structure resulting from  evaluating  the expression, then
  executes the forms as an implicit progn.

  A destructing-bind lambda-list may contain the lambda-list keywords &optional,
  &rest, &key, &allow-other-keys, and &aux; &body and &whole may also be used as
  they are   in defmacro, but &environment  may  not be used. Nested  and dotted
  lambda-lists  are   also permitted  as   for defmacro.   The   idea is  that a
  destructing-bind lambda-list has the same format as inner levels of a defmacro
  lambda-list.

  If the result  of evaluating the  expressions does not match the destructuring
  pattern, an error should be signaled."

  (let ((call (gensym)))
    (multiple-value-bind (bindings constraints)
			 (parse-macro-lambda-list nil lambda-list call)
      `(let* ((,call ,expression) ,@bindings)
	 (unless (and ,@constraints)
	   (error "DESTRUCTING-BIND with wrong number/nesting of arguments: ~S~%~
                   Lambda list to match with: ~S." ,call ',lambda-list))
	 (locally ,@body)) )) )


(defmacro glisp::loop (&rest args)
  `(sloop:sloop ,@args))

(defun glisp:compile-file-pathname (filename &rest options)
  (declare (ignore options))
  (merge-pathnames (make-pathname :type "o") filename))


(defmacro glisp:ignore-errors (&rest body)
  `(IGNORE-ERRORS-FN #'(LAMBDA () ,@body)))

(defun ignore-errors-fn (cont)
  (let ((old (symbol-function 'system:universal-error-handler)))
    (block foo
      (unwind-protect
          (progn
            (setf (symbol-function 'system:universal-error-handler)
                  #'(lambda (&rest x)
                      (return-from foo (values nil x))))
            (funcall cont) )
        (setf (symbol-function 'system:universal-error-handler) old) ))))

(defun glisp::make-pathname (&rest args &key directory &allow-other-keys)
  (cond ((eq (car directory) :relative)
         (apply #'lisp:make-pathname :directory (cdr directory) args))
        ((eq (car directory) :absolute)
         (apply #'lisp:make-pathname :directory (cons :root (cdr directory)) args))
        (t
         (apply #'lisp:make-pathname args))))

(defun glisp::pathname-directory (pathname)
  (let ((d (lisp:pathname-directory pathname)))
    (cond ((eq (car d) :root)
           (cons :absolute (cdr d)))
          (t
           (cons :relative d)))))


(defun glisp::run-unix-shell-command (cmd)
  (glisp::unix/system cmd))
