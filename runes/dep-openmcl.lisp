;;;; dep-openmcl.lisp
;;;;
;;;; This file is part of the CXML parser, released under (L)LGPL.
;;;; See file COPYING for details.
;;;;
;;;; (c) copyright 1999 by Gilbert Baumann

(defmacro runes::defsubst (fun args &body body)
  (if (consp fun)
      `(defun ,fun ,args
         ,@body)
      `(progn
         (defun ,fun ,args .,body)
         (define-compiler-macro ,fun (&rest .args.)
           (cons '(lambda ,args .,body)
                 .args.)))))
