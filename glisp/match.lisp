;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GLISP; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Very simple (non-deterministic) regular expression matching
;;;   Created: 1999-01-21
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1999 by Gilbert Baumann

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

(in-package :GLISP)

;; Syntax
;; ------

;; atom          -- match the atom 
;; (p predicate) -- match, iff (funcall p elt) is non-NIL
;; (& a0 .. an)  -- match a0a1..an
;; (/ a0 .. an)  -- match a0 or a1 ... or an
;; (* a0 .. an)  -- iteration, match any number of (& a0 ... an)
;; (+ . rest)    == (/ (& . rest) (* . rest))
;; (? . rest)    == (/ (& . rest) (&))
;; (= var subexpr) == assign the subexpr to the match variable 'var'
;;
;; not implemented:
;; (- a b)       -- match a, but not b
;; (and a b)     -- matches if a and b matches
;; (or a b)      == (/ a b)
;; (not x)       == matches if x does not match
;; 

;; This syntax has to be merged with clex as well.

(defvar *match-macros* (make-hash-table :test #'eq))

(defmacro define-match-macro (name args &body body)
  `(eval-when (compile load eval)
     (setf (gethash ',name *match-macros*)
       #'(lambda (whole)
           (destructuring-bind ,args (cdr whole)
             ,@body)))
     ',name))

(defun symcat (&rest syms)
  (let ((pack (dolist (k syms nil)
                (when (symbolp k) 
                  (return (symbol-package k))))))
    (cond ((null pack)
           (error "No package for ~S of ~S." 'symcat syms))
          (t
           (intern (apply #'concatenate 'string (mapcar #'string syms))
                   pack)))))

(defun sym-equal (a b)
  (string= (symbol-name a) (symbol-name b)))

(defun bau-funcall (fun &rest args)
  (cond ((and (consp fun) (eq (car fun) 'lambda))
         (cons fun args))
        ((and (consp fun) (eq (car fun) 'function))
         (cons (cadr fun) args))
        (t
         (list* 'funcall fun args))))

(defun compile-srx (srx action &key (string-type 'vector) (test '#'eql))
  (let ((vars nil))
    (labels ((cmp (x cont-expr)
               (cond
                ((atom x)
                 (with-unique-names (string start end)
                   `(lambda (,string ,start ,end)
                      (declare (type fixnum ,start ,end)
                               (type ,string-type ,string))
                      (if (and (< ,start ,end) 
                               ,(bau-funcall test `(aref ,string ,start) `',x))
                          ,(bau-funcall cont-expr string `(the fixnum (1+ ,start)) end)))))

                ((sym-equal (car x) 'p)
                 (destructuring-bind (p) (cdr x)
                   (with-unique-names (string start end)
                     `(lambda (,string ,start ,end)
                        (declare (type fixnum ,start ,end)
                                 (type ,string-type ,string))
                        (if (and (< ,start ,end) 
                                 ,(bau-funcall p `(aref ,string ,start)))
                            ,(bau-funcall cont-expr string `(the fixnum (1+ ,start)) end))))))

                ((sym-equal (car x) '/)
                 (with-unique-names (ccfn string string2 start end end2 j)
                   `(lambda (,string ,start ,end)
                      (declare (type fixnum ,start ,end)
                               (type ,string-type ,string))
                      (labels ((,ccfn (,string2 ,j ,end2) 
                                 (declare (type fixnum ,j ,end2)
                                          (type ,string-type ,string2))
                                 ,(bau-funcall cont-expr string2 j end2)))
                        ,@(mapcar (lambda (a)
                                    `(,(cmp a `#',ccfn) ,string ,start ,end))
                                  (cdr x))))))

                ((sym-equal (car x) '*)
                 (with-unique-names (ccfn string string2 start end end2 j)
                   (let ((subexpr (cons '& (cdr x))))
                     `(lambda (,string ,start ,end)
                        (declare (type fixnum ,start ,end)
                                 (type ,string-type ,string))
                        (labels ((,ccfn (,string2 ,j ,end2)
                                   (declare (type fixnum ,j ,end2)
                                            (type ,string-type ,string2))
                                   (,(cmp subexpr `#',ccfn) ,string2 ,j ,end2)
                                   ,(bau-funcall cont-expr string j end)))
                          (,ccfn ,string ,start ,end))))))

                ((sym-equal (car x) '&)
                 (case (length x)
                   (1 (with-unique-names (string start end)
                        `(lambda (,string ,start ,end)
                           (declare (type fixnum ,start ,end)
                                    (type ,string-type ,string))
                           ,(bau-funcall cont-expr string start end))))
                   (2 (cmp (cadr x) cont-expr))
                   (otherwise
                    (with-unique-names (string start end)
                      `(lambda (,string ,start ,end)
                         (declare (type fixnum ,start ,end)
                                  (type ,string-type ,string))
                         (,(cmp (cadr x) 
                                (with-unique-names (string j end)
                                  `#'(lambda (,string ,j ,end)
                                       (declare (type fixnum ,j ,end)
                                                (type ,string-type ,string))
                                       (,(cmp (cons '& (cddr x)) cont-expr) ,string ,j ,end))))
                          ,string ,start ,end))))))

                ((sym-equal (car x) '=)
                 (destructuring-bind (var subexpr) (cdr x)
                   (pushnew var vars)
                   (with-unique-names (string i0 end)
                     `(lambda (,string ,i0 ,end)
                        (declare (type fixnum ,i0 ,end)
                                 (type ,string-type ,string))
                        (,(cmp subexpr
                               (with-unique-names (string i1 end)
                                 `#'(lambda (,string ,i1 ,end)
                                      (declare (type fixnum ,i1 ,end)
                                               (type ,string-type ,string))
                                      (setf ,(symcat var "-START") ,i0
                                            ,(symcat var "-END") ,i1)
                                      ,(bau-funcall cont-expr string i1 end))))
                         ,string ,i0 ,end)))))
                
                ((sym-equal (car x) '+)
                 (cmp `(& ,@(cdr x) (* ,@(cdr x))) cont-expr))

                ((sym-equal (car x) '?)
                 (cmp `(/ (&) (& ,@(cdr x))) cont-expr))

                (t
                 (let ((mmf (gethash (car x) *match-macros*)))
                   (cond (mmf
                          (cmp (funcall mmf x) cont-expr))
                         (t
                          (error "Unknown symbolic regular expression: ~S." x))))) )))

      (with-unique-names (string start end continuation match)
        (let ((cf (cmp srx `#',continuation)))
          `(lambda (,string ,start ,end)
             (declare ;;#.cl-user:+optimize-very-fast+
                      (type fixnum ,start ,end)
                      (type ,string-type ,string))
             (block ,match
               (let ,(mapcan (lambda (var) (list (symcat var "-START") (symcat var "-END"))) vars)
                 (labels (,(with-unique-names (string j end)
                             `(,continuation (,string ,j ,end)
                                             (declare (type fixnum ,j ,end)
                                                      (type ,string-type ,string))
                                             (declare (ignore ,string))
                                             (if (= ,j ,end)
                                                 (let ()
                                                   (return-from ,match ,action))))))
                   (,cf ,string ,start ,end)))
               nil)))))))

(defmacro if-match ((string &key start end type (test '#'eql)) srx &body actions)
  (let ((str (gensym "str")))
    `(let ((,str ,string))
       (,(compile-srx srx `(progn .,actions) 
                      :string-type (or type 'vector)
                      :test test)
        ,str
        ,(if start start 0)
        ,(if end end `(length ,str))))))

