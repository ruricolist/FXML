;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CLISP dependent stuff + fixups
;;;   Created: 1999-05-25 22:32
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LLGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1999 by Gilbert Baumann

;;; This code is free software; you can redistribute it and/or modify it
;;; under the terms of the version 2.1 of the GNU Lesser General Public
;;; License as published by the Free Software Foundation, as clarified
;;; by the "Preamble to the Gnu Lesser General Public License" found in
;;; the file COPYING.
;;;
;;; This code is distributed in the hope that it will be useful,
;;; but without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; Version 2.1 of the GNU Lesser General Public License is in the file
;;; COPYING that was distributed with this file.  If it is not present,
;;; you can access it from http://www.gnu.org/copyleft/lesser.txt (until
;;; superseded by a newer version) or write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(in-package :CL-USER)

(eval-when (compile load eval)
  (if (fboundp 'cl::define-compiler-macro)
      (pushnew 'define-compiler-macro *features*)))

(setq lisp:*load-paths* '(#P"./"))

#+DEFINE-COMPILER-MACRO
(cl:define-compiler-macro ldb (bytespec value &whole whole)
  (let (pos size)
    (cond ((and (consp bytespec)
                (= (length bytespec) 3)
                (eq (car bytespec) 'byte)
                (constantp (setq size (second bytespec)))
                (constantp (setq pos (third bytespec))))
           `(logand ,(if (eql pos 0) value `(ash ,value (- ,pos)))
                    (1- (ash 1 ,size))))
          (t
           whole))))

#-DEFINE-COMPILER-MACRO
(progn
  (export 'runes::define-compiler-macro :runes)
  (defmacro runes::define-compiler-macro (name args &body body)
    (declare (ignore args body))
    `(progn
       ',name)))

(defmacro runes::defsubst (name args &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args .,body)))
