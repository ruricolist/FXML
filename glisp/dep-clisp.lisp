;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CLISP dependent stuff + fixups
;;;   Created: 1999-05-25 22:32
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

(in-package :CL-USER)

(eval-when (compile load eval)
  (if (fboundp 'cl::define-compiler-macro)
      (pushnew 'define-compiler-macro *features*)))

(setq lisp:*load-paths* '(#P"./"))

(import 'lisp:read-byte-sequence :glisp)
(export 'lisp:read-byte-sequence :glisp)
(import 'lisp:read-char-sequence :glisp)
(export 'lisp:read-char-sequence :glisp)
(export 'glisp::compile-file :glisp)
(export 'glisp::run-unix-shell-command :glisp)
(export 'glisp::make-server-socket :glisp)


#||
(export 'glisp::read-byte-sequence :glisp) 
(defun glisp::read-byte-sequence (sequence input &key (start 0) (end (length sequence)))
  (let (c (i start))
    (loop
      (cond ((= i end) (return i)))
      (setq c (read-byte input nil :eof))
      (cond ((eql c :eof) (return i)))
      (setf (aref sequence i) c)
      (incf i) )))
||#


(defun glisp::compile-file (&rest ap)
  (and (apply #'compile-file ap)
       (apply #'compile-file-pathname ap)))

(defmacro glisp::with-timeout ((&rest ignore) &body body)
  (declare (ignore ignore))
  `(progn
     ,@body))

(defun glisp::open-inet-socket (hostname port)
  (values
   (lisp:socket-connect port hostname)
   :byte))

(defun glisp:make-server-socket (port)
  (lisp:socket-server port))

(defun glisp::accept-connection/low (socket)
  (let ((stream (lisp:socket-accept socket)))
    (setf (stream-element-type stream) '(unsigned-byte 8))
    (values 
     stream
     :byte)))

(defun glisp::g/make-string (length &rest options)
  (apply #'make-array length 
         :element-type 
         '#.(cond ((stringp (make-array 1 :element-type 'string-char))
                   'string-char)
                  ((stringp (make-array 1 :element-type 'base-char))
                   'base-char)
                  (t
                   (error "What is the string element type of the day?")))
         options))

(defun glisp:run-unix-shell-command (command)
  (lisp:shell command))

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
  (export 'glisp::define-compiler-macro :glisp)
  (defmacro glisp::define-compiler-macro (name args &body body)
    (declare (ignore args body))
    `(progn
       ',name)))

#||
(defun xlib:draw-glyph (drawable gcontext x y elt &rest more)
  (apply #'xlib:draw-glyphs drawable gcontext x y (vector elt) more))
||#

(defmacro glisp::defsubst (name args &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args .,body)))

(export 'glisp::getenv :glisp)
(defun glisp::getenv (var)
  (sys::getenv var))



(export 'glisp::mp/process-run-function :glisp)
(defun glisp:mp/process-run-function (name fn &rest args)
  (apply #'mp:process-run-function name fn args))

(export 'glisp::mp/process-kill :glisp)
(defun glisp:mp/process-kill (proc)
  (mp:process-kill proc))

(export 'glisp::mp/current-process :glisp)
(defun glisp:mp/current-process ()
  (mp:current-process))

(export 'glisp::mp/seize-lock :glisp)
(defun glisp::mp/seize-lock (lock &key whostate)
  whostate
  (mp:process-lock lock))

(export 'glisp::mp/release-lock :glisp)
(defun glisp::mp/release-lock (lock)
  (mp:process-unlock lock))

(export 'glisp::mp/process-yield :glisp)
(defun glisp::mp/process-yield (&optional process-to-run)
  process-to-run
  (mp:process-allow-schedule))

(export 'glisp::mp/process-wait :glisp)
(defun glisp::mp/process-wait (whostate predicate)
  (mp::process-wait whostate predicate))

(defmacro glisp::mp/with-lock ((lock) &body body)
  `(mp:with-process-lock (,lock)
     ,@body))

(defun glisp::mp/make-lock (&key name)
  (mp:make-process-lock :name name))












