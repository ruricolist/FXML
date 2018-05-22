;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: RUNES; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Some common utilities for the Closure browser
;;;   Created: 1997-12-27
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: Lisp-LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1997-1999 by Gilbert Baumann

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

;; Changes
;;
;;  When        Who     What
;; ----------------------------------------------------------------------------
;;  1999-08-24  GB      = fixed MULTIPLE-VALUE-OR it now takes any number of
;;                        subforms
;;

(in-package :fxml)

;;; --------------------------------------------------------------------------------
;;;  while and until

(defmacro while (test &body body)
  `(loop while ,test do ,@body))

;; prime numbers

(defun primep (n)
  "Returns true, iff `n' is prime."
  (and (> n 2)
       (oddp n)
       (loop for i from 2
             until (> (* i i) n)
             when (zerop (mod n i))
               do (return nil)
             finally (return t))))

(defun nearest-greater-prime (n)
  "Returns the smallest prime number no less than `n'."
  (cond ((primep n) n)
        ((nearest-greater-prime (+ n 1)))))

;;; XML meta.
(definline xml-character-p (c)
  (declare (optimize speed) (character c))
  (let ((code (char-code c)))
    (or (eql code 9)
        (eql code 10)
        (eql code 13)
        (<= 32 code #xd7ff)
        (<= #xe000 code #xfffd)
        (<= #x10000 code #x10ffff))))

(defun xml-characters-p (str)
  (etypecase str
    ((simple-array character (*))
     (loop for c across str
           always (xml-character-p c)))
    (string
     (loop for c across str
           always (xml-character-p c)))))

