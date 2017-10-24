;;; copyright (c) 2004 knowledgeTools Int. GmbH
;;; Author of this version: David Lichteblau <david@knowledgetools.de>
;;;
;;; derived from runes.lisp, (c) copyright 1998,1999 by Gilbert Baumann
;;;
;;; License: Lisp-LGPL (See file COPYING for details).
;;;
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

(in-package :fxml.runes)

(deftype rune () #-lispworks 'character #+lispworks 'lw:simple-char)
(deftype rod () '(vector rune))
(deftype simple-rod () '(simple-array rune))

(definline rune (rod index)
  (char rod index))

(definline (setf rune) (new rod index)
  (setf (char rod index) new))

;;; TODO simple-string or simple-rod?

(definline %rune (rod index)
  (schar (the simple-string rod) (the array-index index)))

(definline (setf %rune) (new rod index)
  (setf (schar (the simple-string rod) (the array-index index)) new))

(definline rod-capitalize (rod)
  (string-upcase rod))

(definline code-rune (x) (code-char x))
(definline rune-code (x) (char-code x))

(definline rune= (x y) 
  (char= x y))

(definline rune-downcase (rune)
  (char-downcase rune))

(definline rune-upcase (rune)
  (char-upcase rune))

(definline rune-upper-case-letter-p (rune)
  (upper-case-p rune))

(definline rune-lower-case-letter-p (rune)
  (lower-case-p rune))

(definline rune-equal (x y)
  (char-equal x y))

(definline rod-downcase (rod)
  (string-downcase rod))

(definline rod-upcase (rod)
  (string-upcase rod))

(definline white-space-rune-p (char)
  ;; tab, linefeed, carriage return, space
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (case (char-code char)
    ((#.(char-code #\Tab)
      10                                ;Linefeed
      13                                ;Carriage return
      #.(char-code #\Space))
     t)
    (otherwise nil)))

(definline digit-rune-p (char &optional (radix 10))
  (digit-char-p char radix))

(-> rod ((or rod symbol character vector integer)) rod)
(defun rod (x)
  (typecase x
    (rod       x)
    ;; Shadowed.
    ;; (string    (coerce x 'rod))
    (symbol    (string x))
    (character (string x))
    (vector    (coerce x 'rod))
    (integer   (string (code-char x)))
    (t         (error "Cannot convert ~S to a ~S" x 'rod))))

(definline runep (x)
  (characterp x))

(defun rod= (x y)
  (cond
    ((emptyp x) (emptyp y))
    ((emptyp y) nil)
    (t (string= x y))))

(definline rod-equal (x y)
  (string-equal x y))

(definline make-rod (size)
  (make-string size :element-type 'rune))

(definline char-rune (char)
  char)

(definline rune-char (rune &optional default)
  (declare (ignore default))
  rune)

(definline rod-string (rod &optional (default-char #\?))
  (declare (ignore default-char))
  rod)

(definline string-rod (string)
  string)

;;;;

(defun rune<= (rune &rest more-runes)
  (every #'char<= (cons rune more-runes)))

(define-compiler-macro rune<= (rune &rest more-runes)
  `(char<= ,rune ,@more-runes))

(defun rune>= (rune &rest more-runes)
  (every #'char>= (cons rune more-runes)))

(define-compiler-macro rune>= (rune &rest more-runes)
  `(char>= ,rune ,@more-runes))

(definline rodp (object)
  (stringp object))

(definline rod-subseq (source start &optional (end (length source)))
  (unless (stringp source)
    (error 'type-error :datum source :expected-type 'rod))
  (subseq source start end))

(definline rod-subseq* (source start &optional (end (length source)))
  (rod-subseq source start end))

(definline rod< (rod1 rod2)
  (string< rod1 rod2))
