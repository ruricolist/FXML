;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: RUNES; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Unicode strings (called RODs)
;;;   Created: 1999-05-25 22:29
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LLGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1998,1999 by Gilbert Baumann

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
;;  1999-08-15  GB      - ROD=, ROD-EQUAL
;;                        RUNE<=, RUNE>=
;;                        MAKE-ROD, ROD-SUBSEQ
;;                        CHAR-RUNE, RUNE-CHAR, ROD-STRING, STRING-ROD
;;                        new functions
;;                      - Added rune reader
;;

(in-package :runes)

(deftype rune () '(unsigned-byte 16))
(deftype rod () '(array rune (*)))
(deftype simple-rod () '(simple-array rune (*)))

(defsubst rune (rod index)
  (aref rod index))

(defun (setf rune) (new rod index)
  (setf (aref rod index) new))

(defsubst %rune (rod index)
  (aref (the (simple-array (unsigned-byte 16) (*)) rod) (the fixnum index)))

(defsubst (setf %rune) (new rod index)
  (setf (aref (the (simple-array (unsigned-byte 16) (*)) rod) (the fixnum index)) new))

(defun rod-capitalize (rod)
  (warn "~S is not implemented." 'rod-capitalize)
  rod)

(defsubst code-rune (x) x)
(defsubst rune-code (x) x)

(defsubst rune= (x y) 
  (= x y))

(defun rune-downcase (rune)
  (cond ((<= #x0041 rune #x005a) (+ rune #x20))
        ((= rune #x00d7) rune)
        ((<= #x00c0 rune #x00de) (+ rune #x20))
        (t rune)))

(defsubst rune-upcase (rune)
  (cond ((<= #x0061 rune #x007a) (- rune #x20))
        ((= rune #x00f7) rune)
        ((<= #x00e0 rune #x00fe) (- rune #x20))
        (t rune)))

(defun rune-upper-case-letter-p (rune)
  (or (<= #x0041 rune #x005a) (<= #x00c0 rune #x00de)))

(defun rune-lower-case-letter-p (rune)
  (or (<= #x0061 rune #x007a) (<= #x00e0 rune #x00fe)
      (= rune #x00d7)))


(defun rune-equal (x y)
  (rune= (rune-upcase x) (rune-upcase y)))

(defun rod-downcase (rod)
  ;; FIXME
  (register-rod
   (map '(simple-array (unsigned-byte 16) (*)) #'rune-downcase rod)))

(defun rod-upcase (rod)
  ;; FIXME
  (register-rod
   (map '(simple-array (unsigned-byte 16) (*)) #'rune-upcase rod)))

(defsubst white-space-rune-p (char)
  (or (= char 9)        ;TAB
      (= char 10)       ;Linefeed
      (= char 13)       ;Carriage Return
      (= char 32)))     ;Space

(defsubst digit-rune-p (char &optional (radix 10))
  (cond ((<= #.(char-code #\0) char #.(char-code #\9))
         (and (< (- char #.(char-code #\0)) radix)
              (- char #.(char-code #\0))))
        ((<= #.(char-code #\A) char #.(char-code #\Z))
         (and (< (- char #.(char-code #\A) -10) radix)
              (- char #.(char-code #\A) -10)))
        ((<= #.(char-code #\a) char #.(char-code #\z))
         (and (< (- char #.(char-code #\a) -10) radix)
              (- char #.(char-code #\a) -10))) ))

(defun rod (x)
  (cond ((stringp x)    (register-rod (map 'rod #'char-code x)))
        ((symbolp x)    (rod (string x)))
        ((characterp x) (rod (string x)))
        ((vectorp x)    (register-rod (coerce x 'rod)))
        ((integerp x)   (register-rod (map 'rod #'identity (list x))))
        (t              (error "Cannot convert ~S to a ~S" x 'rod))))

(defun runep (x)
  (and (integerp x)
       (<= 0 x #xFFFF)))

(defun sloopy-rod-p (x)
  (and (not (stringp x))
       (vectorp x)
       (every #'runep x)))

(defun rod= (x y)
  (and (= (length x) (length y))
       (dotimes (i (length x) t)
         (unless (rune= (rune x i) (rune y i))
           (return nil)))))

(defun rod-equal (x y)
  (and (= (length x) (length y))
       (dotimes (i (length x) t)
         (unless (rune-equal (rune x i) (rune y i))
           (return nil)))))

(defsubst make-rod (size)
  (let ((res (make-array size :element-type 'rune)))
    (register-rod res)
    res))

(defun char-rune (char)
  (code-rune (char-code char)))

(defun rune-char (rune &optional (default #\?))
  #+CMU
  (if (< rune 256) (code-char rune) default)
  #-CMU
  (or (code-char rune) default))

(defun rod-string (rod &optional (default-char #\?))
  (map 'string (lambda (x) (rune-char x default-char)) rod))

(defun string-rod (string)
  (let* ((n (length string))
         (res (make-rod n)))
    (dotimes (i n)
      (setf (%rune res i) (char-rune (char string i))))
    res))

;;;;

(defun rune<= (rune &rest more-runes)
  (apply #'<= rune more-runes))

(defun rune>= (rune &rest more-runes)
  (apply #'>= rune more-runes))

(defun rodp (object)
  (typep object 'rod))

(defun really-rod-p (object)
  (and (typep object 'rod)
       (really-really-rod-p object)))

(defun rod-subseq (source start &optional (end (length source)))
  (unless (rodp source)
    (error "~S is not of type ~S." source 'rod))
  (unless (and (typep start 'fixnum) (>= start 0))
    (error "~S is not a non-negative fixnum." start))
  (unless (and (typep end 'fixnum) (>= end start))
    (error "END argument, ~S, is not a fixnum no less than START, ~S." end start))
  (when (> start (length source))
    (error "START argument, ~S, should be no greater than length of rod." start))
  (when (> end (length source))
    (error "END argument, ~S, should be no greater than length of rod." end))
  (locally
      (declare (type rod source)
               (type fixnum start end))
    (let ((res (make-rod (- end start))))
      (declare (type rod res))
      (do ((i (- (- end start) 1) (the fixnum (- i 1))))
          ((< i 0) res)
        (declare (type fixnum i))
        (setf (%rune res i) (%rune source (the fixnum (+ i start))))))))

(defun rod-subseq* (source start &optional (end (length source)))
  (unless (and (typep start 'fixnum) (>= start 0))
    (error "~S is not a non-negative fixnum." start))
  (unless (and (typep end 'fixnum) (>= end start))
    (error "END argument, ~S, is not a fixnum no less than START, ~S." end start))
  (when (> start (length source))
    (error "START argument, ~S, should be no greater than length of rod." start))
  (when (> end (length source))
    (error "END argument, ~S, should be no greater than length of rod." end))
  (locally
      (declare (type fixnum start end))
    (let ((res (make-rod (- end start))))
      (declare (type rod res))
      (do ((i (- (- end start) 1) (the fixnum (- i 1))))
          ((< i 0) res)
        (declare (type fixnum i))
        (setf (%rune res i) (aref source (the fixnum (+ i start))))))))

;;; Support for telling ROD and arrays apart:

#+CMU
(progn
  (defvar *rod-hash-table*
    (make-array 5003 :initial-element nil)))

(defun register-rod (rod)
  #+CMU
  (unless (really-really-rod-p rod)
    (push (ext:make-weak-pointer rod)
          (aref *rod-hash-table* (mod (cl::pointer-hash rod)
                                      (length *rod-hash-table*)))))
  rod)

(defun really-really-rod-p (rod)
  #+CMU
  (find rod (aref *rod-hash-table* (mod (cl::pointer-hash rod)
                                        (length *rod-hash-table*)))
        :key #'ext:weak-pointer-value))

#+CMU
(progn
  (defun rod-hash-table-rehash ()
    (let* ((n 5003)
           (new (make-array n :initial-element nil)))
      (loop for bucket across *rod-hash-table* do
            (loop for item in bucket do
                  (let ((v (ext:weak-pointer-value item)))
                    (when v
                      (push item (aref new (mod (cl::pointer-hash v) n)))))))
      (setf *rod-hash-table* new)))

  (defun rod-hash-after-gc-hook ()
    ;; hmm interesting question: should we rehash?
    (rod-hash-table-rehash))

  (pushnew 'rod-hash-after-gc-hook extensions:*after-gc-hooks*) )

(defun rod< (rod1 rod2)
  (do ((i 0 (+ i 1)))
      (nil)
    (cond ((= i (length rod1))
           (return t))
          ((= i (length rod2))
           (return nil))
          ((< (aref rod1 i) (aref rod2 i))
           (return t))
          ((> (aref rod1 i) (aref rod2 i))
           (return nil)))))
