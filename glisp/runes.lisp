;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GLISP; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Unicode strings (called RODs)
;;;   Created: 1999-05-25 22:29
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1998,1999 by Gilbert Baumann

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

(in-package :GLISP)

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
;;;; RUNE Reader
;;;;

;; Portable implementation of WHITE-SPACE-P with regard to the current
;; read table -- this is bit tricky.

(defun rt-white-space-p (char)
  (let ((stream (make-string-input-stream (string char))))
    (eq :eof (peek-char t stream nil :eof))))

(defun read-rune-name (input)
  ;; the first char is unconditionally read
  (let ((char0 (read-char input t nil t)))
    (when (char= char0 #\\)
      (setf char0 (read-char input t nil t)))
    (with-output-to-string (res)
      (write-char char0 res)
      (do ((ch (peek-char nil input nil :eof t) (peek-char nil input nil :eof t)))
          ((or (eq ch :eof)
               (rt-white-space-p ch)
               (multiple-value-bind (function non-terminating-p) (get-macro-character ch)
                 (and function (not non-terminating-p)))))
        (write-char ch res)
        (read-char input)))))           ;consume this character

(defun iso-10646-char-code (char)
  (char-code char))

(defvar *rune-names* (make-hash-table :test #'equal)
  "Hashtable, which maps all known rune names to rune codes;
   Names are stored in uppercase.")

(defun define-rune-name (name code)
  (setf (gethash (string-upcase name) *rune-names*) code)
  name)

(defun lookup-rune-name (name)
  (gethash (string-upcase name) *rune-names*))

(define-rune-name "null"        #x0000)
(define-rune-name "space"       #x0020)
(define-rune-name "newline"     #x000A)
(define-rune-name "return"      #x000D)
(define-rune-name "tab"         #x0009)
(define-rune-name "page"        #x000C)

;; and just for fun:
(define-rune-name "euro"        #x20AC)

;; ASCII control characters
(define-rune-name "nul"  #x0000)        ;null
(define-rune-name "soh"  #x0001)        ;start of header
(define-rune-name "stx"  #x0002)        ;start of text
(define-rune-name "etx"  #x0003)        ;end of text
(define-rune-name "eot"  #x0004)        ;end of transmission
(define-rune-name "enq"  #x0005)        ;
(define-rune-name "ack"  #x0006)        ;acknowledge
(define-rune-name "bel"  #x0007)        ;bell
(define-rune-name "bs"   #x0008)        ;backspace
(define-rune-name "ht"   #x0009)        ;horizontal tab
(define-rune-name "lf"   #X000A)        ;line feed, new line
(define-rune-name "vt"   #X000B)        ;vertical tab
(define-rune-name "ff"   #x000C)        ;form feed
(define-rune-name "cr"   #x000D)        ;carriage return
(define-rune-name "so"   #x000E)        ;shift out
(define-rune-name "si"   #x000F)        ;shift in
(define-rune-name "dle"  #x0010)        ;device latch enable ?
(define-rune-name "dc1"  #x0011)        ;device control 1
(define-rune-name "dc2"  #x0012)        ;device control 2
(define-rune-name "dc3"  #x0013)        ;device control 3
(define-rune-name "dc4"  #x0014)        ;device control 4
(define-rune-name "nak"  #x0015)        ;negative acknowledge
(define-rune-name "syn"  #x0016)        ;
(define-rune-name "etb"  #x0017)        ;
(define-rune-name "can"  #x0018)        ;
(define-rune-name "em"   #x0019)        ;end of message
(define-rune-name "sub"  #x001A)        ;
(define-rune-name "esc"  #x001B)        ;escape
(define-rune-name "fs"   #x001C)        ;field separator ?
(define-rune-name "gs"   #x001D)        ;group separator
(define-rune-name "rs"   #x001E)        ;
(define-rune-name "us"   #x001F)        ;
 
(define-rune-name "del"  #x007F)        ;delete

;; iso-latin
(define-rune-name "nbsp" #x00A0)        ;non breakable space
(define-rune-name "shy"  #x00AD)        ;soft hyphen

(defun rune-from-read-name (name)
  (cond ((= (length name) 1)
         (iso-10646-char-code (char name 0)))
        ((and (= (length name) 2)
              (char= (char name 0) #\\))
         (iso-10646-char-code (char name 1)))
        ((and (>= (length name) 3)
              (char-equal (char name 0) #\u)
              (char-equal (char name 1) #\+)
              (every (lambda (x) (digit-char-p x 16)) (subseq name 2)))
         (parse-integer name :start 2 :radix 16))
        ((lookup-rune-name name))
        (t
         (error "Meaningless rune name ~S." name))))

(defun rune-reader (stream subchar arg)
  subchar arg
  (values (rune-from-read-name (read-rune-name stream))))

(set-dispatch-macro-character #\# #\/ 'rune-reader)

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

;;; ROD ext syntax

(defun rod-reader (stream subchar arg)
  (declare (ignore arg))
  (rod
   (with-output-to-string (bag)
     (do ((c (read-char stream t nil t)
             (read-char stream t nil t)))
         ((char= c subchar))
       (cond ((char= c #\\)
              (setf c (read-char stream t nil t))))
       (princ c bag)))))

(defun rod-printer (stream rod)
  (princ #\# stream)
  (princ #\" stream)
  (loop for x across rod do
        (cond ((or (rune= x #.(char-code #\\))
                   (rune= x #.(char-code #\")))
               (princ #\\ stream)
               (princ (code-char x) stream))
              ((< x char-code-limit)
               (princ (code-char x) stream))
              (t
               (format stream "\\u~4,'0X" x))))
  (princ #\" stream))

(set-pprint-dispatch '(satisfies really-rod-p) #'rod-printer)

(set-dispatch-macro-character #\# #\" 'rod-reader)

#||
(defun longish-array-p (arr)
  (and (arrayp arr)
       (> (array-total-size arr) 10)))

(set-pprint-dispatch '(satisfies longish-array-p)
                     #'(lambda (stream object)
                         (let ((*print-array* nil)
                               (*print-pretty* nil))
                           (prin1 object stream))))
||#