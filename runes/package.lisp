;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Generating a sane DEFPACKAGE for RUNES
;;;   Created: 1999-05-25
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1999,2000 by Gilbert Baumann

(in-package :cl-user)

(defpackage :runes
  (:use :cl)
  (:export #:defsubst

           ;; util.lisp :
           #:compose
           #:curry
           #:rcurry
           #:until
           #:while
    
           ;; runes.lisp
           #:rune
           #:rod
           #:simple-rod
           #:%rune
           #:rod-capitalize
           #:code-rune
           #:rune-code
           #:rune-downcase
           #:rune-upcase
           #:rod-downcase
           #:rod-upcase
           #:white-space-rune-p
           #:digit-rune-p
           #:rune=
           #:rune<=
           #:rune>=
           #:rune-equal
           #:runep
           #:sloopy-rod-p
           #:rod=
           #:rod-equal
           #:make-rod
           #:char-rune
           #:rune-char
           #:rod-string
           #:string-rod
           #:rod-subseq
           #:rod<))
