;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GLISP; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Some common utilities for the Closure browser
;;;   Created: 1997-12-27
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1997-1999 by Gilbert Baumann

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
;;  1999-08-24  GB      = fixed MULTIPLE-VALUE-OR it now takes any number of
;;                        subforms
;;

(in-package :GLISP)

(defun neq (x y) (not (eq x y)))

(define-compiler-macro neq (x y)
  `(not (eq ,x ,y)))

;;; --------------------------------------------------------------------------------
;;;  Meta functions

(defun curry (fun &rest args)
  #'(lambda (&rest more)
      (apply fun (append args more))))

(defun rcurry (fun &rest args)
  #'(lambda (&rest more)
      (apply fun (append more args))))

(defun compose (f g)
  #'(lambda (&rest args)
      (funcall f (apply g args))))

(defun always (value)
  #'(lambda (&rest args)
      (declare (ignore args))
      value))

(defun true (&rest x)
  (declare (ignore x))
  t)

(defun false (&rest x)
  (declare (ignore x))
  nil)

;;; --------------------------------------------------------------------------------
;;;  Promises

(defstruct (promise (:print-function print-promise))
  forced? value fun)

(defun print-promise (self sink depth)
  (declare (ignore depth))
  (if (promise-forced? self)
      (format sink "#<~S ~S ~S>" 'promise :forced (promise-value self))
    (format sink "#<~S ~S>" 'promise :lazy)))

(defmacro promise (form)
  `(make-promise :forced? nil :fun #'(lambda () ,form)))

(defun force (x)
  (if (promise-forced? x)
      (promise-value x)
    (setf (promise-forced? x) t
          (promise-value x) (funcall (promise-fun x)))))

;;; --------------------------------------------------------------------------------
;;;  Some additional <op>f macros

(define-modify-macro maxf (&rest nums) max)
(define-modify-macro minf (&rest nums) min)
(define-modify-macro nconcf (&rest args) nconc)

;; Man sollte mal ein generelles <op>f macro definieren, in etwa so
;; (funcallf #'nconc x 10)

;;; Modifizierte Version von max / min.

(defun max* (&rest nums)
  (reduce (lambda (x y)
            (cond ((null x) y)
                  ((null y) x)
                  (t (max x y))))
          nums :initial-value nil))

(defun min* (&rest nums)
  (reduce (lambda (x y)
            (cond ((null x) y)
                  ((null y) x)
                  (t (min x y))))
          nums :initial-value nil))

;;; --------------------------------------------------------------------------------
;;;  Debuging aids

(defmacro show (&rest exprs)
  `(format T "~&** [~S]~{~#[~:; ~] ~A = ~S~}." ',(current-function-name) 
           (list ,@(mapcan (lambda (x)
                             (list (let ((*print-case* :downcase)) 
                                     (prin1-to-string x))
                                   x))
                           exprs))))

#+ALLEGRO
(defun current-function-name ()
  (car COMPILER::.FUNCTIONS-DEFINED.))

#-ALLEGRO
(defun current-function-name ()
  'ANONYMOUS)

;;; --------------------------------------------------------------------------------
;;;  Multiple values

(defmacro multiple-value-or (&rest xs)
  (cond ((null xs)
         nil)
        ((null (cdr xs)) 
         (car xs))
        (t
         (let ((g (gensym)))
           `(LET ((,g (MULTIPLE-VALUE-LIST ,(car xs))))
                 (IF (CAR ,g)
                     (VALUES-LIST ,g)
                     (MULTIPLE-VALUE-OR ,@(cdr xs))))))))

(defun multiple-value-some (predicate &rest sequences)
  (values-list
   (apply #'some (lambda (&rest args)
                   (let ((res (multiple-value-list (apply predicate args))))
                     (if (car res)
                         res
                       nil)))
          sequences)))

;;; --------------------------------------------------------------------------------
;;;  while and until

(defmacro while (test &body body)
  `(until (not ,test) ,@body))

(defmacro until (test &body body)
  `(do () (,test) ,@body))

;;; --------------------------------------------------------------------------------
;;;  Sequences

(defun split-by-if (predicate seq &key (start 0) (nuke-empty-p nil))
  (let ((p0 (position-if predicate seq :start start)))
    (if p0
        (if (and nuke-empty-p (= start p0))
            (split-by-if predicate seq :start (+ p0 1) :nuke-empty-p nuke-empty-p)
          (cons (subseq seq start p0)
                (split-by-if predicate seq :start (+ p0 1) :nuke-empty-p nuke-empty-p)))
      (if (and nuke-empty-p (= start (length seq)))
          nil
        (list (subseq seq start))))))

(defun split-by (item &rest args)
  (apply #'split-by-if (curry #'eql item) args))

(defun split-by-member (items &rest args)
  (apply #'split-by-if (rcurry #'member items) args))

;;; --------------------------------------------------------------------------------
;;;  Strings

(defun white-space-p (ch)
  ;;(declare #.cl-user:+optimize-very-fast-trusted+)
  (or (eq ch #\Return)
      (eq ch #\Newline)
      (eq ch #\Space)
      (eq ch #\Tab)
      (eq ch #\Page)))

(define-compiler-macro white-space-p (ch)
  `(member ,ch '(#\Return #\Newline #\Space #\Tab #\Page)) )

(defun sanify-string (string &optional (begin? t) (end? t)
                                       (start 0))
  (let ((i (position-if #'white-space-p string :start start)))
    (cond (i
           (let ((j (position-if-not #'white-space-p string :start i)))
             (if j
                 (concatenate 'string (subseq string start i)
                              (if (and (= i start) begin?) "" " ")
                              (sanify-string string nil end? j))
               (concatenate 'string (subseq string start i)
                            (if (not end?) " " "")))))
          (t (subseq string start)))))

(defun sanify-rod (string &optional (begin? t) (end? t) (start 0))
  (let ((i (position-if #'white-space-rune-p string :start start)))
    (cond (i
           (let ((j (position-if-not #'white-space-rune-p string :start i)))
             (if j
                 (concatenate 'rod (subseq string start i)
                              (if (and (= i start) begin?) '#() '#(32))
                              (sanify-rod string nil end? j))
               (concatenate 'rod (subseq string start i)
                            (if (not end?) '#(32) '#())))))
          (t (subseq string start)))))

(defun split-string (bag string)
  (setq string (string-trim bag string))
  (cond ((= (length string) 0) nil)
        (t
         (let ((p (position bag string :test #'(lambda (x y) (member y x)))))
           (if p
               (cons (subseq string 0 p) (split-string bag (subseq string p)))
             (list string))) )))

(defun string-begin-equal (a b)
  "Returns non-NIL if the beginning of 'a' matches 'b'"
  (and (>= (length a) (length b)) 
       (string-equal a b :end1 (length b))) )

(defun string-begin= (a b)
  "Returns non-NIL if the beginning of 'a' matches 'b'"
  (and (>= (length a) (length b)) 
       (string= a b :end1 (length b))) )


;;; ------------------------------------------------------------------------------------------
;;;  Futures
;;;

#||
(defstruct (future (:print-function print-future))
  (read-lock (mp/make-lock))
  (guess-lock (mp/make-lock))
  value)

(defun print-future (self sink depth)
  (if (future-guess-lock self)
      (format sink "#<~S unpredicted>" (type-of self))
    (if (and *print-level* (>= depth *print-level*))
        (format sink "#<~S predicted as ...>" (type-of self))
      (format sink "#<~S predicted as ~S>" (type-of self) (future-value self)))))

(defun future ()
  (let ((res (make-future)))
    (mp/seize-lock (future-guess-lock res))
    res))

(defun guess (future)
  (mp/with-lock ((future-read-lock future))
    (let ((lock (future-guess-lock future)))
      (when lock
        (mp/seize-lock lock))
      (future-value future))))

(defun predict (future value)
  (setf (future-value future) value)
  (let ((lock (future-guess-lock future)))
    (setf (future-guess-lock future) nil)
    (mp/release-lock lock))
  value)

;;; Future lists

(defun fcar (x) (car (guess x)))
(defun fcdr (x) (cdr (guess x)))
(defun fnull (x) (null (guess x)))
(defun fendp (x) (endp (guess x)))

(defmacro doflist ((var list &optional res) &body body)
  (let ((q (make-symbol "Q")))
    `(do ((,q ,list (fcdr ,q)))
         ((fendp ,q) ,res)
       (let ((,var (fcar ,q)))
         ,@body))))

(defun mapfcar (fun flist)
  (cond ((fendp flist) nil)
        ((cons (funcall fun (fcar flist)) (mapfcar fun (fcdr flist))))))

||#

;; Example:

;;  (setq f (future))

;; Thread 1: 
;;  (doflist (k f) (print k))

;; Thread 2:
;;  (setq f (cdr (predict f (cons 'foo (future)))))
;;  (setq f (cdr (predict f (cons 'bar (future)))))
;;  (predict f nil)
;;

;;;; -----------------------------------------------------------------------------------------
;;;;  Homebrew stream classes
;;;;

;; I am really tired of standard Common Lisp streams and thier incompatible implementations.

;; A gstream is an objects with obeys to the following protocol:

;; g/read-byte stream &optional (eof-error-p t) eof-value
;; g/unread-byte byte stream
;; g/read-char stream &optional (eof-error-p t) eof-value
;; g/unread-char char stream
;; g/write-char char stream
;; g/write-byte byte stream
;; g/finish-output stream
;; g/close stream &key abort

;; Additionally the follwing generic functions are implemented based
;; on the above protocol and may be reimplemented for any custom
;; stream class for performance.

;; g/write-string string stream &key start end
;; g/read-line stream &optional (eof-error-p t) eof-value
;; g/read-line* stream &optional (eof-error-p t) eof-value
;; g/read-byte-sequence sequence stream &key start end
;; g/read-char-sequence sequence stream &key start end
;; g/write-byte-sequence sequence stream &key start end
;; g/write-char-sequence sequence stream &key start end


;; The following classes exists

;; gstream
;; use-char-for-byte-stream-flavour
;; use-byte-for-char-stream-flavour
;; cl-stream
;; cl-byte-stream
;; cl-char-stream

(defclass gstream () ())

;;; use-char-for-byte-stream-flavour 

(defclass use-char-for-byte-stream-flavour () ())

(defmethod g/read-byte ((self use-char-for-byte-stream-flavour) &optional (eof-error-p t) eof-value)
  (let ((r (g/read-char self eof-error-p :eof)))
    (if (eq r :eof)
        eof-value
      (char-code r))))

(defmethod g/unread-byte (byte (self use-char-for-byte-stream-flavour))
  (g/unread-char (or (and #+CMU (<= byte char-code-limit) (code-char byte))
                     (error "Cannot stuff ~D. into a character." byte))
                 self))

(defmethod g/write-byte (byte (self use-char-for-byte-stream-flavour))
  (g/write-char (or (and #+CMU (<= byte char-code-limit) (code-char byte))
                    (error "Cannot stuff ~D. into a character." byte))
                self))

;;; use-byte-for-char-stream-flavour

(defclass use-byte-for-char-stream-flavour () ())

(defmethod g/read-char ((self use-byte-for-char-stream-flavour) &optional (eof-error-p t) eof-value)
  (let ((byte (g/read-byte self eof-error-p :eof)))
    (if (eq byte :eof)
        eof-value
      (let ((res (and #+CMU (<= byte char-code-limit) (code-char byte))))
        (or res
            (error "The byte ~D. could not been represented as character in your LISP implementation." byte))))))

(defmethod g/unread-char (char (self use-byte-for-char-stream-flavour))
  (g/unread-byte (char-code char) self))

(defmethod g/write-char (char (self use-byte-for-char-stream-flavour))
  (g/write-byte (char-code char) self))

;;; ------------------------------------------------------------
;;; Streams made up out of Common Lisp streams

;;; cl-stream

(defclass cl-stream (gstream)
  ((cl-stream :initarg :cl-stream)))

(defmethod g/finish-output ((self cl-stream))
  (with-slots (cl-stream) self
    (finish-output cl-stream)))

(defmethod g/close ((self cl-stream) &key abort)
  (with-slots (cl-stream) self
    (close cl-stream :abort abort)))

;;; cl-byte-stream

(defclass cl-byte-stream (use-byte-for-char-stream-flavour cl-stream)
  ((lookahead :initform nil)))

(defmethod g/read-byte ((self cl-byte-stream) &optional (eof-error-p t) eof-value)
  (with-slots (cl-stream lookahead) self
    (if lookahead
        (prog1 lookahead 
          (setf lookahead nil))
      (read-byte cl-stream eof-error-p eof-value))))

(defmethod g/unread-byte (byte (self cl-byte-stream))
  (with-slots (cl-stream lookahead) self
    (if lookahead
        (error "You cannot unread twice.")
      (setf lookahead byte))))

(defmethod g/write-byte (byte (self cl-byte-stream))
  (with-slots (cl-stream) self
    (write-byte byte cl-stream)))

(defmethod g/read-byte-sequence (sequence (input cl-byte-stream) &key (start 0) (end (length sequence)))
  (with-slots (cl-stream) input
    (read-byte-sequence sequence cl-stream :start start :end end)))

(defmethod g/write-byte-sequence (sequence (sink cl-byte-stream) &key (start 0) (end (length sequence)))
  (with-slots (cl-stream) sink
    (cl:write-sequence sequence cl-stream :start start :end end)))

;;; cl-char-stream

(defclass cl-char-stream (use-char-for-byte-stream-flavour cl-stream)
  ())

(defmethod g/read-char ((self cl-char-stream) &optional (eof-error-p t) eof-value)
  (with-slots (cl-stream) self
    (read-char cl-stream eof-error-p eof-value)))

(defmethod g/unread-char (char (self cl-char-stream))
  (with-slots (cl-stream) self
    (unread-char char cl-stream)))

(defmethod g/write-char (char (self cl-char-stream))
  (with-slots (cl-stream) self
    (write-char char cl-stream)))

;;; ------------------------------------------------------------
;;; General or fall back stream methods

(defmethod g/write-string (string (stream t) &key (start 0) (end (length string)))
  (do ((i start (+ i 1)))
      ((>= i end))
    (g/write-char (char string i) stream)))

(defmethod g/read-line ((stream t) &optional (eof-error-p t) eof-value) 
  (let ((res nil))
    (do ((c (g/read-char stream eof-error-p :eof)
            (g/read-char stream nil :eof)))
        ((or (eq c :eof) (char= c #\newline))
         (cond ((eq c :eof) 
                (values (if (null res) eof-value (coerce (nreverse res) 'string))
                        t))
               (t
                (values (coerce (nreverse res) 'string)
                        nil))))
      (push c res))))

(defmethod g/read-line* ((stream t) &optional (eof-error-p t) eof-value) 
  ;; Like read-line, but accepts CRNL, NL, CR as line termination
  (let ((res nil))
    (do ((c (g/read-char stream eof-error-p :eof)
            (g/read-char stream nil :eof)))
        ((or (eq c :eof) (char= c #\newline) (char= c #\return))
         (cond ((eq c :eof) 
                (values (if (null res) eof-value (coerce (nreverse res) 'string))
                        t))
               (t
                (when (char= c #\return)
                  (let ((d (g/read-char stream nil :eof)))
                    (unless (or (eq d :eof) (char= d #\newline))
                      (g/unread-char d stream))))
                (values (coerce (nreverse res) 'string)
                        nil))))
      (push c res))))

(defmethod g/read-byte-sequence (sequence (input t) &key (start 0) (end (length sequence)))
  (let ((i start) c)
    (loop
      (when (>= i end)
        (return i))
      (setf c (g/read-byte input nil :eof))
      (when (eq c :eof)
        (return i))
      (setf (elt sequence i) c)
      (incf i))))

(defmethod g/read-char-sequence (sequence (input t) &key (start 0) (end (length sequence)))
  (let ((i start) c)
    (loop
      (when (>= i end)
        (return i))
      (setf c (g/read-char input nil :eof))
      (when (eq c :eof)
        (return i))
      (setf (elt sequence i) c)
      (incf i))))

(defmethod g/write-byte-sequence (sequence (sink t) &key (start 0) (end (length sequence)))
  (do ((i start (+ i 1)))
      ((>= i end) i)
    (g/write-byte (aref sequence i) sink)))

;;; ----------------------------------------------------------------------------------------------------
;;;  Vector streams
;;;

;; Output

(defclass vector-output-stream (use-byte-for-char-stream-flavour)
  ((buffer :initarg :buffer)))

(defun g/make-vector-output-stream (&key (initial-size 100))
  (make-instance 'vector-output-stream
    :buffer (make-array initial-size :element-type '(unsigned-byte 8)
                        :fill-pointer 0
                        :adjustable t)))

(defmethod g/close ((self vector-output-stream) &key abort)
  (declare (ignorable self abort))
  nil)

(defmethod g/finish-output ((self vector-output-stream))
  nil)

(defmethod g/write-byte (byte (self vector-output-stream))
  (with-slots (buffer) self
    (vector-push-extend byte buffer 100)))

(defmethod g/write-byte-sequence (sequence (self vector-output-stream) &key (start 0) (end (length sequence)))
  (with-slots (buffer) self
    (adjust-array buffer (+ (length buffer) (- end start)))
    (replace buffer sequence :start1 (length buffer) :start2 start :end2 end)
    (setf (fill-pointer buffer) (+ (length buffer) (- end start)))
    end))

;;; ----------------------------------------------------------------------------------------------------
;;;  Echo streams

#||
(defclass echo-stream (use-byte-for-char-stream-flavour)
  ((echoed-to :initarg :echoed-to)))

(defun g/make-echo-stream (echoed-to)
  (make-instance 'echo-stream :echoed-to echoed-to))
||#

#||

Hmm unter PCL geht das nicht            ;-(

(defmethod g/read-byte ((stream stream) &optional (eof-error-p t) eof-value)
  (read-byte stream eof-error-p eof-value))

(defmethod g/read-char ((stream stream) &optional (eof-error-p t) eof-value)
  (read-char stream eof-error-p eof-value))

(defmethod g/unread-char (char (stream stream))
  (unread-char char stream))

(defmethod g/write-char (char (stream stream))
  (write-char char stream))

(defmethod g/write-byte (byte (stream stream))
  (write-byte byte stream))

(defmethod g/finish-output ((stream stream))
  (finish-output stream))

(defmethod g/close ((stream stream) &key abort)
  (close stream :abort abort))

||#

;;;; ----------------------------------------------------------------------------------------------------

#||
(let ((null (make-symbol "NULL")))

  (defstruct (future (:print-function print-future))
    (value null)
    (awaited-by nil))

  (defun print-future (self sink depth)
    (if (eq (future-value self) null)
        (format sink "#<~S unpredicted>" (type-of self))
      (if (and *print-level* (>= depth *print-level*))
          (format sink "#<~S predicted as ...>" (type-of self))
        (format sink "#<~S predicted as ~S>" (type-of self) (future-value self)))))

  (defun future ()
    (make-future))

  (defun guess (future)
    (when (eq (future-value future) null)
      (setf (future-awaited-by future) (mp/current-process))
      (mp/process-wait "Awaiting future" (lambda () (not (eq (future-value future) null))))
      (setf (future-awaited-by future) nil))
    (future-value future))

  (defun predict (future value)
    (setf (future-value future) value)
    (let ((aw (future-awaited-by future)))
      (when aw (mp/process-allow-schedule aw)))
    value)
  )
||#

(defun map-array (fun array &rest make-array-options)
  (let ((res (apply #'make-array (array-dimensions array) make-array-options)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref res i) (funcall fun (row-major-aref array i))))
    res))

;;----------------------------------------------------------------------------------------------------

(defun g/peek-char (&optional (peek-type nil) (source *standard-input*)
                              (eof-error-p T) eof-value)
  (cond ((eq peek-type T)
         (do ((ch (g/read-char source eof-error-p '%the-eof-object%)
                  (g/read-char source eof-error-p '%the-eof-object%)))
             ((or (eq ch '%the-eof-object%)
                  (not (white-space-p ch)))
              (cond ((eq ch '%the-eof-object%) eof-value)
                    (t (g/unread-char ch source) ch)) )))
        ((eq peek-type NIL)
         (let ((ch (g/read-char source eof-error-p '%the-eof-object%)))
           (cond ((eq ch '%the-eof-object%) eof-value)
                 (t (g/unread-char ch source)
                    ch))))
        ((characterp peek-type)
         (do ((ch (g/read-char source eof-error-p '%the-eof-object%)
                  (g/read-char source eof-error-p '%the-eof-object%)))
             ((or (eq ch '%the-eof-object%) (eql ch peek-type))
              (cond ((eq ch '%the-eof-object%) eof-value)
                    (t (g/unread-char ch source) ch)) )) ) ))



(defun cl-byte-stream->gstream (stream)
  (make-instance 'cl-byte-stream :cl-stream stream))

(defun cl-char-stream->gstream (stream)
  (make-instance 'cl-char-stream :cl-stream stream))

(defun g/open-inet-socket (&rest args)
  (multiple-value-bind (stream kind) (apply #'open-inet-socket args)
    (ecase kind
      #-CMU
      (:char (cl-char-stream->gstream stream))
      (:byte (cl-byte-stream->gstream stream)) )))

#||
(defun g/open-inet-socket-ssl (host port)
  (multiple-value-bind (stream) (gluser::make-ssl-client-socket host port)
    (cl-byte-stream->gstream stream)))
||#

(defun accept-connection (socket)
  (multiple-value-bind (stream kind) (accept-connection/low socket)
    (ecase kind
      (:char (cl-char-stream->gstream stream))
      (:byte (cl-byte-stream->gstream stream)) )))


;;; ----------------------------------------------------------------------------------------------------

(defvar *all-temporary-files* nil
  "List of all temporary files.")

(defun find-temporary-file (&key (type nil))
  (let ((temp-dir "/tmp/*")             ;since Motif is only available on unix, we subtly assume a unix host.
        (stream nil))
    (labels ((invent-name ()
               (merge-pathnames (make-pathname
                                 :type type
                                 :name
                                 (let ((*print-base* 35))
                                   (format nil "ws_~S" (random (expt 36 7)))))
                                temp-dir)))
      (unwind-protect
          (do ((name (invent-name) (invent-name)))
              ((setq stream (open name :direction :output :if-exists nil))
               (push name *all-temporary-files*)        ;remember this file
               name))
        (when stream
          (close stream)) ))))

(defun delete-temporary-file (filename)
  (setf *all-temporary-files* (delete filename *all-temporary-files*))
  (ignore-errors (delete-file filename)))

(defmacro with-temporary-file ((name-var &key type) &body body)
  (let ((name (gensym)))
    `(let* ((,name (find-temporary-file :type ,type))
            (,name-var ,name))
       (unwind-protect
           (progn ,@body)
         (when (open ,name :direction :probe)
           (delete-temporary-file ,name)))) ))

;;;;

(defun set-equal (x y &rest options)
  (null (apply #'set-exclusive-or x y options)))

;;;;

(defun maybe-parse-integer (string &key (radix 10))
  (cond ((not (stringp string)) nil)
        (t
         (let ((len (length string)))
           (cond ((= len 0) nil)
                 (t
                  (let ((start 0)
                        (vz +1)
                        (res 0))
                    (cond ((and (> len 1) (char= (char string 0) #\+))
                           (incf start))
                          ((and (> len 1) (char= (char string 0) #\-))
                           (setf vz -1)
                           (incf start)))
                    (do ((i start (+ i 1)))
                        ((= i len) (* vz res))
                      (let ((d (digit-char-p (char string i) radix)))
                        (if d
                            (setf res (+ (* radix res) d))
                          (return nil)))))))))))

;;;

(defun nop (&rest ignore)
  (declare (ignore ignore))
  nil)

(defmacro with-structure-slots ((type &rest slots) obj &body body)
  ;; Something like 'with-slots' but for structures. Assumes that the structure
  ;; slot accessors have the default name. Note that the structure type must
  ;; been provided.
  (let ((obj-var (make-symbol "OBJ")))
    `(LET ((,obj-var ,obj))
       (SYMBOL-MACROLET ,(mapcar (lambda (slot)
                                   (list slot
                                         `(,(intern (concatenate 'string (symbol-name type) "-" (symbol-name slot))
                                                    (symbol-package type))
                                           ,obj-var)))
                                 slots)
         ,@body))))

;;;; ----------------------------------------------------------------------------------------------------

;; Wir helfen den Compiler mal etwas auf die Spruenge ...
(defun compile-funcall (fn args)
  (cond ((eq fn '#'identity)
         (car args))
        ((eq fn '#'nop)
         `(progn ,args nil))
        ((and (consp fn) (eq (car fn) 'function))
         `(,(cadr fn) .,args))
        ((and (consp fn) (eq (car fn) 'lambda))
         `(,fn .,args))
        ((and (consp fn) (eq (car fn) 'curry))
         (compile-funcall (cadr fn) (append (cddr fn) args)))
        ((and (consp fn) (eq (car fn) 'rcurry))
         (compile-funcall (cadr fn) (append args (cddr fn))))
        (t
         (warn "Unable to inline funcall to ~S." fn)
         `(funcall ,fn .,args)) ))

(defmacro funcall* (fn &rest args)
  (compile-funcall fn args))

;; Ich mag mapc viel lieber als dolist, nur viele Compiler optimieren
;; das nicht, deswegen das Macro hier. Einige Compiler haben auch kein
;; DEFINE-COMPILER-MACRO :-(

(defmacro mapc* (fn list)
  (let ((g (gensym)))
    `(dolist (,g ,list)
       ,(compile-funcall fn (list g)))))

;; Das gleiche mit REDUCE und MAPCAR.

;; REDUCE arbeitet sowohl fuer Vectoren als auch fuer Listen. Wir
;; haben allerdings leider keinen vernuenftigen Zugriff auf
;; Deklarationen; Man koennte mit TYPEP herangehen und hoffen, dass
;; der Compiler das optimiert, ich fuerchte aber dass das nicht
;; funktionieren wird. Und CLISP verwirft Deklarationen ja total. Also
;; zwei Versionen: LREDUCE* und VREDUCE*

(defmacro vreduce* (fun seq &rest rest &key (key '#'identity) from-end start end 
                                            (initial-value nil initial-value?))
  (declare (ignore rest))
  (let (($start (make-symbol "start")) 
        ($end (make-symbol "end"))
        ($i (make-symbol "i"))
        ($accu (make-symbol "accu"))
        ($seq (make-symbol "seq")))
    (cond (from-end
         (cond (initial-value?
                `(LET* ((,$seq ,seq)
                        (,$start ,(or start 0))
                        (,$end ,(or end `(LENGTH ,$seq)))
                        (,$accu ,initial-value))
                   (DECLARE (TYPE FIXNUM ,$start ,$end))
                   (DO ((,$i (- ,$end 1) (THE FIXNUM (- ,$i 1))))
                       ((< ,$i ,$start) ,$accu)
                     (DECLARE (TYPE FIXNUM ,$i))
                     (SETF ,$accu (FUNCALL* ,fun (FUNCALL* ,key (AREF ,$seq ,$i)) ,$accu)) )))
               (t
                `(LET* ((,$seq ,seq)
                        (,$start ,(or start 0))
                        (,$end ,(or end `(LENGTH ,$seq))))
                   (DECLARE (TYPE FIXNUM ,$start ,$end))
                   (COND ((= 0 (- ,$end ,$start))
                          (FUNCALL* ,fun))
                         (T
                          (LET ((,$accu (FUNCALL* ,key (AREF ,$seq (- ,$end 1)))))
                            (DO ((,$i (- ,$end 2) (THE FIXNUM (- ,$i 1))))
                                ((< ,$i ,$start) ,$accu)
                              (DECLARE (TYPE FIXNUM ,$i))
                              (SETF ,$accu (FUNCALL* ,fun (FUNCALL* ,key (AREF ,$seq ,$i)) ,$accu)))))))) ))
        (t
         (cond (initial-value?
                `(LET* ((,$seq ,seq)
                        (,$start ,(or start 0))
                        (,$end ,(or end `(LENGTH ,$seq)))
                        (,$accu ,initial-value))
                   (DECLARE (TYPE FIXNUM ,$start ,$end))
                   (DO ((,$i ,$start (THE FIXNUM (+ ,$i 1))))
                       ((>= ,$i ,$end) ,$accu)
                     (DECLARE (TYPE FIXNUM ,$i))
                     (SETF ,$accu (FUNCALL* ,fun ,$accu (FUNCALL* ,key (AREF ,$seq ,$i)))) )))
               (t
                `(let* ((,$seq ,seq)
                        (,$start ,(or start 0))
                        (,$end ,(or end `(LENGTH ,$seq))))
                   (DECLARE (TYPE FIXNUM ,$start ,$end))
                   (COND ((= 0 (- ,$end ,$start))
                          (FUNCALL* ,fun))
                         (T
                          (LET ((,$accu (FUNCALL* ,key (AREF ,$seq ,$start))))
                            (DO ((,$i (+ ,$start 1) (+ ,$i 1)))
                                ((>= ,$i ,$end) ,$accu)
                              (DECLARE (TYPE FIXNUM ,$i))
                              (SETF ,$accu (FUNCALL* ,fun ,$accu (FUNCALL* ,key (AREF ,$seq ,$i)))))))))))))))
                         
(defmacro lreduce* (fun seq &rest rest &key (key '#'identity) from-end start end 
                                            (initial-value nil initial-value?))
  (cond ((or start end from-end)
         `(reduce ,fun ,seq .,rest))
        (t
         (cond (initial-value?
                (let (($accu (make-symbol "accu"))
                      ($k (make-symbol "k")))
                  `(LET* ((,$accu ,initial-value))
                     (DOLIST (,$k ,seq ,$accu)
                        (SETF ,$accu (FUNCALL* ,fun ,$accu (FUNCALL* ,key ,$k)))))))
               (t
                (let (($accu (make-symbol "accu"))
                      ($seq (make-symbol "seq"))
                      ($k (make-symbol "k")))
                  `(LET* ((,$seq ,seq))
                         (IF (NULL ,$seq)
                             (FUNCALL* ,fun)
                             (LET ((,$accu (FUNCALL* ,key (CAR ,$seq))))
                                  (DOLIST (,$k (CDR ,$seq) ,$accu)
                                          (SETF ,$accu (FUNCALL* ,fun ,$accu (FUNCALL* ,key ,$k)))))))) ))) ))


;;; Wenn wir so weiter machen, koennen wir bald gleich unseren eigenen
;;; Compiler schreiben ;-)

#||
(defmacro lreduce* (fun seq &rest x &key key &allow-other-keys)
  (let ((q (copy-list x)))
    (remf q :key)
    (cond (key
           `(reduce ,fun (map 'vector ,key ,seq) .,q))
          (t
           `(reduce ,fun ,seq .,q)))))

(defmacro vreduce* (fun seq &rest x &key key &allow-other-keys)
  (let ((q (copy-list x)))
    (remf q :key)
    (cond (key
           `(reduce ,fun (map 'vector ,key ,seq) .,q))
          (t
           `(reduce ,fun ,seq .,q)))))

||#

;; Stolen from Eclipse (http://elwoodcorp.com/eclipse/unique.htm

(defmacro with-unique-names ((&rest names) &body body)
  `(let (,@(mapcar (lambda (x) (list x `(gensym ',(concatenate 'string (symbol-name x) "-")))) names))
     .,body))


(defun gstream-as-string (gstream &optional (buffer-size 4096))
  (let ((buffer (g/make-string buffer-size :adjustable t)))
    (do* ((i 0 j)
          (j (g/read-char-sequence buffer gstream :start 0 :end buffer-size)
             (g/read-char-sequence buffer gstream :start i :end (+ i buffer-size)) ))
        ((= j i) (subseq buffer 0 j))
      (adjust-array buffer (list (+ j buffer-size))) )))

;;;; Generic hash tables

;; TODO: 
;; - automatic size adjustment
;; - sensible printer
;; - make-load-form?!

(defstruct g/hash-table
  hash-function                         ;hash function
  compare-function                      ;predicate to test for equality
  table                                 ;simple vector of chains
  size                                  ;size of hash table
  (nitems 0))                           ;number of items

(defun g/make-hash-table (&key (size 100) (hash-function #'sxhash) (compare-function #'eql))
  "Creates a generic hashtable;
   `size' is the default size of the table.
   `hash-function' (default #'sxhash) is a specific hash function
   `compare-function' (default #'eql) is a predicate to test for equality."
  (setf size (nearest-greater-prime size))
  (make-g/hash-table :hash-function hash-function
                     :compare-function compare-function
                     :table (make-array size :initial-element nil)
                     :size size
                     :nitems 0))
        
(defun g/hashget (hashtable key &optional (default nil))
  "Looks up the key `key' in the generic hash table `hashtable'. 
   Returns three values:
   value     - value, which as associated with the key, or `default' is no value 
               present.
   successp  - true, iff the key was found.
   key       - the original key in the hash table."
  ;; -> value ; successp ; key
  (let ((j (mod (funcall (g/hash-table-hash-function hashtable) key)
                (g/hash-table-size hashtable))))
    (let ((q (assoc key (aref (g/hash-table-table hashtable) j)
                    :test (g/hash-table-compare-function hashtable))))
      (if q
          (values (cdr q) t (car q))
        (values default nil)))))

(defun (setf g/hashget) (new-value hashtable key &optional (default nil))
  (declare (ignore default))
  (let ((j (mod (funcall (g/hash-table-hash-function hashtable) key)
                (g/hash-table-size hashtable))))
    (let ((q (assoc key (aref (g/hash-table-table hashtable) j)
                    :test (g/hash-table-compare-function hashtable))))
      (cond ((not (null q))
             (setf (cdr q) new-value))
            (t
             (push (cons key new-value)
                   (aref (g/hash-table-table hashtable) j))
             (incf (g/hash-table-nitems hashtable))))))
  new-value)

(defun resize-hash-table (hashtable new-size)
  "Adjust the size of a generic hash table. (the size is round to the next greater prime number)."
  (setf new-size (nearest-greater-prime new-size))
  (let ((new-table (make-array new-size :initial-element nil)))
    (dotimes (i (g/hash-table-size hashtable))
      (dolist (k (aref (g/hash-table-table hashtable) i))
        (push k (aref new-table
                      (mod (funcall (g/hash-table-hash-function hashtable) (car k))
                           new-size)))))
    (setf (g/hash-table-table hashtable) new-table
          (g/hash-table-size hashtable) new-size)
    hashtable))

(defun g/clrhash (hashtable)
  "Clears a generic hash table."
  (dotimes (i (g/hash-table-size hashtable))
    (setf (aref (g/hash-table-table hashtable) i) nil))
  (setf (g/hash-table-nitems hashtable) nil)
  hashtable)

;; hash code utilities

(defconstant +fixnum-bits+
    (1- (integer-length most-positive-fixnum))
  "Pessimistic approximation of the number of bits of fixnums.")

(defconstant +fixnum-mask+
    (1- (expt 2 +fixnum-bits+))
  "Pessimistic approximation of the largest bit-mask, still being a fixnum.")

(defun stir-hash-codes (a b)
  "Stirs two hash codes together; always returns a fixnum.
   When applied sequenitally the first argument should be used as accumulator."
  ;; ich mach das mal wie Bruno
  (logand +fixnum-mask+
          (logxor (logior (logand +fixnum-mask+ (ash a 5))
                          (logand +fixnum-mask+ (ash a (- 5 +fixnum-bits+)))) 
                  b)))

(defun hash-sequence (sequence hash-function &optional (accu 0))
  "Applies the hash function `hash-function' to each element of `sequence' and
   stirs the resulting hash codes together using STIR-HASH-CODE starting from 
   `accu'."
  (map nil (lambda (item)
             (setf accu (stir-hash-codes accu (funcall hash-function item))))
       sequence)
  accu)

;; some specific hash functions

(defun hash/string-equal (string)
  "Hash function compatible with STRING-EQUAL."
  (hash-sequence string (lambda (char)
                          (sxhash (char-upcase char)))))

;; some specific hash tables

(defun make-string-equal-hash-table (&rest options)
  "Constructs a new generic hash table using STRING-EQUAL as predicate."
  (apply #'g/make-hash-table 
         :hash-function #'hash/string-equal
         :compare-function #'string-equal
         options))

;; prime numbers

(defun primep (n)
  "Returns true, iff `n' is prime."
  (and (> n 2)
       (do ((i 2 (+ i 1)))
           ((> (* i i) n) t)
         (cond ((zerop (mod n i)) (return nil))))))

(defun nearest-greater-prime (n)
  "Returns the smallest prime number no less than `n'."
  (cond ((primep n) n)
        ((nearest-greater-prime (+ n 1)))))


;;;

(defun grind-documentation-string (string &optional (sink *standard-output*))
  ;; some people say:
  ;;   (defun foo ()
  ;;     "This function
  ;;   frobinates its two arguments.")
  ;; some say:
  ;;   (defun foo ()
  ;;     "This function
  ;;      frobinates its two arguments.")
  ;; instead.
  (let ((min-indention nil))
    ;; We sort this out by finding the minimum indent in all but the first line.
    (with-input-from-string (in string)
      (read-line in nil nil)            ;ignore first line
      (do ((x (read-line in nil nil) (read-line in nil nil)))
          ((null x))
        (let ((p (position-if-not (curry #'char= #\space) x)))
          (when p
            (setf min-indention (min* min-indention p))))))
    (setf min-indention (or min-indention 0))
    ;; Now we could dump the string
    (with-input-from-string (in string)
      ;; first line goes unindented
      (let ((x (read-line in nil nil)))
        (when x 
          (fresh-line sink)
          (write-string x sink)))
      (do ((x (read-line in nil nil) (read-line in nil nil)))
          ((null x))
        (terpri sink)
        (when (< min-indention (length x))
          (write-string x sink :start min-indention)))))
  (values))

(defun ap (&rest strings)
  "A new apropos."
  (let ((res nil))
    (do-all-symbols (symbol)
      (unless (member symbol res)
        (when (every (lambda (string)
                       (search string (symbol-name symbol)))
                     strings)
          (push symbol res))))
    (dolist (k res)
      (print k)
      (when (fboundp k)
        (princ ", function"))
      (when (boundp k)
        (princ ", variable"))
      )))

