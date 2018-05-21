;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: FXML; readtable: runes; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: XML parser
;;;   Created: 1999-07-17
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;    Author: Henrik Motakef <hmot@henrik-motakef.de>
;;;    Author: David Lichteblau <david@lichteblau.com>
;;;    Author: David Lichteblau <pmr@tbrss.com>
;;;   License: Lisp-LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1999 by Gilbert Baumann
;;;  (c) copyright 2003 by Henrik Motakef
;;;  (c) copyright 2004 knowledgeTools Int. GmbH
;;;  (c) copyright 2004 David Lichteblau
;;;  (c) copyright 2005 David Lichteblau
;;;  (c) copyright 2014 Paul M. Rodriguez
;;;  (c) copyright 2015 Paul M. Rodriguez

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

;;; Streams

;;; xstreams

;; For reading runes, I defined my own streams, called xstreams,
;; because we want to be fast. A function call or even a method call
;; per character is not acceptable, instead of that we define a
;; buffered stream with an advertised buffer layout, so that we
;; could use the trick stdio uses: READ-RUNE and PEEK-RUNE are macros,
;; directly accessing the buffer and only calling some underflow
;; handler in case of stream underflows. This will yield to quite a
;; performance boost vs calling READ-BYTE per character.

;; Also we need to do encoding and character set conversion on input,
;; this better done at large chunks of data rather than on a character
;; by character basis. This way we need a dispatch on the active
;; encoding only once in a while, instead of for each character. This
;; allows us to use a CLOS interface to do the underflow handling.

;;; zstreams

;; Now, for reading tokens, we define another kind of streams, called
;; zstreams. These zstreams also maintain an input stack to implement
;; inclusion of external entities. This input stack contains xstreams
;; or the special marker :STOP. Such a :STOP marker indicates, that
;; input should not continue there, but well stop; that is simulate an
;; EOF. The user is then responsible to pop this marker off the input
;; stack.
;;
;; This input stack is also used to detect circular entity inclusion.

;; The zstream tokenizer recognizes the following types of tokens and
;; is controlled by the *DATA-BEHAVIOUR* flag. (Which should become a
;; slot of zstreams instead).

;; Common
;;    :xml-decl (<target> . <content>)    ;processing-instruction starting with "<?xml"
;;    :pi (<target> . <content>)        ;processing-instruction
;;    :stag (<name> . <atts>)           ;start tag
;;    :etag (<name> . <atts>)           ;end tag
;;    :ztag (<name> . <atts>)           ;empty tag
;;    :<!ELEMENT
;;    :<!ENTITY
;;    :<!ATTLIST
;;    :<!NOTATION
;;    :<!DOCTYPE
;;    :<![
;;    :comment <content>

;; *data-behaviour* = :DTD
;;
;;    :nmtoken <interned-rod>
;;    :#required
;;    :#implied
;;    :#fixed
;;    :#pcdata
;;    :s
;;    :\[ :\] :\( :\) :|\ :\> :\" :\' :\, :\? :\* :\+

;; *data-behaviour* = :DOC
;;
;;    :entity-ref <interned-rod>
;;    :cdata <rod>


;;; TODO
;;
;; o provide for a faster DOM
;;
;; o morph zstream into a context object and thus also get rid of
;;   special variables. Put the current DTD there too.
;;   [partly done]

;; o the *scratch-pad* hack should become something much more
;;   reentrant, we could either define a system-wide resource
;;   or allocate some scratch-pads per context.
;;   [for thread-safety reasons the array are allocated per context now,
;;   reentrancy is still open]

;; o CR handling in utf-16 deocders
;;
;; o UCS-4 reader
;;
;; o max depth together with circle detection
;;   (or proof, that our circle detection is enough).
;;   [gemeint ist zstream-push--david]
;;
;; o better extensibility wrt character representation, one may want to
;;   have
;;    - UCS-4  in vectoren
;;
;; o xstreams auslagern, documententieren und dann auch in SGML und
;;   CSS parser verwenden. (halt alles was zeichen liest).
;;   [ausgelagert sind sie; dokumentiert "so la la"; die Reintegration
;;   in Closure ist ein ganz anderes Thema]
;;
;; o recording of source locations for nodes.
;;
;; o based on the DTD and xml:space attribute implement HTML white
;;   space rules.
;;
;; o on a parser option, do not expand external entities.

;;;; Validity constraints:
;;;; (00) Root Element Type                     like (03), c.f. MAKE-ROOT-MODEL
;;;; (01) Proper Declaration/PE Nesting         P/MARKUP-DECL
;;;; (02) Standalone Document Declaration       all over the place [*]
;;;; (03) Element Valid                         VALIDATE-*-ELEMENT, -CHARACTERS
;;;; (04) Attribute Value Type                  VALIDATE-ATTRIBUTE
;;;; (05) Unique Element Type Declaration       DEFINE-ELEMENT
;;;; (06) Proper Group/PE Nesting               P/CSPEC
;;;; (07) No Duplicate Types                    LEGAL-CONTENT-MODEL-P
;;;; (08) ID                                    VALIDATE-ATTRIBUTE
;;;; (09) One ID per Element Type               DEFINE-ATTRIBUTE
;;;; (10) ID Attribute Default                  DEFINE-ATTRIBUTE
;;;; (11) IDREF                                 VALIDATE-ATTRIBUTE, P/DOCUMENT
;;;; (12) Entity Name                           VALIDATE-ATTRIBUTE
;;;; (13) Name Token                            VALIDATE-ATTRIBUTE
;;;; (14) Notation Attributes                   VALIDATE-ATTRIBUTE, P/ATT-TYPE
;;;; (15) One Notation Per Element Type         DEFINE-ATTRIBUTE
;;;; (16) No Notation on Empty Element          DEFINE-ELEMENT, -ATTRIBUTE
;;;; (17) Enumeration                           VALIDATE-ATTRIBUTE
;;;; (18) Required Attribute                    PROCESS-ATTRIBUTES
;;;; (19) Attribute Default Legal               DEFINE-ATTRIBUTE
;;;; (20) Fixed Attribute Default               VALIDATE-ATTRIBUTE
;;;; (21) Proper Conditional Section/PE Nesting P/CONDITIONAL-SECT, ...
;;;; (22) Entity Declared                       [**]
;;;; (23) Notation Declared                     P/ENTITY-DEF, P/DOCUMENT
;;;; (24) Unique Notation Name                  DEFINE-NOTATION
;;;;
;;;; [*] Perhaps we could revert the explicit checks of (02), if we did
;;;; _not_ read external subsets of standalone documents when parsing in
;;;; validating mode.  Violations of VC (02) constraints would then appear as
;;;; wellformedness violations, right?
;;;;
;;;; [**] Although I haven't investigated this properly yet, I believe that
;;;; we check this VC together with the WFC even in non-validating mode.

(in-package :fxml)
(in-readtable :runes)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *fast* '(optimize (speed 3) (safety 0) (debug 0)
                         (compilation-speed 0) (space 0))))

(deftype ufixnum () `(unsigned-byte ,(integer-length most-positive-fixnum)))

;;; parser context

(defstruct (context (:conc-name nil))
  handler
  (dtd nil)
  model-stack
  ;; xml:base machen wir fuer klacks mal gleich als expliziten stack:
  base-stack
  (referenced-notations '())
  (id-table (%make-rod-hash-table))
  ;; FIXME: Wofuer ist name-hashtable da?  Will man das wissen?
  (name-hashtable (make-rod-hashtable :size 2000))
  (standalone-p nil)
  (entity-resolver nil)
  (disallow-internal-subset nil)
  main-zstream
  (original-rods (make-hash-table :test 'equalp)))

(defvar *ctx* nil)
(declaim (type (or null context) *ctx*))

(defvar *expand-pe-p* nil)

(defparameter *initial-namespace-bindings*
  '((#"" . nil)
    (#"xmlns" . #"http://www.w3.org/2000/xmlns/")
    (#"xml" . #"http://www.w3.org/XML/1998/namespace")))

(defparameter *namespace-bindings* *initial-namespace-bindings*)

;;;; ---------------------------------------------------------------------------
;;;; xstreams
;;;;


(defstruct (stream-name
            (:print-function print-stream-name))
  (entity-name "Anonymous stream" :type rod)
  (entity-kind :main :type keyword)
  (uri (error "No URI") :type (or quri:uri null) :read-only t))

(defun print-stream-name (object stream depth)
  (declare (ignore depth))
  (format stream "[~A ~S ~A]"
          (rod-string (stream-name-entity-name object))
          (stream-name-entity-kind object)
          (stream-name-uri object)))

(deftype read-element () 'rune)

(defun call-with-open-xstream (fn stream)
  (unwind-protect
       (funcall fn stream)
    (close-xstream stream)))

(defmacro with-open-xstream ((var value) &body body)
  (with-thunk (body var)
    `(call-with-open-xstream ,body ,value)))

(defun call-with-open-xfile (continuation &rest open-args)
  (let ((input (apply #'open (car open-args) :element-type '(unsigned-byte 8) (cdr open-args))))
    (unwind-protect
         (progn
           (funcall continuation (make-xstream input)))
      (close input))))

(defmacro with-open-xfile ((stream &rest open-args) &body body)
  (with-thunk (body stream)
    `(call-with-open-xfile ,body ,@open-args)))

;;;; -------------------------------------------------------------------
;;;; Rechnen mit Runen
;;;;

;; Let us first define fast fixnum arithmetric get rid of type
;; checks. (After all we know what we do here).

(defmacro fx-op (op &rest xs)
  `(the fixnum (,op ,@(mapcar (lambda (x) `(the fixnum ,x)) xs))))
(defmacro fx-pred (op &rest xs)
  `(,op ,@(mapcar (lambda (x) `(the fixnum ,x)) xs)))

(defmacro %+   (&rest xs) `(fx-op + ,@xs))
(defmacro %-   (&rest xs) `(fx-op - ,@xs))
(defmacro %*   (&rest xs) `(fx-op * ,@xs))
(defmacro %/   (&rest xs) `(fx-op floor ,@xs))
(defmacro %and (&rest xs) `(fx-op logand ,@xs))
(defmacro %ior (&rest xs) `(fx-op logior ,@xs))
(defmacro %xor (&rest xs) `(fx-op logxor ,@xs))
(defmacro %ash (&rest xs) `(fx-op ash ,@xs))
(defmacro %mod (&rest xs) `(fx-op mod ,@xs))

(defmacro %=  (&rest xs)  `(fx-pred = ,@xs))
(defmacro %<= (&rest xs)  `(fx-pred <= ,@xs))
(defmacro %>= (&rest xs)  `(fx-pred >= ,@xs))
(defmacro %<  (&rest xs)  `(fx-pred < ,@xs))
(defmacro %>  (&rest xs)  `(fx-pred > ,@xs))

;;; XXX Geschwindigkeit dieser Definitionen untersuchen!

(defmacro rune-op (op &rest xs)
  `(code-rune (,op ,@(mapcar (lambda (x) `(rune-code ,x)) xs))))
(defmacro rune-pred (op &rest xs)
  `(,op ,@(mapcar (lambda (x) `(rune-code ,x)) xs)))

(defmacro %rune+   (&rest xs) `(rune-op + ,@xs))
(defmacro %rune-   (&rest xs) `(rune-op - ,@xs))
(defmacro %rune*   (&rest xs) `(rune-op * ,@xs))
(defmacro %rune/   (&rest xs) `(rune-op floor ,@xs))
(defmacro %rune-and (&rest xs) `(rune-op logand ,@xs))
(defmacro %rune-ior (&rest xs) `(rune-op logior ,@xs))
(defmacro %rune-xor (&rest xs) `(rune-op logxor ,@xs))
(defmacro %rune-ash (a b) `(code-rune (ash (rune-code ,a) ,b)))
(defmacro %rune-mod (&rest xs) `(rune-op mod ,@xs))

(defmacro %rune=  (&rest xs)  `(rune-pred = ,@xs))
(defmacro %rune<= (&rest xs)  `(rune-pred <= ,@xs))
(defmacro %rune>= (&rest xs)  `(rune-pred >= ,@xs))
(defmacro %rune<  (&rest xs)  `(rune-pred < ,@xs))
(defmacro %rune>  (&rest xs)  `(rune-pred > ,@xs))

;;;; ---------------------------------------------------------------------------
;;;; rod hashtable
;;;;

;;; make-rod-hashtable
;;; rod-hash-get hashtable rod &optional start end -> value ; successp
;;; (setf (rod-hash-get hashtable rod &optional start end) new-value
;;;

(defstruct (rod-hashtable (:constructor make-rod-hashtable/low))
  (size (error "No size") :type alexandria:array-index) ;size of table
  (table (error "No table") :type simple-vector))

(defun make-rod-hashtable (&key (size 200) (real-size (nearest-greater-prime size)))
  (make-rod-hashtable/low
   :size real-size
   :table (make-array real-size :initial-element nil)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +fixnum-bits+
    (1- (integer-length most-positive-fixnum))
    "Pessimistic approximation of the number of bits of fixnums.")

  (defconstant +fixnum-mask+
    (1- (expt 2 +fixnum-bits+))
    "Pessimistic approximation of the largest bit-mask, still being a fixnum."))

(definline stir (a b)
  (%and +fixnum-mask+
        (%xor (%ior (%ash (%and a #.(ash +fixnum-mask+ -5)) 5)
                    (%ash a #.(- 5 +fixnum-bits+)))
              b)))

(definline rod-hash (rod start end)
  "Compute a hash code out of a rod."
  (let ((res (%- end start)))
    (do ((i start (%+ i 1)))
        ((%= i end))
      (declare (type fixnum i))
      (setf res (stir res (rune-code (%rune rod i)))))
    res))

(defmacro rod=* (x y &key (start1 0) (end1 (length x))
                          (start2 0) (end2 (length y)))
  `(rod=** ,x ,y ,start1 ,end1 ,start2 ,end2))

;;; NB This is *not* the same thing as string=. In particular, the
;;; start and end arguments may be called with invalid indices.
(definline rod=** (x y start1 end1 start2 end2)
  (and (%= (%- end1 start1) (%- end2 start2))
       (do ((i start1 (%+ i 1))
            (j start2 (%+ j 1)))
           ((%= i end1)
            t)
         (unless (rune= (%rune x i) (%rune y j))
           (return nil)))))

(defun rod-hash-get (hashtable rod &optional (start 0) (end (length rod)))
  (declare (type (simple-array rune (*)) rod))
  (let ((j (%mod (rod-hash rod start end)
                 (rod-hashtable-size hashtable))))
    (dolist (q (svref (rod-hashtable-table hashtable) j)
               (values nil nil nil))
      (declare (type cons q))
      (when (rod=** (car q) rod 0 (length (the (simple-array rune (*)) (car q))) start end)
        (return (values (cdr q) t (car q)))))))

(definline rod-subseq* (source start &optional (end (length source)))
  (subseq source start end))

(definline rod-subseq** (source start &optional (end (length source)))
  (subseq source start end))

(defun rod-hash-set (new-value hashtable rod &optional (start 0) (end (length rod)))
  (let ((j (%mod (rod-hash rod start end)
                 (rod-hashtable-size hashtable)))
        (key nil))
    (dolist (q (svref (rod-hashtable-table hashtable) j)
               (progn
                 (setf key (rod-subseq* rod start end))
                 (push (cons key new-value)
                       (aref (rod-hashtable-table hashtable) j))))
      (when (rod=* (car q) rod :start2 start :end2 end)
        (setf key (car q))
        (setf (cdr q) new-value)
        (return)))
    (values new-value key)))

(defun (setf rod-hash-get) (new-value hashtable rod &optional (start 0) (end (length rod)))
  (rod-hash-set new-value hashtable rod start end))

(defun intern-name (rod &optional (start 0) (end (length rod)) &aux (ctx *ctx*))
  (multiple-value-bind (value successp key) (rod-hash-get (name-hashtable ctx) rod start end)
    (declare (ignore value))
    (if successp
        key
        (nth-value 1 (rod-hash-set t (name-hashtable ctx) rod start end)))))

;;;; ---------------------------------------------------------------------------
;;;;
;;;;  rod collector
;;;;

(defvar *scratch-pad*)
(defvar *scratch-pad-2*)
(defvar *scratch-pad-3*)
(defvar *scratch-pad-4*)

(declaim (type (simple-array rune (*))
               *scratch-pad* *scratch-pad-2* *scratch-pad-3* *scratch-pad-4*))

(defun call-with-scratch-pads (thunk)
  (let ((*scratch-pad*   (make-array 1024 :element-type 'rune))
        (*scratch-pad-2* (make-array 1024 :element-type 'rune))
        (*scratch-pad-3* (make-array 1024 :element-type 'rune))
        (*scratch-pad-4* (make-array 1024 :element-type 'rune)))
    (funcall thunk)))

(defmacro with-scratch-pads ((&optional) &body body)
  (with-thunk (body)
    `(call-with-scratch-pads ,body)))

(defmacro %put-unicode-char (code-var put)
  `(,put (code-rune ,code-var)))

(defun adjust-array-by-copying (old-array new-size)
  "Adjust an array by copying and thus ensures, that result is a SIMPLE-ARRAY."
  (let ((res (make-array new-size :element-type (array-element-type old-array))))
    (replace res old-array
             :start1 0 :end1 (length old-array)
             :start2 0 :end2 (length old-array))
    res))

(defmacro with-rune-collector-aux (scratch collect body mode collect-all)
  (setf collect-all (or collect-all (gensym #.(string 'collect-all))))
  (alexandria:with-gensyms (n i b)
    `(let* ((,b ,scratch)
            (,n (length ,b))
            (,i 0))
       (declare (type alexandria:array-index ,n ,i))
       (macrolet
           ((,collect-all (xs)
              `((lambda (xs)
                  (declare #.*fast*)
                  (when (%>= (+ ,',i (length xs)) ,',n)
                    (setf ,',n (* 2 ,',n))
                    (setf ,',b
                          (setf ,',scratch
                                (adjust-array-by-copying ,',b ,',n))))
                  (loop for x across xs do
                    (setf (aref (the (simple-array rune (*)) ,',b) ,',i) x)
                    (incf ,',i)))
                ,xs))
            (,collect (x &rest xs)
              (if xs
                  `((lambda (&rest xs)
                      (declare (dynamic-extent xs))
                      (declare #.*fast*)
                      (when (%>= (+ ,',i ,(1+ (length xs))) ,',n)
                        (setf ,',n (* 2 ,',n))
                        (setf ,',b
                              (setf ,',scratch
                                    (adjust-array-by-copying ,',b ,',n))))
                      (dolist (x xs)
                        (setf (aref (the (simple-array rune (*)) ,',b) ,',i) x)
                        (incf ,',i)))
                    ,x ,@xs)
                  `((lambda (x)
                      (declare #.*fast*)
                      (when (%>= ,',i ,',n)
                        (setf ,',n (* 2 ,',n))
                        (setf ,',b
                              (setf ,',scratch
                                    (adjust-array-by-copying ,',b ,',n))))
                      (setf (aref (the (simple-array rune (*)) ,',b) ,',i) x)
                      (incf ,',i))
                    ,x))))
         ,@body
         ,(ecase mode
            (:intern
             `(intern-name ,b 0 ,i))
            (:copy
             `(replace (make-rod ,i) ,b))
            (:raw
             `(values ,b 0 ,i)))))))

(defmacro with-rune-collector ((collect &optional collect-all) &body body)
  `(with-rune-collector-aux *scratch-pad* ,collect ,body :copy ,collect-all))

(defmacro with-rune-collector-2 ((collect &optional collect-all) &body body)
  `(with-rune-collector-aux *scratch-pad-2* ,collect ,body :copy ,collect-all))

(defmacro with-rune-collector-3 ((collect &optional collect-all) &body body)
  `(with-rune-collector-aux *scratch-pad-3* ,collect ,body :copy ,collect-all))

(defmacro with-rune-collector-4 ((collect &optional collect-all) &body body)
  `(with-rune-collector-aux *scratch-pad-4* ,collect ,body :copy ,collect-all))

(defmacro with-rune-collector/intern ((collect &optional collect-all) &body body)
  `(with-rune-collector-aux *scratch-pad* ,collect ,body :intern ,collect-all))

(defmacro with-rune-collector/raw ((collect &optional collect-all) &body body)
  `(with-rune-collector-aux *scratch-pad* ,collect ,body :raw ,collect-all))

;;;;  ---------------------------------------------------------------------------
;;;;  DTD
;;;;

(defvar *validate* t)
(defvar *external-subset-p* nil)

(defstruct-read-only attdef
  ;; an attribute definition
  element       ;name of element this attribute belongs to
  name          ;name of attribute
  type          ;type of attribute; either one of :CDATA, :ID, :IDREF, :IDREFS,
                                        ; :ENTITY, :ENTITIES, :NMTOKEN, :NMTOKENS, or
                                        ; (:NOTATION <name>*)
                                        ; (:ENUMERATION <name>*)
  default       ;default value of attribute:
                                        ; :REQUIRED, :IMPLIED, (:FIXED content) or (:DEFAULT content)
  (external-p *external-subset-p*)
  )

(defstruct elmdef
  ;; an element definition
  name          ;name of the element
  content       ;content model            [*]
  attributes    ;list of defined attributes
  compiled-cspec ;cons of validation function for contentspec
  (external-p *external-subset-p*)
  )

(define-condition xml-parse-error (simple-error) ()
  (:documentation
   "Superclass of all conditions signalled by the FXML parser."))

(define-condition well-formedness-violation (xml-parse-error) ()
  (:documentation
   "This condition is signalled for all well-formedness violations.

    Note for validating mode: Sometimes violations of well-formedness are
    first detected as validity errors by the parser and signalled as
    instances of @class{validity-error} rather
    than well-formedness-violation."))

(define-condition validity-error (xml-parse-error) ()
  (:documentation
   "Reports the violation of a validity constraint."))

;; We make some effort to signal end of file as a special condition, but we
;; don't actually try very hard.  Not sure whether we should.  Right now I
;; would prefer not to document this class.
(define-condition end-of-xstream (well-formedness-violation) ())

(define-condition xml-security-error (error) ())

(define-condition external-reference-forbidden (xml-security-error)
  ((pubid :initarg :pubid :reader external-reference-pubid)
   (sysid :initarg :sysid :reader external-reference-sysid))
  (:report (lambda (c s)
             (with-slots (sysid) c
               (format s "External reference forbidden: ~a" sysid)))))

(define-condition dtd-forbidden (xml-security-error)
  ((name :initarg :name :reader dtd-name)
   (sysid :initarg :sysid :reader dtd-sysid)
   (pubid :initarg :pubid :reader dtd-pubid))
  (:report (lambda (c s)
             (with-slots (name sysid pubid) c
               (format s "Forbidden DTD (~a, ~a, ~a)"
                       name sysid pubid)))))

(defvar *forbid-entities*)

(define-condition entities-forbidden (xml-security-error)
  ((name :initarg :name :reader entity-name)
   (value :initarg :value :reader entity-value))
  (:report (lambda (c s)
             (with-slots (name) c
               (format s "Entity declaration in internal subset: ~a" name)))))

(defun describe-xstream (x s)
  (format s "  Line ~D, column ~D in ~A~%"
          (xstream-line-number x)
          (xstream-column-number x)
          (let ((name (xstream-name x)))
            (cond
              ((null name)
               "<anonymous stream>")
              ((eq :main (stream-name-entity-kind name))
               (stream-name-uri name))
              (t
               name)))))

(defstruct zstream
  token-category
  token-semantic
  input-stack)

(defun build-error-message (stream message)
  (with-output-to-string (s)
    (let* ((zmain (if *ctx* (main-zstream *ctx*) nil))
           (zstream (if (zstream-p stream) stream zmain))
           (xstream (if (xstream-p stream) stream nil)))
      (write-line message s)
      (when xstream
        (write-line "Location:" s)
        (describe-xstream xstream s))
      (when zstream
        (let ((stack
                (remove xstream (remove :stop (zstream-input-stack zstream)))))
          (when stack
            (write-line "Context:" s)
            (dolist (x stack)
              (describe-xstream x s)))))
      (when (and zmain (not (eq zstream zmain)))
        (let ((stack
                (remove xstream (remove :stop (zstream-input-stack zmain)))))
          (when stack
            (write-line "Context in main document:" s)
            (dolist (x stack)
              (describe-xstream x s))))))))

(defun %error (class stream message &rest args)
  (apply #'error class
         :format-control "~A"
         :format-arguments (list (build-error-message stream message))
         args))

(defun validity-error (fmt &rest args)
  (%error 'validity-error
          nil
          (format nil "Document not valid: ~?" fmt args)))

(defun wf-error (stream fmt &rest args)
  (%error 'well-formedness-violation
          stream
          (format nil "Document not well-formed: ~?" fmt args)))

(defun wf-error* (type stream fmt fmt-args &rest args)
  (apply #'%error type stream
         (format nil "Document not well-formed: ~?" fmt fmt-args)
         args))

(define-condition undefined-entity (well-formedness-violation)
  ((name :initarg :name :reader undefined-entity-name)))

(defun undefined-entity (stream entity-name)
  (let ((name (rod-string entity-name)))
    (wf-error* 'undefined-entity
               stream
               "Entity '~A' is not defined."
               (list name)
               :name name)))

(define-condition undeclared-namespace (well-formedness-violation)
  ((prefix :initarg :prefix :reader undeclared-namespace-prefix)))

(defun undeclared-namespace (prefix)
  (let ((prefix (rod-string prefix)))
    (wf-error* 'undeclared-namespace nil
               "Undeclared namespace prefix: ~A"
               (list (rod-string prefix))
               :prefix prefix)))

(defun eox (stream &optional x &rest args)
  (%error 'end-of-xstream
          stream
          (format nil "End of file~@[: ~?~]" x args)))

(defclass fxml-parser (fxml.sax:sax-parser)
  ((ctx :initarg :ctx :reader parser.ctx)))

(defun parser-xstream (parser)
  (car (zstream-input-stack (main-zstream (parser.ctx parser)))))

(defun parser-stream-name (parser)
  (let ((xstream (parser-xstream parser)))
    (if xstream
        (xstream-name xstream)
        nil)))

(defmethod fxml.sax:line-number ((parser fxml-parser))
  (let ((x (parser-xstream parser)))
    (if x
        (xstream-line-number x)
        nil)))

(defmethod fxml.sax:column-number ((parser fxml-parser))
  (let ((x (parser-xstream parser)))
    (if x
        (xstream-column-number x)
        nil)))

(defmethod fxml.sax:system-id ((parser fxml-parser))
  (let ((name (parser-stream-name parser)))
    (if name
        (stream-name-uri name)
        nil)))

(defmethod fxml.sax:xml-base ((parser fxml-parser))
  (let ((uri (car (base-stack (parser.ctx parser)))))
    (if (or (null uri) (stringp uri))
        uri
        (quri:render-uri uri nil))))

(definline copy-cons (x)
  (cons (car x) (cdr x)))

(defun validate-start-element (ctx name)
  (when *validate*
    (let* ((pair (car (model-stack ctx)))
           (newval (funcall (car pair) name)))
      (unless newval
        (validity-error "(03) Element Valid: ~A" (rod-string name)))
      (setf (car pair) newval)
      (let ((e (find-element name (dtd ctx))))
        (unless e
          (validity-error "(03) Element Valid: no definition for ~A"
                          (rod-string name)))
        (maybe-compile-cspec e)
        (push (copy-cons (elmdef-compiled-cspec e)) (model-stack ctx))))))

(defun validate-end-element (ctx name)
  (when *validate*
    (let ((pair (car (model-stack ctx))))
      (unless (eq (funcall (car pair) nil) t)
        (validity-error "(03) Element Valid: ~A" (rod-string name)))
      (pop (model-stack ctx)))))

(defun validate-characters (ctx rod)
  (when *validate*
    (let ((pair (car (model-stack ctx))))
      (unless (funcall (cdr pair) rod)
        (validity-error "(03) Element Valid: unexpected PCDATA")))))

(defun standalone-check-necessary-p (def)
  (and *validate*
       (standalone-p *ctx*)
       (etypecase def
         (elmdef (elmdef-external-p def))
         (attdef (attdef-external-p def)))))

;; attribute validation, defaulting, and normalization -- except for for
;; uniqueness checks, which are done after namespaces have been declared
(defun process-attributes (ctx name attlist)
  (let ((e (find-element name (dtd ctx))))
    (cond
      (e
       (dolist (ad (elmdef-attributes e)) ;handle default values
         (unless (get-attribute (attdef-name ad) attlist)
           (case (attdef-default ad)
             (:IMPLIED)
             (:REQUIRED
              (when *validate*
                (validity-error "(18) Required Attribute: ~S not specified"
                                (rod-string (attdef-name ad)))))
             (t
              (when (standalone-check-necessary-p ad)
                (validity-error "(02) Standalone Document Declaration: missing attribute value"))
              (push (fxml.sax:make-attribute :qname (attdef-name ad)
                                             :value (cadr (attdef-default ad))
                                             :specified-p nil)
                    attlist)))))
       (dolist (a attlist)		;normalize non-CDATA values
         (let* ((qname (fxml.sax:attribute-qname a))
                (adef (find-attribute e qname)))
           (when adef
             (when (and *validate*
                        fxml.sax:*namespace-processing*
                        (eq (attdef-type adef) :ID)
                        (find #/: (fxml.sax:attribute-value a)))
               (validity-error "colon in ID attribute"))
             (unless (eq (attdef-type adef) :CDATA)
               (let ((canon (canon-not-cdata-attval (fxml.sax:attribute-value a))))
                 (when (and (standalone-check-necessary-p adef)
                            (not (rod= (fxml.sax:attribute-value a) canon)))
                   (validity-error "(02) Standalone Document Declaration: attribute value not normalized"))
                 (setf (fxml.sax:attribute-value a) canon))))))
       (when *validate*		;maybe validate attribute values
         (dolist (a attlist)
           (validate-attribute ctx e a))))
      ((and *validate* attlist)
       (validity-error "(04) Attribute Value Type: no definition for element ~A"
                       (rod-string name)))))
  attlist)

(defun get-attribute (name attributes)
  (member name attributes :key #'fxml.sax:attribute-qname :test #'rod=))

(defun validate-attribute (ctx e a)
  (when (fxml.sax:attribute-specified-p a)   ;defaults checked by DEFINE-ATTRIBUTE
    (let* ((qname (fxml.sax:attribute-qname a))
           (adef
             (or (find-attribute e qname)
                 (validity-error "(04) Attribute Value Type: not declared: ~A"
                                 (rod-string qname)))))
      (validate-attribute* ctx adef (fxml.sax:attribute-value a)))))

(defstruct (entdef (:constructor)))

(defstruct (internal-entdef
            (:include entdef)
            (:constructor make-internal-entdef (value))
            (:conc-name #:entdef-))
  (value (error "missing argument") :type rod)
  (expansion nil)
  (external-subset-p *external-subset-p*))

(defstruct (external-entdef
            (:include entdef)
            (:constructor make-external-entdef (extid ndata))
            (:conc-name #:entdef-))
  (extid (error "missing argument") :type extid)
  (ndata nil :type (or rod null)))

(block-compile (:entry-points (validate-attribute*))
  (defun split-names (rod)
    (flet ((whitespacep (x)
             (or (rune= x #/U+0009)
                 (rune= x #/U+000A)
                 (rune= x #/U+000D)
                 (rune= x #/U+0020))))
      (if (let ((n (length rod)))
            (and (not (zerop n))
                 (or (whitespacep (rune rod 0))
                     (whitespacep (rune rod (1- n))))))
          nil
          (split-sequence-if #'whitespacep rod :remove-empty-subseqs t))))

  (defun validate-idref (ctx value)
    (unless (valid-name-p value)
      (validity-error "(11) IDREF: not a name: ~S" (rod-string value)))
    (unless (gethash value (id-table ctx))
      (setf (gethash value (id-table ctx)) nil)))

  (defun validate-nmtoken (value)
    (unless (valid-nmtoken-p value)
      (validity-error "(13) Name Token: not a NMTOKEN: ~S"
                      (rod-string value))))

  (defun validate-entity (value)
    (unless (valid-name-p value)
      (validity-error "(12) Entity Name: not a name: ~S" (rod-string value)))
    (let ((def (let ((*validate*
                       ;; Similarly the entity refs are internal and
                       ;; don't need normalization ... the unparsed
                       ;; entities (and entities) aren't "references"
                       ;;   -- sun/valid/sa03.xml
                       nil))
                 (get-entity-definition value :general (dtd *ctx*)
                                              :errorp nil))))
      (unless (and (typep def 'external-entdef) (entdef-ndata def))
        ;; unparsed entity
        (validity-error "(12) Entity Name: ~S" (rod-string value)))))

  (defun validate-attribute* (ctx adef value)
    (let ((type (attdef-type adef))
          (default (attdef-default adef)))
      (when (and (listp default)
                 (eq (car default) :FIXED)
                 (not (rod= value (cadr default))))
        (validity-error "(20) Fixed Attribute Default: expected ~S but got ~S"
                        (rod-string (cadr default))
                        (rod-string value)))
      (ecase (if (listp type) (car type) type)
        (:ID
         (unless (valid-name-p value)
           (validity-error "(08) ID: not a name: ~S" (rod-string value)))
         (when (eq (gethash value (id-table ctx)) t)
           (validity-error "(08) ID: ~S not unique" (rod-string value)))
         (setf (gethash value (id-table ctx)) t))
        (:IDREF
         (validate-idref ctx value))
        (:IDREFS
         (let ((names (split-names value)))
           (unless names
             (validity-error "(11) IDREF: malformed names"))
           (mapc (curry #'validate-idref ctx) names)))
        (:NMTOKEN
         (validate-nmtoken value))
        (:NMTOKENS
         (let ((tokens (split-names value)))
           (unless tokens
             (validity-error "(13) Name Token: malformed NMTOKENS"))
           (mapc #'validate-nmtoken tokens)))
        (:ENUMERATION
         (unless (member value (cdr type) :test #'rod=)
           (validity-error "(17) Enumeration: value not declared: ~S"
                           (rod-string value))))
        (:NOTATION
         (unless (member value (cdr type) :test #'rod=)
           (validity-error "(14) Notation Attributes: ~S" (rod-string value))))
        (:ENTITY
         (validate-entity value))
        (:ENTITIES
         (let ((names (split-names value)))
           (unless names
             (validity-error "(13) Name Token: malformed NMTOKENS"))
           (mapc #'validate-entity names)))
        (:CDATA)))))

(defun zstream-base-sysid (zstream)
  (let ((base-sysid
          (dolist (k (zstream-input-stack zstream))
            (let ((base-sysid (stream-name-uri (xstream-name k))))
              (when base-sysid (return base-sysid))))))
    base-sysid))

(defun absolute-uri (sysid source-stream)
  (let ((base-sysid (zstream-base-sysid source-stream)))
    ;; XXX is the IF correct?
    (if base-sysid
        (quri:merge-uris sysid base-sysid)
        sysid)))

(defstruct (extid (:constructor make-extid (public system)))
  (public nil :type (or rod null))
  (system (error "missing argument") :type (or quri:uri null)))

(setf (documentation 'extid 'type)
      "Represents an External ID, consisting of a Public ID and a System ID.

       @see-constructor{make-extiid}
       @see-slot{exitid-system}
       @see-slot{exitid-public}")

(setf (documentation #'make-extid 'function)
      "@arg[publicid]{string or nil}
       @arg[systemid]{@class{quri:uri} or nil}
       @return{an instance of @class{extid}}

       Create an object representing the External ID composed
       of the specified Public ID and System ID.")

(setf (documentation #'extid-public 'function)
      "@arg[extid]{A @class{extid}}
       @return[publicid]{string or nil}

       Returns the Public ID part of this External ID.")

(setf (documentation #'extid-system 'function)
      "@arg[extid]{A @class{extid}}
       @return[sytemid]{quri:uri or nil}

       Returns the System ID part of this External ID.")

(defun absolute-extid (source-stream extid)
  (let ((sysid (extid-system extid))
        (result (copy-extid extid)))
    (setf (extid-system result) (absolute-uri sysid source-stream))
    result))

(defstruct-read-only dtd
  (elements (%make-rod-hash-table))     ;elmdefs
  (gentities (%make-rod-hash-table))    ;general entities
  (pentities (%make-rod-hash-table))    ;parameter entities
  (notations (%make-rod-hash-table)))

(defun define-entity (source-stream name kind def)
  (setf name (intern-name name))
  (when (and fxml.sax:*namespace-processing* (find #/: name))
    (wf-error source-stream "colon in entity name"))
  (let ((table
          (ecase kind
            (:general (dtd-gentities (dtd *ctx*)))
            (:parameter (dtd-pentities (dtd *ctx*))))))
    (unless (gethash name table)
      (when (and source-stream (handler *ctx*))
        (report-entity (handler *ctx*) kind name def))
      (when (typep def 'external-entdef)
        (setf (entdef-extid def)
              (absolute-extid source-stream (entdef-extid def))))
      (setf (gethash name table)
            (cons *external-subset-p* def)))))

(defun get-entity-definition (entity-name kind dtd &key (errorp t))
  (unless dtd
    (undefined-entity nil entity-name))
  (destructuring-bind (extp &rest def)
      (gethash entity-name
               (ecase kind
                 (:general (dtd-gentities dtd))
                 (:parameter (dtd-pentities dtd)))
               '(nil))
    (when (and *validate* (standalone-p *ctx*) extp)
      (validity-error "(02) Standalone Document Declaration: entity reference: ~S"
                      (rod-string entity-name)))
    (or def
        (when errorp
          (restart-case
              (undefined-entity nil entity-name)
            (continue ()
              :report "Back up and include a literal ampersand."
              (make-internal-entdef
               (rod (format nil "&amp;~a"
                            (rod-string entity-name)))))
            (use-value (value)
              :report "Supply the expansion"
              (check-type value string)
              (make-internal-entdef value)))))))

(defun entity->xstream (zstream entity-name kind &optional internalp)
  ;; `zstream' is for error messages
  (let ((def (get-entity-definition entity-name kind (dtd *ctx*)))
        r)
    (etypecase def
      (internal-entdef
       (when (and (standalone-p *ctx*)
                  (entdef-external-subset-p def))
         (wf-error
          zstream
          "entity declared in external subset, but document is standalone"))
       (setf r (make-rod-xstream (entdef-value def)))
       (setf (xstream-name r)
             (make-stream-name :entity-name entity-name
                               :entity-kind kind
                               :uri nil)))
      (external-entdef
       (when internalp
         (wf-error zstream
                   "entity not internal: ~A" (rod-string entity-name)))
       (when (entdef-ndata def)
         (wf-error zstream
                   "reference to unparsed entity: ~A"
                   (rod-string entity-name)))
       (setf r (xstream-open-extid (extid-using-catalog (entdef-extid def))))
       (setf (stream-name-entity-name (xstream-name r)) entity-name
             (stream-name-entity-kind (xstream-name r)) kind)))
    r))

(defun checked-get-entdef (name type)
  (get-entity-definition name type (dtd *ctx*)))

(defun xstream-open-extid* (entity-resolver pubid sysid)
  (let* ((stream
           (or (funcall (or entity-resolver (constantly nil)) pubid sysid)
               (open (uri-to-pathname sysid)
                     :element-type '(unsigned-byte 8)
                     :direction :input))))
    (make-xstream stream
                  :name (make-stream-name :uri sysid)
                  :initial-speed 1)))

(defun xstream-open-extid (extid)
  (xstream-open-extid* (entity-resolver *ctx*)
                       (extid-public extid)
                       (extid-system extid)))

(defun call-with-entity-expansion-as-stream (zstream cont name kind internalp)
  ;; `zstream' is for error messages
  (let ((in (entity->xstream zstream name kind internalp)))
    (push (stream-name-uri (xstream-name in)) (base-stack *ctx*))
    (unwind-protect
         (funcall cont in)
      (pop (base-stack *ctx*))
      (close-xstream in))))

(defun ensure-dtd ()
  (unless (dtd *ctx*)
    (setf (dtd *ctx*) (make-dtd))
    (define-default-entities)))

(defun define-default-entities ()
  (define-entity nil #"lt"   :general (make-internal-entdef #"&#60;"))
  (define-entity nil #"gt"   :general (make-internal-entdef #">"))
  (define-entity nil #"amp"  :general (make-internal-entdef #"&#38;"))
  (define-entity nil #"apos" :general (make-internal-entdef #"'"))
  (define-entity nil #"quot" :general (make-internal-entdef #"\"")))

;; [*] in XML it is possible to define attributes before the element
;; itself is defined and since we hang attribute definitions into the
;; relevant element definitions, the `content' slot indicates whether an
;; element was actually defined.  It is NIL until set to a content model
;; when the element type declaration is processed.

(defun %make-rod-hash-table ()
  (make-hash-table :test 'equal))

(defun make-dtd-cache ()
  (make-hash-table :test 'equalp))

(defvar *cache-all-dtds* nil)
(defvar *dtd-cache* (make-dtd-cache))

(defun remdtd (uri dtd-cache)
  (remhash uri dtd-cache))

(defun clear-dtd-cache (dtd-cache)
  (clrhash dtd-cache))

(defun getdtd (uri dtd-cache)
  (gethash uri dtd-cache))

(defun (setf getdtd) (newval uri dtd-cache)
  (setf (gethash uri dtd-cache) newval))


;;;;

(defun find-element (name dtd)
  (gethash name (dtd-elements dtd)))

(defun define-element (dtd element-name &optional content-model)
  (let ((e (find-element element-name dtd)))
    (cond
      ((null e)
       (prog1
           (setf (gethash element-name (dtd-elements dtd))
                 (make-elmdef :name element-name :content content-model))
         (when content-model
           (fxml.sax:element-declaration (handler *ctx*) element-name content-model))))
      ((null content-model)
       e)
      (t
       (when *validate*
         (when (elmdef-content e)
           (validity-error "(05) Unique Element Type Declaration"))
         (when (eq content-model :EMPTY)
           (dolist (ad (elmdef-attributes e))
             (let ((type (attdef-type ad)))
               (when (and (listp type) (eq (car type) :NOTATION))
                 (validity-error "(16) No Notation on Empty Element: ~S"
                                 (rod-string element-name)))))))
       (fxml.sax:element-declaration (handler *ctx*) element-name content-model)
       (setf (elmdef-content e) content-model)
       (setf (elmdef-external-p e) *external-subset-p*)
       e))))

(defvar *redefinition-warning* nil)

(defun define-attribute (dtd element name type default)
  (let ((adef (make-attdef :element element
                           :name name
                           :type type
                           :default default))
        (e (or (find-element element dtd)
               (define-element dtd element))))
    (when (and *validate* (listp default))
      (unless (eq (attdef-type adef) :CDATA)
        (setf (second default) (canon-not-cdata-attval (second default))))
      (validate-attribute* *ctx* adef (second default)))
    (cond ((find-attribute e name)
           (when *redefinition-warning*
             (warn "Attribute \"~A\" of \"~A\" not redefined."
                   (rod-string name)
                   (rod-string element))))
          (t
           (when *validate*
             (when (eq type :ID)
               (when (find :ID (elmdef-attributes e) :key #'attdef-type)
                 (validity-error "(09) One ID per Element Type: element ~A"
                                 (rod-string element)))
               (unless (member default '(:REQUIRED :IMPLIED))
                 (validity-error "(10) ID Attribute Default: ~A"
                                 (rod-string element))))
             (flet ((notationp (type)
                      (and (listp type) (eq (car type) :NOTATION))))
               (when (notationp type)
                 (when (find-if #'notationp (elmdef-attributes e)
                                :key #'attdef-type)
                   (validity-error "(15) One Notation Per Element Type: ~S"
                                   (rod-string element)))
                 (when (eq (elmdef-content e) :EMPTY)
                   (validity-error "(16) No Notation on Empty Element: ~S"
                                   (rod-string element))))))
           (fxml.sax:attribute-declaration (handler *ctx*) element name type default)
           (push adef (elmdef-attributes e))))))

(defun find-attribute (elmdef name)
  (find name (elmdef-attributes elmdef) :key #'attdef-name :test #'rod=))

(defun define-notation (dtd name id)
  (let ((ns (dtd-notations dtd)))
    (when (gethash name ns)
      (validity-error "(24) Unique Notation Name: ~S" (rod-string name)))
    (setf (gethash name ns) id)))

(defun find-notation (name dtd)
  (gethash name (dtd-notations dtd)))

;;;; ---------------------------------------------------------------------------
;;;;  z streams and lexer
;;;;

(defun call-with-zstream (fn zstream)
  (unwind-protect
       (funcall fn zstream)
    (dolist (input (zstream-input-stack zstream))
      (when (xstream-p input)
        (close-xstream input)))))

(defmacro with-zstream ((zstream &rest args) &body body)
  (with-thunk (body zstream)
    `(call-with-zstream ,body (make-zstream ,@args))))

(defun read-token (input)
  (cond ((zstream-token-category input)
         (multiple-value-prog1
             (values (zstream-token-category input)
                     (zstream-token-semantic input))
           (setf (zstream-token-category input) nil
                 (zstream-token-semantic input) nil)))
        (t
         (read-token-2 input))))

(defun peek-token (input)
  (cond ((zstream-token-category input)
         (values
          (zstream-token-category input)
          (zstream-token-semantic input)))
        (t
         (multiple-value-bind (c s) (read-token input)
           (setf (zstream-token-category input) c
                 (zstream-token-semantic input) s))
         (values (zstream-token-category input)
                 (zstream-token-semantic input)))))

(defun read-token-2 (input)
  (cond ((null (zstream-input-stack input))
         (values :eof nil))
        (t
         (let ((c (peek-rune (car (zstream-input-stack input)))))
           (cond ((eq c :eof)
                  (cond ((eq (cadr (zstream-input-stack input)) :stop)
                         (values :eof nil))
                        (t
                         (close-xstream (pop (zstream-input-stack input)))
                         (if (null (zstream-input-stack input))
                             (values :eof nil)
                             (values :S nil) ;fake #x20 after PE expansion
                             ))))
                 (t
                  (read-token-3 input)))))))

(defvar *data-behaviour*)
(declaim (type (member :dtd :doc) *data-behaviour*))

(defun read-token-3 (zinput)
  (let ((input (car (zstream-input-stack zinput))))
    ;; PI Comment
    (let ((c (read-rune input)))
      (cond
        ;; first the common tokens
        ((rune= #/< c)
         (read-token-after-|<| zinput input))
        ;; now dispatch
        (t
         (ecase *data-behaviour*
           (:DTD
            (cond ((rune= #/\[ c) :\[)
                  ((rune= #/\] c) :\])
                  ((rune= #/\( c) :\()
                  ((rune= #/\) c) :\))
                  ((rune= #/\| c) :\|)
                  ((rune= #/\> c) :\>)
                  ((rune= #/\" c) :\")
                  ((rune= #/\' c) :\')
                  ((rune= #/\, c) :\,)
                  ((rune= #/\? c) :\?)
                  ((rune= #/\* c) :\*)
                  ((rune= #/\+ c) :\+)
                  ((name-rune-p c)
                   (unread-rune c input)
                   (values :nmtoken (read-name-token input)))
                  ((rune= #/# c)
                   (let ((q (read-name-token input)))
                     (cond ((rod= q '#.(string-rod "REQUIRED")) :|#REQUIRED|)
                           ((rod= q '#.(string-rod "IMPLIED")) :|#IMPLIED|)
                           ((rod= q '#.(string-rod "FIXED"))   :|#FIXED|)
                           ((rod= q '#.(string-rod "PCDATA"))  :|#PCDATA|)
                           (t
                            (wf-error zinput "Unknown token: ~S." q)))))
                  ((or (rune= c #/U+0020)
                       (rune= c #/U+0009)
                       (rune= c #/U+000D)
                       (rune= c #/U+000A))
                   (values :S nil))
                  ((rune= #/% c)
                   (cond ((name-start-rune-p (peek-rune input))
                          ;; an entity reference
                          (read-pe-reference zinput))
                         (t
                          (values :%))))
                  (t
                   (wf-error zinput "Unexpected character ~S." c))))
           (:DOC
            (cond
              ((rune= c #/&)
               (multiple-value-bind (kind data) (read-entity-like input)
                 (cond ((eq kind :ENTITY-REFERENCE)
                        (values :ENTITY-REF data))
                       ((eq kind :CHARACTER-REFERENCE)
                        (values :CDATA
                                (with-rune-collector (collect)
                                  (%put-unicode-char data collect)))))))
              (t
               (unread-rune c input)
               (values :CDATA (read-cdata input)))))))))))

(definline check-rune (input actual expected)
  (unless (eql actual expected)
    (wf-error input "expected #/~A but found #/~A"
              (rune-char expected)
              (rune-char actual))))

(defun read-pe-reference (zinput)
  (let* ((input (car (zstream-input-stack zinput)))
         (nam (read-name-token input)))
    (check-rune input #/\; (read-rune input))
    (cond (*expand-pe-p*
           ;; no external entities here!
           (let ((i2 (entity->xstream  zinput nam :parameter)))
             (zstream-push i2 zinput))
           (values :S nil) ;space before inserted PE expansion.
           )
          (t
           (values :PE-REFERENCE nam)) )))

(defun read-token-after-|<| (zinput input)
  (let ((d (read-rune input)))
    (cond ((eq d :eof)
           (eox input "EOF after '<'"))
          ((rune= #/! d)
           (read-token-after-|<!| input))
          ((rune= #/? d)
           (multiple-value-bind (target content) (read-pi input)
             (cond ((rod= target '#.(string-rod "xml"))
                    (values :xml-decl (cons target content)))
                   ((rod-equal target '#.(string-rod "XML"))
                    (wf-error zinput
                              "You lost -- no XML processing instructions."))
                   ((and fxml.sax:*namespace-processing* (position #/: target))
                    (wf-error zinput
                              "Processing instruction target ~S is not a ~
                               valid NcName."
                              (mu target)))
                   (t
                    (values :PI (cons target content))))))
          ((eq *data-behaviour* :DTD)
            (unread-rune d input)
            (unless (or (rune= #// d) (name-start-rune-p d))
              (wf-error zinput "Expected '!' or '?' after '<' in DTD."))
            (values :seen-< nil))
          ((rune= #// d)
           (let ((c (peek-rune input)))
             (cond ((name-start-rune-p c)
                    (read-tag-2 zinput input :etag))
                   (t
                    (wf-error zinput
                              "Expecting name start rune after \"</\".")))))
          ((name-start-rune-p d)
           (unread-rune d input)
           (read-tag-2 zinput input :stag))
          (t
           (wf-error zinput "Expected '!' or '?' after '<' in DTD.")))))

(defun read-token-after-|<!| (input)
  (let ((d (read-rune input)))
    (cond ((eq d :eof)
           (eox input "EOF after \"<!\"."))
          ((name-start-rune-p d)
           (unread-rune d input)
           (let ((name (read-name-token input)))
             (cond ((rod= name '#.(string-rod "ELEMENT")) :|<!ELEMENT|)
                   ((rod= name '#.(string-rod "ENTITY")) :|<!ENTITY|)
                   ((rod= name '#.(string-rod "ATTLIST")) :|<!ATTLIST|)
                   ((rod= name '#.(string-rod "NOTATION")) :|<!NOTATION|)
                   ((rod= name '#.(string-rod "DOCTYPE")) :|<!DOCTYPE|)
                   (t
                    (wf-error  input"`<!~A' unknown." (rod-string name))))))
          ((rune= #/\[ d)
           (values :|<![| nil))
          ((rune= #/- d)
           (setf d (read-rune input))
           (cond ((rune= #/- d)
                  (values
                   :COMMENT
                   (read-comment-content input)))
                 (t
                  (wf-error input"Bad character ~S after \"<!-\"" d))))
          (t
           (wf-error input "Bad character ~S after \"<!\"" d)))))

(definline read-S? (input)
  (while (member (peek-rune input) '(#/U+0020 #/U+0009 #/U+000A #/U+000D)
                 :test #'eql)
    (consume-rune input)))

(defun read-attribute-list (zinput input imagine-space-p)
  (cond ((or imagine-space-p
             (let ((c (peek-rune input)))
               (and (not (eq c :eof))
                    (space-rune-p c))))
         (read-S? input)
         (cond ((eq (peek-rune input) :eof)
                nil)
               ((name-start-rune-p (peek-rune input))
                (cons (read-attribute zinput input)
                      (read-attribute-list zinput input nil)))
               (t
                nil)))
        (t
         nil)))

(defun read-entity-like (input)
  "Read an entity reference off the xstream `input'. Returns two values:
   either :ENTITY-REFERENCE <interned-rod> in case of a named entity
   or     :CHARACTER-REFERENCE <integer> in case of character references.
   The initial #\\& is considered to be consumed already."
  (let ((c (peek-rune input)))
    (cond ((eq c :eof)
           (eox input "EOF after '&'"))
          ((rune= c #/#)
           (values :CHARACTER-REFERENCE (read-character-reference input)))
          (t
           (unless (name-start-rune-p (peek-rune input))
             (restart-case
                 (wf-error input "Expecting name after &.")
               (continue ()
                 :report "Treat the broken entity reference as a literal ampersand."
                 (return-from read-entity-like
                   (values :ENTITY-REFERENCE "amp")))))
           (let ((name (read-name-token input)))
             (setf c (read-rune input))
             (unless (rune= c #/\;)
               (restart-case
                   (wf-error input "Expected \";\".")
                 (continue ()
                   :report "Back up and include a literal ampersand."
                   (unread-rune c input)
                   (return-from read-entity-like
                     (values :non-reference name)))))
             (values :ENTITY-REFERENCE name))))))

(defun read-tag-2 (zinput input kind)
  (let ((name (read-name-token input))
        (atts nil))
    (setf atts (read-attribute-list zinput input nil))

    ;; check for double attributes
    (do ((q atts (cdr q)))
        ((null q))
      (cond ((find (caar q) (cdr q) :key #'car)
             (wf-error zinput "Attribute ~S has two definitions in element ~S."
                       (rod-string (caar q))
                       (rod-string name)))))

    (cond ((eq (peek-rune input) #/>)
           (consume-rune input)
           (values kind (cons name atts)))
          ((eq (peek-rune input) #//)
           (consume-rune input)
           (check-rune input #/> (read-rune input))
           (values :ztag (cons name atts)))
          (t
           (wf-error zinput "syntax error in read-tag-2.")) )))

(defun read-attribute (zinput input)
  (unless (name-start-rune-p (peek-rune input))
    (wf-error zinput "Expected name."))
  ;; arg thanks to the post mortem nature of name space declarations,
  ;; we could only process the attribute values post mortem.
  (let ((name (read-name-token input)))
    (while (let ((c (peek-rune input)))
             (and (not (eq c :eof))
                  (or (rune= c #/U+0020)
                      (rune= c #/U+0009)
                      (rune= c #/U+000A)
                      (rune= c #/U+000D))))
      (consume-rune input))
    (unless (eq (read-rune input) #/=)
      (wf-error zinput "Expected \"=\"."))
    (while (let ((c (peek-rune input)))
             (and (not (eq c :eof))
                  (or (rune= c #/U+0020)
                      (rune= c #/U+0009)
                      (rune= c #/U+000A)
                      (rune= c #/U+000D))))
      (consume-rune input))
    (cons name (read-att-value-2 input))))

(defun canon-not-cdata-attval (value)
  ;; | If the declared value is not CDATA, then the XML processor must
  ;; | further process the normalized attribute value by discarding any
  ;; | leading and trailing space (#x20) characters, and by replacing
  ;; | sequences of space (#x20) characters by a single space (#x20)
  ;; | character.
  (with-rune-collector (collect)
    (let ((gimme-20 nil)
          (anything-seen-p nil))
      (map nil (lambda (c)
                 (cond ((rune= c #/u+0020)
                        (setf gimme-20 t))
                       (t
                        (when (and anything-seen-p gimme-20)
                          (collect #/u+0020))
                        (setf gimme-20 nil)
                        (setf anything-seen-p t)
                        (collect c))))
           value))))

(definline data-rune-p (rune)
  ;; Any Unicode character, excluding FFFE, and FFFF.
  ;; Allow surrogates if using UTF-16, else allow >= 0x10000.
  (declare #.*fast*)
  (and rune
       (let ((c (rune-code rune)))
         (or (= c #x9) (= c #xA) (= c #xD)
             (<= #x20 c #xD7FF)
             (<= #xE000 c #xFFFD)
             (<= #x10000 c #x10FFFF)))))

(defun read-att-value (zinput input mode &optional canon-space-p (delim nil))
  (with-rune-collector-2 (collect collect-all)
    (labels ((muffle (input delim)
               (let (c)
                 (loop
                   (setf c (read-rune input))
                   (cond ((eql delim c)
                          (return))
                         ((eq c :eof)
                          (eox input "EOF"))
                         ((rune= c #/&)
                          (setf c (peek-rune input))
                          (cond ((eql c :eof)
                                 (eox input))
                                ((rune= c #/#)
                                 (let ((c (read-character-reference input)))
                                   (%put-unicode-char c collect)))
                                (t
                                 (unless (name-start-rune-p (peek-rune input))
                                   (wf-error zinput "Expecting name after &."))
                                 (let ((name (read-name-token input)))
                                   (setf c (read-rune input))
                                   (check-rune input c #/\;)
                                   (ecase mode
                                     (:ATT
                                      (recurse-on-entity
                                       zinput name :general
                                       (lambda (zinput)
                                         (muffle (car (zstream-input-stack zinput))
                                                 :eof))
                                       t))
                                     (:ENT
                                      ;; bypass, but never the less we
                                      ;; need to check for legal
                                      ;; syntax.
                                      ;; Must it be defined?
                                      ;; allerdings: unparsed sind verboten
                                      (collect #/&)
                                      (collect-all name)
                                      (collect #/\; )))))))
                         ((and (eq mode :ENT) (rune= c #/%))
                          (let ((d (peek-rune input)))
                            (when (eq d :eof)
                              (eox input))
                            (unless (name-start-rune-p d)
                              (wf-error zinput "Expecting name after %.")))
                          (let ((name (read-name-token input)))
                            (setf c (read-rune input))
                            (check-rune input c #/\;)
                            (cond (*expand-pe-p*
                                   (recurse-on-entity
                                    zinput name :parameter
                                    (lambda (zinput)
                                      (muffle (car (zstream-input-stack zinput))
                                              :eof))))
                                  (t
                                   (wf-error zinput "No PE here.")))))
                         ((and (eq mode :ATT) (rune= c #/<))
                           (wf-error zinput "unexpected #\/<"))
                         ((and canon-space-p (space-rune-p c))
                          (collect #/space))
                         ((not (data-rune-p c))
                          (wf-error zinput "illegal char: ~S." c))
                         (t
                          (collect c)))))))
      (declare (dynamic-extent #'muffle))
      (muffle input (or delim
                        (let ((delim (read-rune input)))
                          (unless (member delim '(#/\" #/\') :test #'eql)
                            (wf-error zinput "invalid attribute delimiter"))
                          delim))))))

(defun read-character-reference (input)
  ;; The #/& is already read
  (let ((res
         (let ((c (read-rune input)))
           (check-rune input c #/#)
           (setq c (read-rune input))
           (cond ((eql c :eof)
                  (eox input))
                 ((eql c #/x)
                  ;; hexadecimal
                  (setq c (read-rune input))
                  (when (eql c :eof)
                    (eox input))
                  (unless (digit-rune-p c 16)
                    (wf-error input "garbage in character reference"))
                  (prog1
                      (parse-integer
                       (with-output-to-string (sink)
                         (write-char (rune-char c) sink)
                         (while (progn
                                  (setq c (read-rune input))
                                  (when (eql c :eof)
                                    (eox input))
                                  (digit-rune-p c 16))
                           (write-char (rune-char c) sink)))
                       :radix 16)
                    (check-rune input c #/\;)))
                 ((rune<= #/0 c #/9)
                  ;; decimal
                  (prog1
                      (parse-integer
                       (with-output-to-string (sink)
                         (write-char (rune-char c) sink)
                         (while (progn
                                  (setq c (read-rune input))
                                  (when (eql c :eof)
                                    (eox input))
                                  (rune<= #/0 c #/9))
                           (write-char (rune-char c) sink)))
                       :radix 10)
                    (check-rune input c #/\;)))
                 (t
                  (wf-error input "Bad char in numeric character entity."))))))
    (unless (code-data-char-p res)
      (wf-error
       input
       "expansion of numeric character reference (#x~X) is no data char."
       res))
    res))

(defun read-pi (input)
  ;; "<?" is already read
  (let (name)
    (let ((c (peek-rune input)))
      (unless (name-start-rune-p c)
        (wf-error input "Expecting name after '<?'"))
      (setf name (read-name-token input)))
    (cond
      ((member (peek-rune input) '(#/U+0020 #/U+0009 #/U+000A #/U+000D)
               :test #'eql)
        (values name (read-pi-content input)))
      (t
        (unless (and (eql (read-rune input) #/?)
                     (eql (read-rune input) #/>))
          (wf-error input "malformed processing instruction"))
        (values name "")))))

(defun read-pi-content (input)
  (read-S? input)
  (let (d)
    (with-rune-collector (collect)
      (block nil
        (tagbody
         state-1
          (setf d (read-rune input))
          (when (eq d :eof)
            (eox input))
          (unless (data-rune-p d)
            (if d
                (wf-error input "Illegal char: ~S." d)
                (setf d #/U+FFFD)))
          (when (rune= d #/?) (go state-2))
          (collect d)
          (go state-1)
         state-2 ;; #/? seen
          (setf d (read-rune input))
          (when (eq d :eof)
            (eox input))
          (unless (data-rune-p d)
            (if d
                (wf-error input "Illegal char: ~S." d)
                (setf d #/U+FFFD)))
          (when (rune= d #/>) (return))
          (when (rune= d #/?)
            (collect #/?)
            (go state-2))
          (collect #/? d)
          (go state-1))))))

(defun read-comment-content (input &aux d)
  (with-rune-collector (collect)
    (block nil
      (tagbody
       state-1
        (setf d (read-rune input))
        (when (eq d :eof)
          (eox input))
        (unless (data-rune-p d)
          (wf-error input "Illegal char: ~S." d))
        (when (rune= d #/-) (go state-2))
        (collect d)
        (go state-1)
       state-2 ;; #/- seen
        (setf d (read-rune input))
        (when (eq d :eof)
          (eox input))
        (unless (data-rune-p d)
          (wf-error input "Illegal char: ~S." d))
        (when (rune= d #/-) (go state-3))
        (collect #/- d)
        (go state-1)
       state-3 ;; #/- #/- seen
        (setf d (read-rune input))
        (when (eq d :eof)
          (eox input))
        (unless (data-rune-p d)
          (wf-error input "Illegal char: ~S." d))
        (when (rune= d #/>) (return))
        (wf-error input "'--' not allowed in a comment")
        (when (rune= d #/-)
          (collect #/-)
          (go state-3))
         (collect #/- #/- d)
        (go state-1)))))

(defun read-cdata-sect (input)
  ;; <![CDATA[ is already read
  ;; read anything up to ]]>
  (with-rune-collector (collect)
    (block nil
      (tagbody
       state-1
         (let ((d (read-rune input)))
           (when (eq d :eof)
             (eox input))
           (unless (data-rune-p d)
             (restart-case
                 (wf-error input "Illegal char: ~S." d)
               (continue ()
                 :report "Skip character"
                 (go state-1))))
           (when (rune= d #/\]) (go state-2))
           (collect d)
           (go state-1))
       state-2 ;; #/] seen
         (let ((d (read-rune input)))
           (when (eq d :eof)
             (eox input))
           (unless (data-rune-p d)
             (wf-error input "Illegal char: ~S." d))
           (when (rune= d #/\]) (go state-3))
           (collect #/\] d)
           (go state-1))
       state-3 ;; #/\] #/\] seen
         (let ((d (read-rune input)))
           (when (eq d :eof)
             (eox input))
           (unless (data-rune-p d)
             (wf-error input "Illegal char: ~S." d))
           (when (rune= d #/>)
             (return))
           (when (rune= d #/\])
             (collect #/\])
             (go state-3))
           (collect #/\] #/\] d)
           (go state-1))))))

;; some character categories

(defun space-rune-p (rune)
  (declare (type rune rune))
  (or (rune= rune #/U+0020)
      (rune= rune #/U+0009)
      (rune= rune #/U+000A)
      (rune= rune #/U+000D)))

(defun code-data-char-p (c)
  ;; Any Unicode character, excluding FFFE, and FFFF.
  ;; Allow surrogates if using UTF-16, else allow >= 0x10000.
  (or (= c #x9) (= c #xA) (= c #xD)
      (<= #x20 c #xD7FF)
      (<= #xE000 c #xFFFD)
      (<= #x10000 c #x10FFFF)))

(defun pubid-char-p (c)
  (or (rune= c #/u+0020) (rune= c #/u+000D) (rune= c #/u+000A)
      (rune<= #/a c #/z)
      (rune<= #/A c #/Z)
      (rune<= #/0 c #/9)
      (member c '(#/- #/' #/\( #/\) #/+ #/, #/. #//
                  #/: #/= #/? #/\; #/! #/* #/#
                  #/@ #/$ #/_ #/%))))


(defun expect (input category)
  (multiple-value-bind (cat sem) (read-token input)
    (unless (eq cat category)
      (wf-error input "Expected ~S saw ~S [~S]" category cat sem))
    (values cat sem)))

(defun consume-token (input)
  (read-token input))

;;;; ---------------------------------------------------------------------------
;;;;  Parser
;;;;

(defun p/S (input)
  ;; S ::= (#x20 | #x9 | #xD | #xA)+
  (expect input :S)
  (while (eq (peek-token input) :S)
    (consume-token input)))

(defun p/S? (input)
  ;; S ::= (#x20 | #x9 | #xD | #xA)+
  (while (eq (peek-token input) :S)
    (consume-token input)))

(defun p/nmtoken (input)
  (nth-value 1 (expect input :nmtoken)))

(defun p/name (input)
  (let ((result (p/nmtoken input)))
    (unless (name-start-rune-p (elt result 0))
      (wf-error input "Expected name."))
    result))

(defun p/attlist-decl (input)
  ;; [52] AttlistDecl ::= '<!ATTLIST' S Name (S AttDef)* S? '>'
  (let (elm-name)
    (expect input :|<!ATTLIST|)
    (p/S input)
    (setf elm-name (p/nmtoken input))
    (loop
      (let ((tok (read-token input)))
        (case tok
          (:S
           (p/S? input)
           (cond ((eq (peek-token input) :>)
                  (consume-token input)
                  (return))
                 (t
                  (multiple-value-bind (name type default) (p/attdef input)
                    (define-attribute (dtd *ctx*) elm-name name type default)) )))
          (:>
           (return))
          (otherwise
           (wf-error input
                     "Expected either another AttDef or end of \"<!ATTLIST\". -- saw ~S."
                     tok)))))))

(defun p/attdef (input)
  ;; [53] AttDef ::= Name S AttType S DefaultDecl
  (let (name type default)
    (setf name (p/nmtoken input))
    (p/S input)
    (setf type (p/att-type input))
    (p/S input)
    (setf default (p/default-decl input))
    (values name type default)))

(defun p/list (input item-parser delimiter)
  ;; Parse something like S? <item> (S? <delimiter> <item>)* S?
  ;;
  (declare (type function item-parser))
  (let (res)
    (p/S? input)
    (setf res (list (funcall item-parser input)))
    (loop
      (p/S? input)
      (cond ((eq (peek-token input) delimiter)
             (consume-token input)
             (p/S? input)
             (push (funcall item-parser input) res))
            (t
             (return))))
    (p/S? input)
    (reverse res)))

(defun p/att-type (input)
  ;; [54] AttType ::= StringType | TokenizedType | EnumeratedType
  ;; [55] StringType ::= 'CDATA'
  ;; [56] TokenizedType ::= 'ID'                          /*VC: ID */
  ;;                                                        /*VC: One ID per Element Type */
  ;;                                                        /*VC: ID Attribute Default */
  ;;                          | 'IDREF'                     /*VC: IDREF */
  ;;                          | 'IDREFS'                    /*VC: IDREF */
  ;;                          | 'ENTITY'                    /*VC: Entity Name */
  ;;                          | 'ENTITIES'                  /*VC: Entity Name */
  ;;                          | 'NMTOKEN'                   /*VC: Name Token */
  ;;                          | 'NMTOKENS'                  /*VC: Name Token */
  ;; [57] EnumeratedType ::= NotationType | Enumeration
  ;; [58]   NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
  ;; /* VC: Notation Attributes */
  ;; [59] Enumeration ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')' /* VC: Enumeration */
  (multiple-value-bind (cat sem) (read-token input)
    (cond ((eq cat :nmtoken)
           (cond ((rod= sem '#.(string-rod "CDATA"))    :CDATA)
                 ((rod= sem '#.(string-rod "ID"))       :ID)
                 ((rod= sem '#.(string-rod "IDREF"))    :IDREFS)
                 ((rod= sem '#.(string-rod "IDREFS"))   :IDREFS)
                 ((rod= sem '#.(string-rod "ENTITY"))   :ENTITY)
                 ((rod= sem '#.(string-rod "ENTITIES")) :ENTITIES)
                 ((rod= sem '#.(string-rod "NMTOKEN"))  :NMTOKEN)
                 ((rod= sem '#.(string-rod "NMTOKENS")) :NMTOKENS)
                 ((rod= sem '#.(string-rod "NOTATION"))
                  (let (names)
                    (p/S input)
                    (expect input :\()
                    (setf names (p/list input #'p/nmtoken :\| ))
                    (expect input :\))
                    (when *validate*
                      (setf (referenced-notations *ctx*)
                            (append names (referenced-notations *ctx*))))
                    (cons :NOTATION names)))
                 (t
                  (wf-error input "In p/att-type: ~S ~S." cat sem))))
          ((eq cat :\()
           ;; XXX Die Nmtoken-Syntax pruefen wir derzeit nur beim Validieren.
           (let (names)
             ;;(expect input :\()
             (setf names (p/list input #'p/nmtoken :\| ))
             (expect input :\))
             (cons :ENUMERATION names)))
          (t
           (wf-error input "In p/att-type: ~S ~S." cat sem)) )))

(defun p/default-decl (input)
  ;; [60] DefaultDecl ::= '#REQUIRED' | '#IMPLIED'
  ;;                       | (('#FIXED' S)? AttValue) /* VC: Required Attribute */
  ;;
  ;; /* VC: Attribute Default Legal */
  ;; /* WFC: No < in Attribute Values */
  ;; /* VC: Fixed Attribute Default */
  (multiple-value-bind (cat sem) (peek-token input)
    (cond ((eq cat :|#REQUIRED|)
           (consume-token input) :REQUIRED)
          ((eq cat :|#IMPLIED|)
           (consume-token input) :IMPLIED)
          ((eq cat :|#FIXED|)
           (consume-token input)
           (p/S input)
           (list :FIXED (p/att-value input)))
          ((or (eq cat :\') (eq cat :\"))
           (list :DEFAULT (p/att-value input)))
          (t
           (wf-error input "p/default-decl: ~S ~S." cat sem)) )))
;;;;

;;  [70] EntityDecl ::= GEDecl | PEDecl
;;  [71]     GEDecl ::= '<!ENTITY' S Name S EntityDef S? '>'
;;  [72]     PEDecl ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
;;  [73]  EntityDef ::= EntityValue | (ExternalID NDataDecl?)
;;  [74]      PEDef ::= EntityValue | ExternalID
;;  [75] ExternalID ::= 'SYSTEM' S SystemLiteral
;;                      | 'PUBLIC' S PubidLiteral S SystemLiteral
;;  [76]  NDataDecl ::= S 'NDATA' S Name                /* VC: Notation Declared */

(defun p/entity-decl (input)
  (let (name def kind)
    (expect input :|<!ENTITY|)
    (p/S input)
    (cond ((eq (peek-token input) :%)
           (setf kind :parameter)
           (consume-token input)
           (p/S input))
          (t
           (setf kind :general)))
    (setf name (p/name input))
    (p/S input)
    (setf def (p/entity-def input kind))
    (if *forbid-entities*
        (with-simple-restart (continue "Skip the entity declaration")
          (error 'entities-forbidden
                 :name name
                 :value (typecase def
                          (internal-entdef (entdef-value def))
                          (t nil))))
        (define-entity input name kind def))
    (p/S? input)
    (expect input :\>)))

(defun report-entity (h kind name def)
  (etypecase def
    (external-entdef
      (let ((extid (entdef-extid def))
            (ndata (entdef-ndata def)))
        (if ndata
            (fxml.sax:unparsed-entity-declaration h
                                             name
                                             (extid-public extid)
                                             (uri-rod (extid-system extid))
                                             ndata)
            (fxml.sax:external-entity-declaration h
                                             kind
                                             name
                                             (extid-public extid)
                                             (uri-rod (extid-system extid))))))
    (internal-entdef
      (fxml.sax:internal-entity-declaration h kind name (entdef-value def)))))

(defun p/entity-def (input kind)
  (multiple-value-bind (cat sem) (peek-token input)
    (cond ((member cat '(:\" :\'))
           (make-internal-entdef (p/entity-value input)))
          ((and (eq cat :nmtoken)
                (or (rod= sem '#.(string-rod "SYSTEM"))
                    (rod= sem '#.(string-rod "PUBLIC"))))
           (let (extid ndata)
             (setf extid (p/external-id input nil))
             (when (eq kind :general)   ;NDATA allowed at all?
               (cond ((eq (peek-token input) :S)
                      (p/S? input)
                      (when (and (eq (peek-token input) :nmtoken)
                                 (rod= (nth-value 1 (peek-token input))
                                         '#.(string-rod "NDATA")))
                        (consume-token input)
                        (p/S input)
                        (setf ndata (p/nmtoken input))
                        (when *validate*
                          (push ndata (referenced-notations *ctx*)))))))
             (make-external-entdef extid ndata)))
          (t
           (wf-error input "p/entity-def: ~S / ~S." cat sem)) )))

(defun p/entity-value (input)
  (let ((delim (if (eq (read-token input) :\") #/\" #/\')))
    (read-att-value input
                    (car (zstream-input-stack input))
                    :ENT
                    nil
                    delim)))

(defun p/att-value (input)
  (let ((delim (if (eq (read-token input) :\") #/\" #/\')))
    (read-att-value input
                    (car (zstream-input-stack input))
                    :ATT
                    t
                    delim)))

(defun p/external-id (input &optional (public-only-ok-p nil))
  ;; xxx public-only-ok-p
  (multiple-value-bind (cat sem) (read-token input)
    (cond ((and (eq cat :nmtoken) (rod= sem '#.(string-rod "SYSTEM")))
           (p/S input)
           (make-extid nil (p/system-literal input)))
          ((and (eq cat :nmtoken) (rod= sem '#.(string-rod "PUBLIC")))
           (let (pub sys)
             (p/S input)
             (setf pub (p/pubid-literal input))
             (when (eq (peek-token input) :S)
               (p/S input)
               (when (member (peek-token input) '(:\" :\'))
                 (setf sys (p/system-literal input))))
             (when (and (not public-only-ok-p)
                        (null sys))
               (wf-error input "System identifier needed for this PUBLIC external identifier."))
             (make-extid pub sys)))
          (t
           (wf-error input "Expected external-id: ~S / ~S." cat sem)))))


;;  [11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
;;  [12]  PubidLiteral ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
;;  [13]     PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9]
;;                         | [-'()+,./:=?;!*#@$_%]

(defun p/id (input)
  (multiple-value-bind (cat) (read-token input)
    (cond ((member cat '(:\" :\'))
           (let ((delim (if (eq cat :\") #/\" #/\')))
             (with-rune-collector (collect)
               (loop
                 (let ((c (read-rune (car (zstream-input-stack input)))))
                   (cond ((eq c :eof)
                          (eox input "EOF in system literal."))
                         ((rune= c delim)
                          (return))
                         (t
                          (collect c))))))))
          (t
           (wf-error input "Expect either \" or \'.")))))

;; it is important to cache the orginal URI rod, since the re-serialized
;; uri-string can be different from the one parsed originally.
(defun uri-rod (uri)
  (if uri
      (let ((ctx *ctx*))
        (or (and ctx (gethash uri (original-rods ctx)))
            (rod (quri:render-uri uri nil))))
      nil))

(defun p/system-literal (input)
  (let* ((rod (p/id input))
         (uri (quri:uri (rod-string rod)))
         (ctx *ctx*))
    (when ctx
      (setf (gethash uri (original-rods ctx)) rod))
    uri))

(defun p/pubid-literal (input)
  (let ((result (p/id input)))
    (unless (every #'pubid-char-p result)
      (wf-error input "Illegal pubid: ~S." (rod-string result)))
    result))


;;;;

(defun p/element-decl (input)
  (let (name content)
    (expect input :|<!ELEMENT|)
    (p/S input)
    (setf name (p/nmtoken input))
    (p/S input)
    (setf content (normalize-mixed-cspec (p/cspec input)))
    (unless (legal-content-model-p content *validate*)
      (wf-error input "Malformed or invalid content model: ~S." (mu content)))
    (p/S? input)
    (expect input :\>)
    (define-element (dtd *ctx*) name content)
    (list :element name content)))

(defun maybe-compile-cspec (e)
  (or (elmdef-compiled-cspec e)
      (setf (elmdef-compiled-cspec e)
            (let ((cspec (elmdef-content e)))
              (unless cspec
                (validity-error "(03) Element Valid: no definition for ~A"
                                (rod-string (elmdef-name e))))
              (multiple-value-call #'cons
                (compile-cspec cspec (standalone-check-necessary-p e)))))))

(defun make-root-model (name)
  (cons (lambda (actual-name)
          (if (rod= actual-name name)
              (constantly :dummy)
              nil))
        (constantly t)))

;;; content spec validation:
;;;
;;; Given a `contentspec', COMPILE-CSPEC returns as multiple values two
;;; functions A and B of one argument to be called for every
;;;   A. child element
;;;   B. text child node
;;;
;;; Function A will be called with
;;;   - the element name rod as its argument.  If that element may appear
;;;     at the current position, a new function to be called for the next
;;;     child is returned.  Otherwise NIL is returned.
;;;   - argument NIL at the end of the element, it must then return T or NIL
;;;     to indicate whether the end tag is valid.
;;;
;;; Function B will be called with the character data rod as its argument, it
;;; returns a boolean indicating whether this text node is allowed.
;;;
;;; That is, if one of the functions ever returns NIL, the node is
;;; rejected as invalid.

(block-compile (:entry-points (compile-cspec legal-content-model-p))
  (defun cmodel-done (actual-value)
    (null actual-value))

  (defun compile-cspec (cspec &optional standalone-check)
    (cond
      ((atom cspec)
       (ecase cspec
         (:EMPTY (values #'cmodel-done (constantly nil)))
         (:PCDATA (values #'cmodel-done (constantly t)))
         (:ANY
          (values (labels ((doit (name) (if name #'doit t))) #'doit)
                  (constantly t)))))
      ((and (eq (car cspec) '*)
            (let ((subspec (second cspec)))
              (and (eq (car subspec) 'or) (eq (cadr subspec) :PCDATA))))
       (values (compile-mixed (second cspec))
               (constantly t)))
      (t
       (values (compile-content-model cspec)
               (lambda (rod)
                 (when standalone-check
                   (validity-error "(02) Standalone Document Declaration: whitespace"))
                 (every #'white-space-rune-p rod))))))

  (defun compile-mixed (cspec)
    ;; das koennten wir theoretisch auch COMPILE-CONTENT-MODEL erledigen lassen
    (let ((allowed-names (cddr cspec)))
      (labels ((doit (actual-name)
                 (cond
                   ((null actual-name) t)
                   ((member actual-name allowed-names :test #'rod=) #'doit)
                   (t nil))))
        #'doit)))

  (defun compile-content-model (cspec &optional (continuation #'cmodel-done))
    (if (vectorp cspec)
        (lambda (actual-name)
          (if (and actual-name (rod= cspec actual-name))
              continuation
              nil))
        (ecase (car cspec)
          (and
           (labels ((traverse (seq)
                      (compile-content-model (car seq)
                                             (if (cdr seq)
                                                 (traverse (cdr seq))
                                                 continuation))))
             (traverse (cdr cspec))))
          (or
           (let ((options (mapcar (rcurry #'compile-content-model continuation)
                                  (cdr cspec))))
             (lambda (actual-name)
               (some (rcurry #'funcall actual-name) options))))
          (?
           (let ((maybe (compile-content-model (second cspec) continuation)))
             (lambda (actual-name)
               (or (funcall maybe actual-name)
                   (funcall continuation actual-name)))))
          (*
           (let (maybe-continuation)
             (labels ((recurse (actual-name)
                        (if (null actual-name)
                            (funcall continuation actual-name)
                            (or (funcall maybe-continuation actual-name)
                                (funcall continuation actual-name)))))
               (setf maybe-continuation
                     (compile-content-model (second cspec) #'recurse))
               #'recurse)))
          (+
           (let ((it (cadr cspec)))
             (compile-content-model `(and ,it (* ,it)) continuation))))))

  (defun legal-content-model-p (cspec &optional validate)
    (or (eq cspec :PCDATA)
        (eq cspec :ANY)
        (eq cspec :EMPTY)
        (and (consp cspec)
             (eq (car cspec) '*)
             (consp (cadr cspec))
             (eq (car (cadr cspec)) 'or)
             (eq (cadr (cadr cspec)) :PCDATA)
             (every #'vectorp (cddr (cadr cspec)))
             (if (and validate (not (setp (cddr (cadr cspec)) :test #'rod=)))
                 (validity-error "VC: No Duplicate Types (07)")
                 t))
        (labels ((walk (x)
                   (cond ((member x '(:PCDATA :ANY :EMPTY))
                          nil)
                         ((atom x) t)
                         ((and (walk (car x))
                               (walk (cdr x)))))))
          (walk cspec)))))

;; wir fahren besser, wenn wir machen:

;; cspec ::= 'EMPTY' | 'ANY' | '#PCDATA'
;;         | Name
;;         | cs
;;    cs ::= '(' S? cspec ( S? '|' S? cspec)* S? ')' ('?' | '*' | '+')?
;; und eine post factum analyse

(defun p/cspec (input &optional recursivep)
  (let ((term
         (let ((names nil) op-cat op res stream)
           (multiple-value-bind (cat sem) (peek-token input)
             (cond ((eq cat :nmtoken)
                    (consume-token input)
                    (cond ((rod= sem '#.(string-rod "EMPTY"))
                           :EMPTY)
                          ((rod= sem '#.(string-rod "ANY"))
                           :ANY)
                          ((not recursivep)
                           (wf-error input "invalid content spec"))
                          (t
                           sem)))
                   ((eq cat :\#PCDATA)
                    (consume-token input)
                    :PCDATA)
                   ((eq cat :\()
                    (setf stream (car (zstream-input-stack input)))
                    (consume-token input)
                    (p/S? input)
                    (setq names (list (p/cspec input t)))
                    (p/S? input)
                    (cond ((member (peek-token input) '(:\| :\,))
                            (setf op-cat (peek-token input))
                            (setf op (if (eq op-cat :\,) 'and 'or))
                            (while (eq (peek-token input) op-cat)
                              (consume-token input)
                              (p/S? input)
                              (push (p/cspec input t) names)
                              (p/S? input))
                            (setf res (cons op (reverse names))))
                      (t
                        (setf res (cons 'and names))))
                    (p/S? input)
                    (expect input :\))
                    (when *validate*
                      (unless (eq stream (car (zstream-input-stack input)))
                        (validity-error "(06) Proper Group/PE Nesting")))
                    res)
                   (t
                    (wf-error input "p/cspec - ~s / ~s" cat sem)))))))
    (cond ((eq (peek-token input) :?) (consume-token input) (list '? term))
          ((eq (peek-token input) :+) (consume-token input) (list '+ term))
          ((eq (peek-token input) :*) (consume-token input) (list '* term))
          (t
           term))))

(defun normalize-mixed-cspec (cspec)
  ;; der Parser oben funktioniert huebsch fuer die children-Regel, aber
  ;; fuer Mixed ist das Ergebnis nicht praktisch, denn dort wollen wir
  ;; eigentlich auf eine Liste von Namen in einheitlichem Format hinaus.
  ;; Dazu normalisieren wir einfach in eine der beiden folgenden Formen:
  ;;   (* (or :PCDATA ...rods...))     -- und zwar exakt so!
  ;;   :PCDATA                         -- sonst ganz trivial
  (flet ((trivialp (c)
           (and (consp c)
                (and (eq (car c) 'and)
                     (eq (cadr c) :PCDATA)
                     (null (cddr c))))))
    (if (or (trivialp cspec)            ;(and PCDATA)
            (and (consp cspec)          ;(* (and PCDATA))
                 (and (eq (car cspec) '*)
                      (null (cddr cspec))
                      (trivialp (cadr cspec)))))
        :PCDATA
        cspec)))

(defun normalize-public-id (rod)
  (with-rune-collector (collect)
    (let ((gimme-20 nil)
          (anything-seen-p nil))
      (map nil (lambda (c)
                 (cond
                   ((or (rune= c #/u+0009)
                        (rune= c #/u+000A)
                        (rune= c #/u+000D)
                        (rune= c #/u+0020))
                    (setf gimme-20 t))
                   (t
                    (when (and anything-seen-p gimme-20)
                      (collect #/u+0020))
                    (setf gimme-20 nil)
                    (setf anything-seen-p t)
                    (collect c))))
           rod))))

;;;

(block-compile (:entry-points (p/ignore-sect p/ext-subset-decl))
  ;; [52] AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'


  ;; [52] AttlistDecl ::= '<!ATTLIST' S Name AttDefs S? '>'
  ;; [52] AttlistDecl ::= '<!ATTLIST' S Name S? '>'
  ;; [53] AttDefs ::= S Name S AttType S DefaultDecl AttDefs
  ;; [53] AttDefs ::=

  (defun p/conditional-sect (input)
    (expect input :<!\[ )
    (let ((stream (car (zstream-input-stack input))))
      (p/S? input)
      (multiple-value-bind (cat sem) (read-token input)
        (cond ((and (eq cat :nmtoken)
                    (rod= sem '#.(string-rod "INCLUDE")))
               (p/include-sect input stream))
              ((and (eq cat :nmtoken)
                    (rod= sem '#.(string-rod "IGNORE")))
               (p/ignore-sect input stream))
              (t
               (wf-error input "Expected INCLUDE or IGNORE after \"<![\"."))))))

  (defun p/cond-expect (input cat initial-stream)
    (expect input cat)
    (when *validate*
      (unless (eq (car (zstream-input-stack input)) initial-stream)
        (validity-error "(21) Proper Conditional Section/PE Nesting"))))

  (defun p/include-sect (input initial-stream)
    ;; <![INCLUDE is already read.
    (p/S? input)
    (p/cond-expect input :\[ initial-stream)
    (p/ext-subset-decl input)
    (p/cond-expect input :\] initial-stream)
    (p/cond-expect input :\] initial-stream)
    (p/cond-expect input :\> initial-stream))

  (defun p/ignore-sect (input initial-stream)
    ;; <![IGNORE is already read.
    ;; XXX Is VC 21 being checked for nested sections?
    (p/S? input)
    (p/cond-expect input :\[ initial-stream)
    (let ((input (car (zstream-input-stack input))))
      (let ((level 0))
        (do ((c1 (read-rune input) (read-rune input))
             (c2 #/U+0000 c1)
             (c3 #/U+0000 c2))
            ((= level -1))
          (declare (type fixnum level))
          (cond ((eq c1 :eof)
                 (eox input "EOF in <![IGNORE ... >")))
          (cond ((and (rune= c3 #/<) (rune= c2 #/!) (rune= c1 #/\[))
                 (incf level)))
          (cond ((and (rune= c3 #/\]) (rune= c2 #/\]) (rune= c1 #/>))
                 (decf level))) )))
    (unless (eq (car (zstream-input-stack input)) initial-stream)
      (validity-error "(21) Proper Conditional Section/PE Nesting")))

  (defun p/ext-subset-decl (input)
    ;; ( markupdecl | conditionalSect | S )*
    (loop
       (case (let ((*expand-pe-p* nil)) (peek-token input))
         (:|<![| (let ((*expand-pe-p* t)) (p/conditional-sect input)))
         (:S     (consume-token input))
         (:eof   (return))
         ((:|<!ELEMENT| :|<!ATTLIST| :|<!ENTITY| :|<!NOTATION| :PI :COMMENT)
          (let ((*expand-pe-p* t)
                (*external-subset-p* t))
            (p/markup-decl input)))
         ((:PE-REFERENCE)
          (let ((name (nth-value 1 (read-token input))))
            (recurse-on-entity input name :parameter
                               (lambda (input)
                                 (etypecase (checked-get-entdef name :parameter)
                                   (external-entdef
                                    (p/ext-subset input))
                                   (internal-entdef
                                    (p/ext-subset-decl input)))
                                 (unless (eq :eof (peek-token input))
                                   (wf-error input "Trailing garbage."))))))
         (otherwise (return)))) ))

(block-compile (:entry-points (p/markup-decl))
  (defun p/notation-decl (input)
    (let (name id)
      (expect input :|<!NOTATION|)
      (p/S input)
      (setf name (p/name input))
      (p/S input)
      (setf id (p/external-id input t))
      (p/S? input)
      (expect input :\>)
      (fxml.sax:notation-declaration (handler *ctx*)
                                     name
                                     (if (extid-public id)
                                         (normalize-public-id (extid-public id))
                                         nil)
                                     (uri-rod (extid-system id)))
      (when (and fxml.sax:*namespace-processing* (find #/: name))
        (wf-error input "colon in notation name"))
      (when *validate*
        (define-notation (dtd *ctx*) name id))
      (list :notation-decl name id)))

  (defun p/markup-decl-unsafe (input)
    ;; markupdecl ::= elementdecl | AttlistDecl       /* VC: Proper Declaration/PE Nesting */
    ;;              | EntityDecl | NotationDecl
    ;;              | PI | Comment               /* WFC: PEs in Internal Subset */
    (let ((token (peek-token input))
          (*expand-pe-p* (and *expand-pe-p* *external-subset-p*)))
      (case token
        (:|<!ELEMENT|  (p/element-decl input))
        (:|<!ATTLIST|  (p/attlist-decl input))
        (:|<!ENTITY|   (p/entity-decl input))
        (:|<!NOTATION| (p/notation-decl input))
        (:PI
         (let ((sem (nth-value 1 (read-token input))))
           (fxml.sax:processing-instruction (handler *ctx*) (car sem) (cdr sem))))
        (:COMMENT      (consume-token input))
        (otherwise
         (wf-error input "p/markup-decl ~S" (peek-token input))))))

  (defun p/markup-decl (input)
    (peek-token input)
    (let ((stream (car (zstream-input-stack input))))
      (multiple-value-prog1
          (p/markup-decl-unsafe input)
        (when *validate*
          (unless (eq stream (car (zstream-input-stack input)))
            (validity-error "(01) Proper Declaration/PE Nesting")))))))

(definline current-encoding ()
  "Return the current encoding of the input stream."
  (let ((xstream (car (zstream-input-stack (main-zstream *ctx*)))))
    (when xstream
      (xstream-encoding xstream))))

(defstruct xml-header
  version
  encoding
  (standalone-p nil))

(defun setup-encoding (input xml-header)
  (when (xml-header-encoding xml-header)
    (let ((enc (find-encoding (xml-header-encoding xml-header)))
          (xstream (car (zstream-input-stack input))))
      (cond ((eql enc :utf-8)
             (let ((old (xstream-encoding xstream)))
               (unless (eql old :utf-8)
                 (with-simple-restart (continue "Stick with ~a" old)
                   (wf-error input "Header says UTF-8, but BOM says ~a." old)))))
            (enc (setf (xstream-encoding xstream) enc))
            (t
             (warn "There is no such encoding: ~S." (xml-header-encoding xml-header)))))))

(definline set-full-speed (input)
  (let ((xstream (car (zstream-input-stack input))))
    (when xstream
      (set-to-full-speed xstream))))

(defun p/ext-subset (input)
  (cond ((eq (peek-token input) :xml-decl)
         (let ((hd (parse-text-decl (cdr (nth-value 1 (peek-token input))))))
           (setup-encoding input hd))
         (consume-token input)))
  (set-full-speed input)
  (p/ext-subset-decl input)
  (unless (eq (peek-token input) :eof)
    (wf-error input "Trailing garbage - ~S." (peek-token input))))

(defvar *catalog* nil)

(defun extid-using-catalog (extid)
  (if *catalog*
      (let ((sysid
             (resolve-extid (extid-public extid)
                            (extid-system extid)
                            *catalog*)))
        (if sysid
            (make-extid nil sysid)
            extid))
      extid))

(defun p/doctype-decl (input &key ((:dtd dtd-extid) nil) forbid-dtd)
  (let ()
    (let ((*expand-pe-p* nil)
          name extid)
      (expect input :|<!DOCTYPE|)
      (p/S input)
      (setq name (p/nmtoken input))
      (when *validate*
        (setf (model-stack *ctx*) (list (make-root-model name))))
      (when (eq (peek-token input) :S)
        (p/S input)
        (unless (or (eq (peek-token input) :\[ )
                    (eq (peek-token input) :\> ))
          (setf extid (p/external-id input t))))
      (when dtd-extid
        (setf extid dtd-extid))
      (p/S? input)

      (let ((pubid (and extid (extid-public extid)))
            (sysid (and extid (uri-rod (extid-system extid)))))
        (when forbid-dtd
          (with-simple-restart (continue "Parse the DTD")
            (error 'dtd-forbidden
                   :name name
                   :pubid pubid
                   :sysid sysid)))
        (fxml.sax:start-dtd (handler *ctx*) name pubid sysid))

      (when (eq (peek-token input) :\[ )
        (when (disallow-internal-subset *ctx*)
          (wf-error input "document includes an internal subset"))
        (ensure-dtd)
        (consume-token input)
        (fxml.sax:start-internal-subset (handler *ctx*))
        (while (progn (p/S? input)
                      (not (eq (peek-token input) :\] )))
          (if (eq (peek-token input) :PE-REFERENCE)
              (let ((name (nth-value 1 (read-token input))))
                (recurse-on-entity input name :parameter
                                   (lambda (input)
                                     (etypecase (checked-get-entdef name :parameter)
                                       (external-entdef
                                        (p/ext-subset input))
                                       (internal-entdef
                                        (p/ext-subset-decl input)))
                                     (unless (eq :eof (peek-token input))
                                       (wf-error input "Trailing garbage.")))))
              (let ((*expand-pe-p* t))
                (p/markup-decl input))))
        (consume-token input)
        (fxml.sax:end-internal-subset (handler *ctx*))
        (p/S? input))
      (expect input :>)
      (when extid
        (restart-case
            (let* ((effective-extid
                     (extid-using-catalog (absolute-extid input extid)))
                   (sysid (extid-system effective-extid))
                   (fresh-dtd-p (null (dtd *ctx*)))
                   (cached-dtd
                     (and fresh-dtd-p
                          (not (standalone-p *ctx*))
                          (getdtd sysid *dtd-cache*))))
              (cond
                (cached-dtd
                 (setf (dtd *ctx*) cached-dtd)
                 (report-cached-dtd cached-dtd))
                (t
                 (let ((xi2 (xstream-open-extid effective-extid)))
                   (with-zstream (zi2 :input-stack (list xi2))
                     (ensure-dtd)
                     (unless (ignore-errors (have-internal-subset (handler *ctx*)))
                       (fxml.sax:start-internal-subset (handler *ctx*)))
                     (p/ext-subset zi2)
                     (when (and fresh-dtd-p
                                *cache-all-dtds*
                                *validate*
                                (not (standalone-p *ctx*)))
                       (setf (getdtd sysid *dtd-cache*) (dtd *ctx*)))
                     (fxml.sax:end-internal-subset (handler *ctx*)))))))
          (continue ()
            :report "Use empty DTD"
            (setf (dtd *ctx*) (make-dtd)
                  (entity-resolver *ctx*) #'void-entity-resolver))))
      (fxml.sax:end-dtd (handler *ctx*))
      (let ((dtd (dtd *ctx*)))
        (fxml.sax:entity-resolver
         (handler *ctx*)
         (lambda (name handler) (resolve-entity name handler dtd)))
        (fxml.sax::dtd (handler *ctx*) dtd))
      (list :DOCTYPE name extid))))

(defun report-cached-dtd (dtd)
  (maphash (lambda (k v)
             (report-entity (handler *ctx*) :general k (cdr v)))
           (dtd-gentities dtd))
  (maphash (lambda (k v)
             (report-entity (handler *ctx*) :parameter k (cdr v)))
           (dtd-pentities dtd))
  (maphash (lambda (k v)
             (fxml.sax:notation-declaration
              (handler *ctx*)
              k
              (if (extid-public v)
                  (normalize-public-id (extid-public v))
                  nil)
              (uri-rod (extid-system v))))
           (dtd-notations dtd)))

(defun void-entity-resolver (pubid sysid)
  (declare (ignore pubid sysid))
  (flexi-streams:make-in-memory-input-stream nil))

(defun external-reference-forbidden (pubid sysid)
  (restart-case
      (error 'external-reference-forbidden
             :pubid pubid
             :sysid sysid)
    (continue ()
      :report "Ignore this reference."
      (void-entity-resolver pubid sysid))))

(block-compile (:entry-points (p/document))
  (defun p/misc*-2 (input)
    ;; Misc*
    (while (member (peek-token input) '(:COMMENT :PI :S))
      (case (peek-token input)
        (:COMMENT
          (fxml.sax:comment (handler *ctx*) (nth-value 1 (peek-token input))))
        (:PI
         (fxml.sax:processing-instruction
          (handler *ctx*)
          (car (nth-value 1 (peek-token input)))
          (cdr (nth-value 1 (peek-token input))))))
      (consume-token input)))

  (defun p/document
      (input handler
       &key validate dtd root entity-resolver disallow-internal-subset
            (recode t) ignore-dtd
            forbid-dtd
            (forbid-entities *forbid-entities*)
            (forbid-external t))
    ;; check types of user-supplied arguments for better error messages:
    (check-type validate boolean)
    (check-type recode boolean)
    (check-type dtd (or null extid))
    (check-type root (or null rod))
    (check-type entity-resolver (or null function symbol))
    (check-type disallow-internal-subset boolean)
    (check-type ignore-dtd boolean)
    (check-type forbid-dtd boolean)
    (check-type forbid-entities boolean)
    (check-type forbid-external boolean)
    (when ignore-dtd
      (setf entity-resolver #'void-entity-resolver))
    (when forbid-external
      (setf entity-resolver #'external-reference-forbidden))
    (let* ((xstream (car (zstream-input-stack input)))
           (name (xstream-name xstream))
           (base (when name (stream-name-uri name)))
           (*ctx*
             (make-context :handler handler
                           :main-zstream input
                           :entity-resolver entity-resolver
                           :base-stack (list (or base ""))
                           :disallow-internal-subset disallow-internal-subset))
           (*validate* validate)
           (*namespace-bindings* *initial-namespace-bindings*))
      (fxml.sax:register-sax-parser handler (make-instance 'fxml-parser :ctx *ctx*))
      (fxml.sax:with-document-events (handler)
        ;; document ::= XMLDecl? Misc* (doctypedecl Misc*)? element Misc*
        ;; Misc ::= Comment | PI |  S
        ;; xmldecl::='<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
        ;; sddecl::= S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
        (let ((*data-behaviour* :DTD))
          ;; optional XMLDecl?
          (p/xmldecl input)
          ;; Misc*
          (p/misc*-2 input)
          ;; (doctypedecl Misc*)?
          (cond
            ((eq (peek-token input) :<!DOCTYPE)
             (p/doctype-decl input :dtd dtd :forbid-dtd forbid-dtd)
             (p/misc*-2 input))
            (dtd
             (synthesize-doctype dtd input))
            ((and validate (not dtd))
             (validity-error "invalid document: no doctype")))
          (ensure-dtd)
          ;; Override expected root element if asked to
          (when root
            (setf (model-stack *ctx*) (list (make-root-model root))))
          ;; element
          (let ((*data-behaviour* :DOC))
            (fix-seen-< input)
            (p/element input))
          ;; optional Misc*
          (p/misc*-2 input)
          (p/eof input))))))

(defun synthesize-doctype (dtd input)
  (let ((dummy (string->xstream "<!DOCTYPE dummy>")))
    (setf (xstream-name dummy)
          (make-stream-name
           :entity-name "dummy doctype"
           :entity-kind :main
           :uri (zstream-base-sysid input)))
    (with-zstream (zstream :input-stack (list dummy))
      (p/doctype-decl zstream :dtd dtd))))

(defun fix-seen-< (input)
  (when (eq (peek-token input) :seen-<)
    (multiple-value-bind (c s)
        (read-token-after-|<| input (car (zstream-input-stack input)))
      (setf (zstream-token-category input) c
            (zstream-token-semantic input) s))))

(defun p/xmldecl (input)
  ;; we will use the attribute-value parser for the xml decl.
  (prog1
      (when (eq (peek-token input) :xml-decl)
        (let ((hd (parse-xml-decl (cdr (nth-value 1 (peek-token input))))))
          (setf (standalone-p *ctx*) (eq (xml-header-standalone-p hd) :yes))
          (setup-encoding input hd)
          (read-token input)
          hd))
    (set-full-speed input)))

(defun p/eof (input)
  (unless (eq (peek-token input) :eof)
    (with-simple-restart (continue "Ignore garbage")
      (wf-error input "Garbage at end of document.")))
  (when *validate*
    (maphash (lambda (k v)
               (unless v
                 (validity-error "(11) IDREF: ~S not defined" (rod-string k))))
             (id-table *ctx*))

    (dolist (name (referenced-notations *ctx*))
      (unless (find-notation name (dtd *ctx*))
        (validity-error "(23) Notation Declared: ~S" (rod-string name))))))

(defun p/element (input &aux (ctx *ctx*))
  (multiple-value-bind (cat n-b new-b uri lname qname attrs) (p/sztag input)
    (fxml.sax:start-element (handler ctx) uri lname qname attrs)
    (when (eq cat :stag)
      (let ((*namespace-bindings* n-b))
        (p/content input))
      (with-simple-restart (continue "Close the current tag")
        (p/etag input qname)))
    (fxml.sax:end-element (handler ctx) uri lname qname)
    (undeclare-namespaces new-b)
    (pop (base-stack ctx))
    (validate-end-element ctx qname)))

(block-compile (:entry-points (p/sztag))
  ;; copy&paste from cxml-rng
  (defun escape-uri (string)
    (with-output-to-string (out)
      (loop for c across (fxml::rod-to-utf8-string string) do
        (let ((code (char-code c)))
          ;; http://www.w3.org/TR/xlink/#link-locators
          (if (or (>= code 127) (<= code 32) (find c "<>\"{}|\\^`"))
              (format out "%~2,'0X" code)
              (write-char c out))))))

  (defun compute-base (attrs)
    (let ((new (fxml.sax:find-attribute #"xml:base" attrs))
          (current (car (base-stack *ctx*))))
      (if new
          (quri:merge-uris
           (quri:uri (escape-uri (fxml.sax:attribute-value new)))
           (quri:uri current))
          current)))

  (defun p/sztag (input &aux (ctx *ctx*))
    (multiple-value-bind (cat sem) (read-token input)
      (case cat
        ((:stag :ztag))
        (:eof (eox input))
        (t (restart-case
               (wf-error input "element expected")
             (continue ()
               :report "Skip garbage"
               (loop (setf (values cat sem) (read-token input))
                     (case cat
                       ((:stag :ztag) (return))
                       (:eof (eox input))))))))
      (destructuring-bind (&optional name &rest raw-attrs) sem
        (validate-start-element ctx name)
        (let* ((attrs
                 (process-attributes ctx name (build-attribute-list raw-attrs)))
               (*namespace-bindings* *namespace-bindings*)
               new-namespaces)
          (when fxml.sax:*namespace-processing*
            (setf new-namespaces (declare-namespaces attrs))
            (mapc #'set-attribute-namespace attrs))
          (push (compute-base attrs) (base-stack ctx))
          (multiple-value-bind (uri prefix local-name)
              (if fxml.sax:*namespace-processing*
                  (restart-case
                      (decode-qname name)
                    (store-value (uri)
                      :report "Provide a URI for the namespace."
                      (check-type uri string)
                      (let* ((prefix (split-qname name))
                             (uri (rod uri))
                             (decls (cons prefix uri)))
                        (push decls *namespace-bindings*)
                        (fxml.sax:start-prefix-mapping (handler *ctx*)
                                                       (car decls)
                                                       (cdr decls))
                        (let ((attr (fxml.sax:make-attribute :qname (rod (format nil "xmlns:~a" prefix))
                                                             :value uri
                                                             :specified-p t)))
                          (set-attribute-namespace attr)
                          (push attr attrs))
                        (decode-qname name))))
                  (values nil nil nil))
            (declare (ignore prefix))
            (check-attribute-uniqueness attrs)
            (unless (or fxml.sax:*include-xmlns-attributes*
                        (null fxml.sax:*namespace-processing*))
              (setf attrs
                    (remove-if (compose #'xmlns-attr-p #'fxml.sax:attribute-qname)
                               attrs)))
            (values cat
                    *namespace-bindings*
                    new-namespaces
                    uri local-name name attrs)))))))

(defun p/etag (input qname)
  (multiple-value-bind (cat2 sem2) (read-token input)
    (unless (and (eq cat2 :etag)
                 (eq (car sem2) qname))
      (wf-error input "Bad nesting. ~S / ~S"
                (mu qname)
                (mu (cons cat2 sem2))))
    (when (cdr sem2)
      (wf-error input "no attributes allowed in end tag"))))

(defun process-characters (input sem)
  (consume-token input)
  (when (search #"]]>" sem)
    (wf-error input "']]>' not allowed in CharData"))
  (validate-characters *ctx* sem))

(defun process-cdata-section (input)
  (consume-token input)
  (let ((input (car (zstream-input-stack input))))
    (unless (and (rune= #/C (read-rune input))
                 (rune= #/D (read-rune input))
                 (rune= #/A (read-rune input))
                 (rune= #/T (read-rune input))
                 (rune= #/A (read-rune input))
                 (rune= #/\[ (read-rune input)))
      (wf-error input "After '<![', 'CDATA[' is expected."))
    (validate-characters *ctx* #"hack")	;anything other than whitespace
    (read-cdata-sect input)))

(defun p/content (input)
  ;; [43] content ::= (element | CharData | Reference | CDSect | PI | Comment)*
  (loop
    (multiple-value-bind (cat sem) (peek-token input)
      (case cat
        ((:stag :ztag)
         (p/element input))
        ((:CDATA)
         (process-characters input sem)
         (fxml.sax:characters (handler *ctx*) sem))
        ((:ENTITY-REF)
         (let ((name sem))
           (consume-token input)
           (flet ((cont (input)
                    (prog1
                        (etypecase (checked-get-entdef name :general)
                          (internal-entdef (p/content input))
                          (external-entdef (p/ext-parsed-ent input)))
                      (unless (eq (peek-token input) :eof)
                        (wf-error input "Trailing garbage. - ~S"
                                  (peek-token input))))))
             (declare (dynamic-extent #'cont))
             (recurse-on-entity input name :general #'cont))))
        ((:<!\[)
         (let ((data (process-cdata-section input)))
           (fxml.sax:start-cdata (handler *ctx*))
           (fxml.sax:characters (handler *ctx*) data)
           (fxml.sax:end-cdata (handler *ctx*))))
        ((:PI)
         (consume-token input)
         (fxml.sax:processing-instruction (handler *ctx*) (car sem) (cdr sem)))
        ((:COMMENT)
         (consume-token input)
         (fxml.sax:comment (handler *ctx*) sem))
        (otherwise
         (return))))))

;; [78] extParsedEnt ::= TextDecl? contentw
;; [79]        extPE ::= TextDecl? extSubsetDecl

(defun p/ext-parsed-ent (input)
  ;; [78] extParsedEnt ::= '<?xml' VersionInfo? EncodingDecl S? '?>' content
  (when (eq (peek-token input) :xml-decl)
    (let ((hd (parse-text-decl (cdr (nth-value 1 (peek-token input))))))
      (setup-encoding input hd))
    (consume-token input))
  (set-full-speed input)
  (p/content input))

(defun parse-xml-decl (content)
  (let* ((res (make-xml-header))
         (i (make-rod-xstream content)))
    (with-zstream (z :input-stack (list i))
      (let ((atts (read-attribute-list z i t)))
        (unless (eq (peek-rune i) :eof)
          (wf-error i "Garbage at end of XMLDecl."))
        ;; versioninfo muss da sein
        ;; dann ? encodingdecl
        ;; dann ? sddecl
        ;; dann ende
        (unless (eq (caar atts) (intern-name '#.(string-rod "version")))
          (wf-error i "XMLDecl needs version."))
        (unless (and (>= (length (cdar atts)) 1)
                     (every (lambda (x)
                              (or (rune<= #/a x #/z)
                                  (rune<= #/A x #/Z)
                                  (rune<= #/0 x #/9)
                                  (rune= x #/_)
                                  (rune= x #/.)
                                  (rune= x #/:)
                                  (rune= x #/-)))
                            (cdar atts)))
          (wf-error i"Bad XML version number: ~S." (rod-string (cdar atts))))
        (setf (xml-header-version res) (rod-string (cdar atts)))
        (pop atts)
        (when (eq (caar atts) (intern-name '#.(string-rod "encoding")))
          (unless (and (>= (length (cdar atts)) 1)
                       (every (lambda (x)
                                (or (rune<= #/a x #/z)
                                    (rune<= #/A x #/Z)
                                    (rune<= #/0 x #/9)
                                    (rune= x #/_)
                                    (rune= x #/.)
                                    (rune= x #/-)))
                              (cdar atts))
                       ((lambda (x)
                          (or (rune<= #/a x #/z)
                              (rune<= #/A x #/Z)))
                        (aref (cdar atts) 0)))
            (wf-error i "Bad XML encoding name: ~S." (rod-string (cdar atts))))
          (setf (xml-header-encoding res) (rod-string (cdar atts)))
          (pop atts))
        (when (eq (caar atts) (intern-name '#.(string-rod "standalone")))
          (unless (or (rod= (cdar atts) '#.(string-rod "yes"))
                      (rod= (cdar atts) '#.(string-rod "no")))
            (wf-error i "XMLDecl's 'standalone' attribute must be exactly \"yes\" or \"no\" and not ~S."
                      (rod-string (cdar atts))))
          (setf (xml-header-standalone-p res)
                (if (rod-equal '#.(string-rod "yes") (cdar atts))
                    :yes
                    :no))
          (pop atts))
        (when atts
          (wf-error i "Garbage in XMLDecl: ~A" (rod-string content)))
        res))))

(defun parse-text-decl (content)
  (let* ((res (make-xml-header))
         (i (make-rod-xstream content)))
    (with-zstream (z :input-stack (list i))
      (let ((atts (read-attribute-list z i t)))
        (unless (eq (peek-rune i) :eof)
          (wf-error i "Garbage at end of TextDecl"))
        ;; versioninfo optional
        ;; encodingdecl muss da sein
        ;; dann ende
        (when (eq (caar atts) (intern-name '#.(string-rod "version")))
          (unless (and (>= (length (cdar atts)) 1)
                       (every (lambda (x)
                                (or (rune<= #/a x #/z)
                                    (rune<= #/A x #/Z)
                                    (rune<= #/0 x #/9)
                                    (rune= x #/_)
                                    (rune= x #/.)
                                    (rune= x #/:)
                                    (rune= x #/-)))
                              (cdar atts)))
            (wf-error i "Bad XML version number: ~S." (rod-string (cdar atts))))
          (setf (xml-header-version res) (rod-string (cdar atts)))
          (pop atts))
        (unless (eq (caar atts) (intern-name '#.(string-rod "encoding")))
          (wf-error i "TextDecl needs encoding."))
        (unless (and (>= (length (cdar atts)) 1)
                     (every (lambda (x)
                              (or (rune<= #/a x #/z)
                                  (rune<= #/A x #/Z)
                                  (rune<= #/0 x #/9)
                                  (rune= x #/_)
                                  (rune= x #/.)
                                  (rune= x #/-)))
                            (cdar atts))
                     ((lambda (x)
                        (or (rune<= #/a x #/z)
                            (rune<= #/A x #/Z)
                            (rune<= #/0 x #/9)))
                      (aref (cdar atts) 0)))
          (wf-error i "Bad XML encoding name: ~S." (rod-string (cdar atts))))
        (setf (xml-header-encoding res) (rod-string (cdar atts)))
        (pop atts)
        (when atts
          (wf-error i "Garbage in TextDecl: ~A" (rod-string content)))))
    res))

;;;; ---------------------------------------------------------------------------
;;;;  mu
;;;;

(defun mu (x)
  (cond ((stringp x) x)
        ((vectorp x) (rod-string x))
        ((consp x)
         (cons (mu (car x)) (mu (cdr x))))
        (x)))

;;;; ---------------------------------------------------------------------------
;;;; User interface ;;;;

(defun specific-or (component &optional (alternative nil))
  (if (eq component :unspecific)
      alternative
      component))

(defun string-or (str &optional (alternative nil))
  (if (zerop (length str))
      alternative
      str))

(defun pathname-to-uri (pathname)
  (quri:uri (format nil "file://~a" (namestring pathname))))

(defun parse-name.type (str)
  (if str
      (let ((i (position #\. str :from-end t)))
        (if i
            (values (subseq str 0 i) (subseq str (1+ i)))
            (values str nil)))
      (values nil nil)))

(defun uri-to-pathname (uri)
  (let ((uri (if (quri:uri-p uri)
                 uri
                 (quri:uri uri))))
    (let ((scheme (quri:uri-scheme uri)))
      (unless (or (null scheme) (string-equal scheme "file"))
        (error 'xml-parse-error
               :format-control "URI scheme ~S not supported"
               :format-arguments (list scheme))))
    (quri:uri-file-pathname uri)))

(defun parse
    (input handler &rest args
     &key validate dtd root entity-resolver disallow-internal-subset
          recode pathname ignore-dtd
          forbid-dtd
          (forbid-entities t)
          (forbid-external t))
  "@arg[input]{A string, pathname, octet vector, or stream.}
   @arg[handler]{A @class{SAX handler}}
   @arg[validate]{Boolean.  Defaults to @code{nil}.  If true, parse in
     validating mode, i.e. assert that the document contains a DOCTYPE
     declaration and conforms to the DTD declared.}
   @arg[dtd]{unless @code{nil}, an extid instance specifying the external
     subset to load.  This options overrides the extid specified in the
     document type declaration, if any.  See below for @fun{make-extid}.
     This option is useful for verification purposes together with the
     @var{root} and @var{disallow-internal-subset} arguments.}
   @arg[root]{The expected root element name, or @code{nil} (the default).
     If specified, this argument overrides the name stated in the input's
     DOCTYPE (if any).}
   @arg[entity-resolver]{@code{nil} or a function of two arguments which
     is invoked for every entity referenced by the document with the
     entity's Public ID (a rod) and System ID (an URI object) as arguments.
     The function may either return nil, FXML will then try to resolve the
     entity as usual. Alternatively it may return a Common Lisp stream
     specialized on @code{(unsigned-byte 8)} which will be used instead.
     (It may also signal an error, of course, which can be useful to prohibit
     parsed XML documents from including arbitrary files readable by
     the parser.)}
   @arg[disallow-internal-subset]{Boolean.  If true, signal
     an error if the document contains an internal subset.}
   @arg[recode]{Boolean.  (Ignored on Lisps with Unicode
     support.)  Recode rods to UTF-8 strings.  Defaults to true.
     Make sure to use @fun{fxml.utf8-dom:make-dom-builder} if this
     option is enabled and @fun{fxml.rune-dom:make-dom-builder}
     otherwise.}
   @return{The value returned by @fun{fxml.sax:end-document} on @var{handler}.}

   Parse an XML document from @var{input}, which can be a string, pathname,
   octet vector, or stream.

   Return values from this function depend on the SAX handler used.
   This is an old-style convenience wrapper around the new-style interface
   @fun{parse}.

   Parse an XML document from @var{filename}, and signal SAX events to
   @var{handler} while doing so.

   All SAX parsing functions share the same keyword arguments.  Refer to
   @fun{parse} for details on keyword arguments."
  (declare (ignore validate dtd root entity-resolver disallow-internal-subset
                   recode ignore-dtd forbid-dtd forbid-external))
  (let ((*forbid-entities* forbid-entities)
        (args (alexandria:remove-from-plist args :pathname)))
    (etypecase input
      (xstream  (apply #'parse-xstream input handler args))
      (pathname (apply #'parse-file input handler args))
      (rod      (apply #'parse-rod input handler args))
      (array    (apply #'parse-octets input handler args))
      (stream
       (let ((xstream (make-xstream input :speed 8192)))
         (setf (xstream-name xstream)
               (make-stream-name
                :entity-name "main document"
                :entity-kind :main
                :uri (if pathname
                         (pathname-to-uri (merge-pathnames pathname))
                         (safe-stream-sysid input))))
         (apply #'parse-xstream xstream handler args))))))

(defun parse-xstream (xstream handler &rest args)
  (let ((*ctx* nil))
    (handler-case
        (with-zstream (zstream :input-stack (list xstream))
          (peek-rune xstream)
          (with-scratch-pads ()
            (apply #'p/document zstream handler args)))
      (fxml.runes-encoding:encoding-error (c)
        (wf-error xstream "~A" c)))))

(defun parse-file (filename handler &rest args)
  "@arg[filename]{An pathname designator.}
   @arg[handler]{A @class{SAX handler}}
   @return{The value returned by @fun{fxml.sax:end-document} on @var{handler}.}

   This is an old-style convenience wrapper around the new-style interface
   @fun{parse}.

   Parse an XML document from @var{filename}, and signal SAX events to
   @var{handler} while doing so.

   All SAX parsing functions share the same keyword arguments.  Refer to
   @fun{parse} for details on keyword arguments."
  (with-open-xfile (input filename)
    (setf (xstream-name input)
      (make-stream-name
       :entity-name "main document"
       :entity-kind :main
       :uri (pathname-to-uri (merge-pathnames filename))))
    (apply #'parse-xstream input handler args)))

(defun resolve-synonym-stream (stream)
  (while (typep stream 'synonym-stream)
    (setf stream (symbol-value (synonym-stream-symbol stream))))
  stream)

(defun safe-stream-sysid (stream)
  (if (and (typep (resolve-synonym-stream stream) 'file-stream)
           ;; ignore-errors, because sb-bsd-sockets creates instances of
           ;; FILE-STREAMs that aren't
           (ignore-errors (pathname stream)))
      (pathname-to-uri (merge-pathnames (pathname stream)))
      nil))

(deftype |SAX HANDLER| ()
  'fxml.sax:abstract-handler
  "Historically, any object has been usable as a SAX handler with FXML,
   as long as it implemented all SAX events, i.e. had methods
   for the generic functions defined in the SAX package.

   While this approach still works, it is now recommended that SAX handlers
   should be implemented by subclassing @class{abstract-handler} or one
   of its subclasses.  Useful subclasses are @class{content-handler}
   and @class{default-handler}.

   (In addition, the value @code{nil} is valid SAX handler, which discards
   all events it receives.)

   As a rule of thumb, write a subclass of @class{default-handler} if
   you want to handle only a few special SAX events and ignore the rest,
   because this class has no-op default methods for all events.

   If, however, you want to make certain that your class implements all
   important SAX events explicitly, a good choice is @class{content-handler},
   which has no-op default methods only for less important, DTD-related
   events, and requires subclasses to implement all events related to the
   content model.

   In some cases, it might be helpful to implement @class{abstract-handler}
   directly, which has no default event methods at all.")

(defun parse-stream (stream handler &rest args)
  "@arg[stream]{An (unsigned-byte 8) stream}
   @arg[handler]{A @class{SAX handler}}
   @return{The value returned by @fun{fxml.sax:end-document} on @var{handler}.}

   This is an old-style convenience wrapper around the new-style interface
   @fun{parse}.

   Parse an XML document from @var{stream}, and signal SAX events to
   @var{handler} while doing so.

   All SAX parsing functions share the same keyword arguments.  Refer to
   @fun{parse} for details on keyword arguments."
  (let ((xstream
         (make-xstream
          stream
          :name (make-stream-name
                 :entity-name "main document"
                 :entity-kind :main
                 :uri (safe-stream-sysid stream))
          :initial-speed 1)))
    (apply #'parse-xstream xstream handler args)))

(defun parse-empty-document
    (uri qname handler &key public-id system-id entity-resolver (recode t))
  "@arg[uri]{a string or nil}
   @arg[qname]{a string or nil}
   @arg[handler]{a @class{SAX handler}}
   @arg[public-id]{a string or nil}
   @arg[system-id]{a @type{quri:uri} or nil}
   @arg[entity-resolver]{@code{nil} or a function of two arguments which
     is invoked for every entity referenced by the document with the
     entity's Public ID (a rod) and System ID (an URI object) as arguments.
     The function may either return nil, FXML will then try to resolve the
     entity as usual. Alternatively it may return a Common Lisp stream
     specialized on @code{(unsigned-byte 8)} which will be used instead.
     (It may also signal an error, of course, which can be useful to prohibit
     parsed XML documents from including arbitrary files readable by
     the parser.)}
   @arg[recode]{Boolean.  (Ignored on Lisps with Unicode
     support.)  Recode rods to UTF-8 strings.  Defaults to true.
     Make sure to use @fun{fxml.utf8-dom:make-dom-builder} if this
     option is enabled and @fun{fxml.rune-dom:make-dom-builder}
     otherwise.}
   @return{The value returned by @fun{fxml.sax:end-document} on @var{handler}.}

   Simulate parsing of a document with a document element @var{qname}
   having no attributes except for an optional namespace
   declaration to @var{uri}.  If an external ID is specified
   (@var{system-id}, @var{public-id}), find, parse, and report
   this DTD as if with @fun{parse-file}, using the specified
   entity resolver."
  (check-type uri (or null rod))
  (check-type qname (or null rod))
  (check-type public-id (or null rod))
  (check-type system-id (or null quri:uri))
  (check-type entity-resolver (or null function symbol))
  (check-type recode boolean)
  (let ((*ctx*
          (make-context :handler handler :entity-resolver entity-resolver))
        (*validate* nil)
        (extid
          (when (or public-id system-id)
            (extid-using-catalog (make-extid public-id system-id)))))
    (fxml.sax:with-document-events (handler)
      (when extid
        (fxml.sax:start-dtd handler
                            qname
                            (and public-id)
                            (and system-id (uri-rod system-id)))
        (setf (dtd *ctx*) (getdtd (extid-system extid) *dtd-cache*))
        (unless (dtd *ctx*)
          (with-scratch-pads ()
            (let ((*data-behaviour* :DTD))
              (let ((xi2 (xstream-open-extid extid)))
                (with-zstream (zi2 :input-stack (list xi2))
                  (ensure-dtd)
                  (p/ext-subset zi2))))))
        (fxml.sax:end-dtd handler)
        (let ((dtd (dtd *ctx*)))
          (fxml.sax:entity-resolver handler (lambda (n h) (resolve-entity n h dtd)))
          (fxml.sax::dtd handler dtd)))
      (ensure-dtd)
      (when (or uri qname)
        (let* ((attrs
                 (when uri
                   (list (fxml.sax:make-attribute :qname #"xmlns"
                                                  :value (rod uri)
                                                  :specified-p t))))
               (*namespace-bindings* *namespace-bindings*)
               new-namespaces)
          (when fxml.sax:*namespace-processing*
            (setf new-namespaces (declare-namespaces attrs))
            (mapc #'set-attribute-namespace attrs))
          (multiple-value-bind (uri prefix local-name)
              (if fxml.sax:*namespace-processing* (decode-qname qname) nil)
            (declare (ignore prefix))
            (unless (or fxml.sax:*include-xmlns-attributes*
                        (null fxml.sax:*namespace-processing*))
              (setf attrs nil))
            (fxml.sax:start-element (handler *ctx*) uri local-name qname attrs)
            (fxml.sax:end-element (handler *ctx*) uri local-name qname))
          (undeclare-namespaces new-namespaces))))))

(defun parse-dtd-file (filename &optional handler)
  "@arg[filename]{An pathname designator.}
   @arg[handler]{A @class{SAX handler}}
   @return{A @class{dtd} instance.}

   Parse @a[http://www.w3.org/TR/2000/REC-xml-20001006#NT-extSubset]{declarations}
   from @var{filename} and return an object representing the DTD,
   suitable as an argument to @code{validate} with @fun{parse}."
  (with-open-file (s filename :element-type '(unsigned-byte 8))
    (parse-dtd-stream s handler)))

(defun parse-dtd-stream (stream &optional handler)
  "@arg[stream]{An (unsigned-byte 8) stream.}
   @arg[handler]{A @class{SAX handler}}
   @return{A @class{dtd} instance.}

   Parse @a[http://www.w3.org/TR/2000/REC-xml-20001006#NT-extSubset]{declarations}
   from @var{stream} and return an object representing the DTD,
   suitable as an argument to @code{validate} with @fun{parse}."
  (let ((input (make-xstream stream)))
    (setf (xstream-name input)
          (make-stream-name
           :entity-name "dtd"
           :entity-kind :main
           :uri (safe-stream-sysid stream)))
    (let ((*ctx* (make-context :handler handler))
          (*validate* t)
          (*data-behaviour* :DTD))
      (with-zstream (zstream :input-stack (list input))
        (with-scratch-pads ()
          (ensure-dtd)
          (peek-rune input)
          (p/ext-subset zstream)
          (dtd *ctx*))))))

(defun parse-rod (string handler &rest args)
  "@arg[string]{An string of unicode characters.}
   @arg[handler]{A @class{SAX handler}}
   @return{The value returned by @fun{fxml.sax:end-document} on @var{handler}.}

   This is an old-style convenience wrapper around the new-style interface
   @fun{parse}.

   Parse an XML document from @var{string}, and signal SAX events to
   @var{handler} while doing so.

   Note: This function assumes that @var{string} has already been decoded into
   Unicode runes and ignores the encoding specified in the XML declaration,
   if any.

   All SAX parsing functions share the same keyword arguments.  Refer to
   @fun{parse} for details on keyword arguments."
  (let ((xstream (string->xstream string)))
    (setf (xstream-name xstream)
          (make-stream-name
           :entity-name "main document"
           :entity-kind :main
           :uri nil))
    (apply #'parse-xstream xstream handler args)))

(defun string->xstream (string)
  (make-rod-xstream (string-rod string)))

(defun parse-octets (octets handler &rest args)
  "@arg[octets]{An (unsigned-byte 8) vector.}
   @arg[handler]{A @class{SAX handler}}
   @return{The value returned by @fun{fxml.sax:end-document} on @var{handler}.}

   This is an old-style convenience wrapper around the new-style interface
   @fun{parse}.

   Parse an XML document from @var{octets}, and signal SAX events to
   @var{handler} while doing so.

   All SAX parsing functions share the same keyword arguments.  Refer to
   @fun{parse} for details on keyword arguments."
  (apply #'parse-stream (make-octet-input-stream octets) handler args))

;;;;

(defun zstream-push (new-xstream zstream)
  (cond ((find-if (lambda (x)
                    (and (xstream-p x)
                         (eql (stream-name-entity-name (xstream-name x))
                              (stream-name-entity-name (xstream-name new-xstream)))
                         (eql (stream-name-entity-kind (xstream-name x))
                              (stream-name-entity-kind (xstream-name new-xstream)))))
                  (zstream-input-stack zstream))
         (wf-error zstream "Infinite recursion.")))
  (push new-xstream (zstream-input-stack zstream))
  zstream)

(defun recurse-on-entity (zstream name kind continuation &optional internalp)
  (assert (not (zstream-token-category zstream)))
  (flet ((rec (new-xstream)
           (push :stop (zstream-input-stack zstream))
           (zstream-push new-xstream zstream)
           (prog1
               (funcall continuation zstream)
             (assert (eq (peek-token zstream) :eof))
             (assert (eq (pop (zstream-input-stack zstream)) new-xstream))
             (close-xstream new-xstream)
             (assert (eq (pop (zstream-input-stack zstream)) :stop))
             (setf (zstream-token-category zstream) nil)
             '(consume-token zstream))))
    (declare (dynamic-extent #'rec))
    (with-simple-restart (continue "Skip entity")
      (call-with-entity-expansion-as-stream
       zstream
       #'rec
       name
       kind
       internalp))))

#||
(defmacro read-data-until* ((predicate input res res-start res-end) &body body)
  ;; fast variant -- for now disabled for no apparent reason
  ;; -> res, res-start, res-end
  `(let* ((rptr (xstream-read-ptr ,input))
          (p0   rptr)
          (fptr (xstream-fill-ptr ,input))
          (buf  (xstream-buffer ,input))
          ,res ,res-start ,res-end)
     (declare (type fixnum rptr fptr p0)
              (type (simple-array read-element (*)) buf))
     (loop
       (cond ((%= rptr fptr)
              ;; underflow -- hmm inject the scratch-pad with what we
              ;; read and continue, while using read-rune and collecting
              ;; d.h. besser waere hier auch while-reading zu benutzen.
              (setf (xstream-read-ptr ,input) rptr)
              (multiple-value-setq (,res ,res-start ,res-end)
                (with-rune-collector/raw (collect)
                  (do ((i p0 (%+ i 1)))
                      ((%= i rptr))
                    (collect (%rune buf i)))
                  (let (c)
                    (loop
                      (cond ((%= rptr fptr)
                             (setf (xstream-read-ptr ,input) rptr)
                             (setf c (peek-rune input))
                             (cond ((eq c :eof)
                                    (return)))
                             (setf rptr (xstream-read-ptr ,input)
                                   fptr (xstream-fill-ptr ,input)
                                   buf  (xstream-buffer ,input)))
                            (t
                             (setf c (%rune buf rptr))))
                      (cond ((,predicate c)
                             ;; we stop
                             (setf (xstream-read-ptr ,input) rptr)
                             (return))
                            (t
                             ;; we continue
                             (collect c)
                             (setf rptr (%+ rptr 1))) )))))
              (return))
             ((,predicate (%rune buf rptr))
              ;; we stop
              (setf (xstream-read-ptr ,input) rptr)
              (setf ,res buf ,res-start p0 ,res-end rptr)
              (return) )
             (t
              we continue
              (sf rptr (%+ rptr 1))) ))
     ,@body ))
||#

(defmacro read-data-until* ((predicate input res res-start res-end) &body body)
  "Read data from `input' until `predicate' applied to the read char
   turns true. Then execute `body' with `res', `res-start', `res-end'
   bound to denote a subsequence (of RUNEs) containing the read portion.
   The rune upon which `predicate' turned true is neither consumed from
   the stream, nor included in `res'.

   Keep the predicate short, this it may be included more than once into
   the macro's expansion."
  ;;
  (let ((input-var (gensym))
        (collect (gensym))
        (c (gensym)))
    ;; The optimization declarations are necessary to prevent run-time
    ;; typep in Clozure.
    `(locally (declare (optimize (safety 1) (debug 0)))
       (let ((,input-var ,input))
         (multiple-value-bind (,res ,res-start ,res-end)
             (with-rune-collector/raw (,collect)
               (loop
                  (let ((,c (peek-rune ,input-var)))
                    (cond ((eq ,c :eof)
                           ;; xxx error message
                           (return))
                          ((funcall ,predicate ,c)
                           (return))
                          (t
                           (,collect ,c)
                           (consume-rune ,input-var))))))
           (locally
               ,@body))))))

(defun read-name-token (input)
  (read-data-until* ((lambda (rune)
                       (declare (type rune rune))
                       (not (name-rune-p rune)))
                     input
                     r rs re)
    (intern-name r rs re)))

(defun read-cdata (input)
  (read-data-until* ((lambda (rune)
                       (declare (type rune rune))
                       (when (and (%rune< rune #/U+0020)
                                  (not (or (%rune= rune #/U+0009)
                                           (%rune= rune #/U+000a)
                                           (%rune= rune #/U+000d))))
                         (wf-error input "code point invalid: ~A" rune))
                       (or (%rune= rune #/<) (%rune= rune #/&)))
                     input
                     source start end)
    (locally
        (declare (type (simple-array rune (*)) source)
                 (type ufixnum start)
                 (type ufixnum end)
                 (optimize (speed 3) (safety 0)))
      (let ((res (make-array (%- end start) :element-type 'rune)))
        (declare (type (simple-array rune (*)) res))
        (let ((i (%- end start)))
          (declare (type ufixnum i))
          (loop
             (setf i (- i 1))
             (setf (%rune res i) (%rune source (the ufixnum (+ i start))))
             (when (= i 0)
               (return))))
        res))))

(block-compile (:entry-points (resolve-entity read-att-value-2))
  ;; used only by read-att-value-2
  (defun internal-entity-expansion (name)
    (let ((def (get-entity-definition name :general (dtd *ctx*))))
      (unless (typep def 'internal-entdef)
        (wf-error nil "Entity '~A' is not an internal entity." name))
      (or (entdef-expansion def)
          (setf (entdef-expansion def) (find-internal-entity-expansion name)))))

  ;; used only by read-att-value-2
  (defun find-internal-entity-expansion (name)
    (with-zstream (zinput)
      (with-rune-collector-3 (collect)
        (labels ((muffle (input)
                   (let (c)
                     (loop
                        (setf c (read-rune input))
                        (cond ((eq c :eof)
                               (return))
                              ((rune= c #/&)
                               (setf c (peek-rune input))
                               (cond ((eql c :eof)
                                      (eox input))
                                     ((rune= c #/#)
                                      (let ((c (read-character-reference input)))
                                        (%put-unicode-char c collect)))
                                     (t
                                      (unless (name-start-rune-p c)
                                        (wf-error zinput "Expecting name after &."))
                                      (let ((name (read-name-token input)))
                                        (setf c (read-rune input))
                                        (check-rune input c #/\;)
                                        (recurse-on-entity
                                         zinput name :general
                                         (lambda (zinput)
                                           (muffle (car (zstream-input-stack zinput)))))))))
                              ((rune= c #/<)
                               (wf-error zinput "unexpected #\/<"))
                              ((space-rune-p c)
                               (collect #/space))
                              ((not (data-rune-p c))
                               (wf-error zinput "illegal char: ~S." c))
                              (t
                               (collect c)))))))
          (declare (dynamic-extent #'muffle))
          (recurse-on-entity
           zinput name :general
           (lambda (zinput)
             (muffle (car (zstream-input-stack zinput)))))))))

  (defun resolve-entity (name handler dtd)
    (let ((*validate* nil))
      (if (get-entity-definition name :general dtd
                                      :errorp nil)
          (let* ((*ctx* (make-context :handler handler :dtd dtd))
                 (*data-behaviour* :DOC))
            (with-zstream (input)
              (with-scratch-pads ()
                (recurse-on-entity
                 input name :general
                 (lambda (input)
                   (prog1
                       (etypecase (checked-get-entdef name :general)
                         (internal-entdef (p/content input))
                         (external-entdef (p/ext-parsed-ent input)))
                     (unless (eq (peek-token input) :eof)
                       (wf-error input "Trailing garbage. - ~S"
                                 (peek-token input)))))))))
          nil)))

  (defun read-att-value-2 (input)
    (let ((delim (read-rune input)))
      (when (eql delim :eof)
        (eox input))
      (unless (member delim '(#/\" #/\') :test #'eql)
        (wf-error input
                  "Bad attribute value delimiter ~S, must be either #\\\" or #\\\'."
                  (rune-char delim)))
      (with-rune-collector-4 (collect collect-all)
        (loop
           (let ((c (read-rune input)))
             (cond ((eq c :eof)
                    (eox input "EOF"))
                   ((rune= c delim)
                    (return))
                   ((rune= c #/<)
                    (wf-error input "'<' not allowed in attribute values"))
                   ((rune= #/& c)
                    (multiple-value-bind (kind sem) (read-entity-like input)
                      (ecase kind
                        (:CHARACTER-REFERENCE
                         (%put-unicode-char sem collect))
                        (:ENTITY-REFERENCE
                         (let* ((exp (internal-entity-expansion sem))
                                (n (length exp)))
                           (declare (type (simple-array rune (*)) exp))
                           (do ((i 0 (%+ i 1)))
                               ((%= i n))
                             (collect (%rune exp i)))))
                        (:non-reference
                         (collect #\&)
                         (collect-all sem)))))
                   ((space-rune-p c)
                    (collect #/u+0020))
                   (t
                    (collect c)))))))))

;;;;;;;;;;;;;;;;;

;;; Namespace stuff

;; We already know that name is part of a valid XML name, so all we
;; have to check is that the first rune is a name-start-rune and that
;; there is not colon in it.
(defun nc-name-p (name)
  (and (plusp (length name))
       (name-start-rune-p (rune name 0))
       (notany #'(lambda (rune) (rune= #/: rune)) name)))

(declaim (ftype (function (string) (values (or string null) string)) split-qname))
(defun split-qname (qname)
  (declare (type fxml.runes:simple-rod qname)
           (optimize speed (safety 1) (debug 0)))
  (let ((pos (position  #/: qname)))
    (if pos
        (let ((prefix (subseq qname 0 pos))
              (local-name (subseq qname (1+ pos))))
          (when (zerop pos)
            (wf-error nil "empty namespace prefix"))
          (if (nc-name-p local-name)
              (values prefix local-name)
              (wf-error nil "~S is not a valid NcName."
                        (rod-string local-name))))
        (values () qname))))

(defun decode-qname (qname)
  "decode-qname name => namespace-uri, prefix, local-name"
  (declare (type fxml.runes:simple-rod qname))
  (multiple-value-bind (prefix local-name) (split-qname qname)
    (let ((uri (find-namespace-binding prefix)))
      (if uri
          (values uri prefix local-name)
          (values nil nil qname)))))


(defun find-namespace-binding (prefix)
  (cdr (or (assoc (or prefix #"") *namespace-bindings* :test #'rod=)
           (undeclared-namespace prefix))))

;; FIXME: Should probably be refactored by adding :start and :end to rod=/rod-equal
(block-compile (:entry-points (xmlns-attr-p))
  (defun rod-starts-with (prefix rod)
    (and (<= (length prefix) (length rod))
         (dotimes (i (length prefix) t)
           (unless (rune= (rune prefix i) (rune rod i))
             (return nil)))))

  (defun xmlns-attr-p (attr-name)
    (rod-starts-with #.(string-rod "xmlns") attr-name)))

(block-compile (:entry-points (find-namespace-declarations))
  (defun attrname->prefix (attrname)
    (if (< 5 (length attrname))
        (subseq attrname 6)
        nil))

  (defun find-namespace-declarations (attributes)
    (loop
      for attribute in attributes
      for qname = (fxml.sax:attribute-qname attribute)
      when (xmlns-attr-p qname)
        collect (cons (attrname->prefix qname) (fxml.sax:attribute-value attribute)))))

(defun declare-namespaces (attributes)
  (let ((ns-decls (find-namespace-declarations attributes)))
    (dolist (ns-decl ns-decls)
      (with-simple-restart (continue "Ignore namespace declaration")
        ;; check some namespace validity constraints
        (let ((prefix (car ns-decl))
              (uri (cdr ns-decl)))
          (cond
            ((and (rod= prefix #"xml")
                  (not (rod= uri #"http://www.w3.org/XML/1998/namespace")))
             (wf-error nil
                       "Attempt to rebind the prefix \"xml\" to ~S." (mu uri)))
            ((and (rod= uri #"http://www.w3.org/XML/1998/namespace")
                  (not (rod= prefix #"xml")))
             (wf-error nil
                       "The namespace ~
                      URI \"http://www.w3.org/XML/1998/namespace\" may not ~
                      be bound to the prefix ~S, only \"xml\" is legal."
                       (mu prefix)))
            ((and (rod= prefix #"xmlns")
                  (rod= uri #"http://www.w3.org/2000/xmlns/"))
             (wf-error nil
                       "Attempt to bind the prefix \"xmlns\" to its predefined ~
                      URI \"http://www.w3.org/2000/xmlns/\", which is ~
                      forbidden for no good reason."))
            ((rod= prefix #"xmlns")
             (wf-error nil
                       "Attempt to bind the prefix \"xmlns\" to the URI ~S, ~
                      but it may not be declared." (mu uri)))
            ((rod= uri #"http://www.w3.org/2000/xmlns/")
             (wf-error nil
                       "The namespace URI \"http://www.w3.org/2000/xmlns/\" may ~
                      not be bound to prefix ~S (or any other)." (mu prefix)))
            ((and (rod= uri #"") prefix)
             (wf-error nil
                       "Only the default namespace (the one without a prefix) ~
                      may be bound to an empty namespace URI, thus ~
                      undeclaring it."))
            (t
             (push (cons prefix (if (rod= #"" uri) nil uri))
                   *namespace-bindings*)
             (fxml.sax:start-prefix-mapping (handler *ctx*)
                                            (car ns-decl)
                                            (cdr ns-decl)))))))
    ns-decls))

(defun undeclare-namespaces (ns-decls)
  (dolist (ns-decl ns-decls)
    (fxml.sax:end-prefix-mapping (handler *ctx*) (car ns-decl))))

(defun build-attribute-list (attr-alist)
  ;; fixme: if there is a reason this function reverses attribute order,
  ;; it should be documented.
  (loop for (qname . value) in attr-alist
        collect (fxml.sax:make-attribute :qname qname
                                         :value value
                                         :specified-p t)))

(defun check-attribute-uniqueness (attributes)
  ;; 5.3 Uniqueness of Attributes
  ;; In XML documents conforming to [the xmlns] specification, no
  ;; tag may contain two attributes which:
  ;; 1. have identical names, or
  ;; 2. have qualified names with the same local part and with
  ;; prefixes which have been bound to namespace names that are
  ;; identical.
  ;;
  ;; 1. is checked by read-tag-2, so we only deal with 2 here
  (loop for (attr-1 . rest) on attributes do
    (when (and (fxml.sax:attribute-namespace-uri attr-1)
               (find-if (lambda (attr-2)
                          (and (rod= (fxml.sax:attribute-namespace-uri attr-1)
                                     (fxml.sax:attribute-namespace-uri attr-2))
                               (rod= (fxml.sax:attribute-local-name attr-1)
                                     (fxml.sax:attribute-local-name attr-2))))
                        rest))
      (wf-error nil
                "Multiple definitions of attribute ~S in namespace ~S."
                (mu (fxml.sax:attribute-local-name attr-1))
                (mu (fxml.sax:attribute-namespace-uri attr-1))))))

(defun set-attribute-namespace (attribute)
  (let ((qname (fxml.sax:attribute-qname attribute)))
    (if (and fxml.sax:*use-xmlns-namespace* (rod= qname #"xmlns"))
        (setf (fxml.sax:attribute-namespace-uri attribute)
              #"http://www.w3.org/2000/xmlns/")
        (multiple-value-bind (prefix local-name) (split-qname qname)
          (when (and prefix ;; default namespace doesn't apply to attributes
                     (or (not (rod= #"xmlns" prefix))
                         fxml.sax:*use-xmlns-namespace*))
            (setf (fxml.sax:attribute-namespace-uri attribute)
                  (decode-qname qname)))
          (setf (fxml.sax:attribute-local-name attribute) local-name)))))

;;;;;;;;;;;;;;;;;

;; System Identifier Protocol

;; A system identifier is an object obeying to the system identifier
;; protocol. Often something like an URL or a pathname.

;; OPEN-SYS-ID sys-id                                   [generic function]
;;
;; Opens the resource associated with the system identifier `sys-id'
;; for reading and returns a stream. For now it is expected, that the
;; stream is an octet stream (one of element type (unsigned-byte 8)).
;;
;; More precisely: The returned object only has to obey to the xstream
;; controller protocol. (That is it has to provide implementations for
;; READ-OCTETS and XSTREAM-CONTROLLER-CLOSE).

;; MERGE-SYS-ID sys-id base                             [generic function]
;;
;; Merges two system identifiers. That is resolve `sys-id' relative to
;; `base' yielding an absolute system identifier suitable for
;; OPEN-SYS-ID.


;;;;;;;;;;;;;;;;;
;;; SAX validation handler

(defclass validator ()
  ((context :initarg :context :accessor context)
   (cdatap :initform nil :accessor cdatap)))

(defun make-validator (dtd root)
  "@arg[dtd]{An @class{dtd} instance.}
   @arg[root]{Element name, a string.}
   @return{A @class{SAX handler}.}

   Create a SAX handler which validates against a DTD instance.
   The document's root element must be named @code{root}.
   Used with @fun{dom:map-document}, this validates a document
   object as if by re-reading it with a validating parser, except
   that declarations recorded in the document instance are completely
   ignored.

   Example:

   @pre{(let ((d (parse-file \"~/test.xml\" (fxml-dom:make-dom-builder)))
      (x (parse-dtd-file \"~/test.dtd\")))
  (fxml.dom:map-document (fxml:make-validator x #\"foo\") d))}"
  (make-instance 'validator
    :context (make-context
              :handler nil
              :dtd dtd
              :model-stack (list (make-root-model root)))))

(macrolet ((with-context ((validator) &body body)
             `(let ((*ctx* (context ,validator))
                    (*validate* t))
                (with-scratch-pads ()   ;nicht schoen
                  ,@body))))
  (defmethod fxml.sax:start-element ((handler validator) uri lname qname attributes)
    uri lname
    (with-context (handler)
      (validate-start-element *ctx* qname)
      (process-attributes *ctx* qname attributes)))

  (defmethod fxml.sax:start-cdata ((handler validator))
    (setf (cdatap handler) t))

  (defmethod fxml.sax:characters ((handler validator) data)
    (with-context (handler)
      (validate-characters *ctx* (if (cdatap handler) #"hack" data))))

  (defmethod fxml.sax:end-cdata ((handler validator))
    (setf (cdatap handler) nil))

  (defmethod fxml.sax:end-element ((handler validator) uri lname qname)
    uri lname
    (with-context (handler)
      (validate-end-element *ctx* qname))))
