;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XML; readtable: glisp; Encoding: utf-8; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: A prototype XML parser
;;;   Created: 1999-07-17
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  © copyright 1999 by Gilbert Baumann

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
;; buffered stream with and advertised buffer layout, so that we
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
;;    :xml-pi (<target> . <content>)    ;processing-instruction starting with "<?xml"
;;    :pi (<target> . <content>)        ;processing-instruction
;;    :stag (<name> . <atts>)           ;start tag
;;    :etag (<name> . <atts>)           ;end tag
;;    :ztag (<name> . <atts>)           ;empty tag
;;    :<!element
;;    :<!entity
;;    :<!attlist
;;    :<!notation
;;    :<!doctype
;;    :<![
;;    :comment <content>

;; *data-behaviour* = :DTD
;;
;;    :name <interned-rod>
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




;;; NOTES
;;
;; Stream buffers as well as RODs are supposed to be encoded in
;; UTF-16. 

;; where does the time go?
;; DATA-RUNE-P
;; CANON-NOT-CDATA-ATTVAL
;; READ-ATTVAL (MUFFLE)
;; CLOSy DOM
;; UTF-8 decoder (13%)
;; READ-ATTVAL (10%)
;; 

;;; TODO
;;
;; o Improve error messages:
;;    - line and column number counters
;;    - better texts
;;    - better handling of errors (no crash'n burn behaviour)
;;
;; o provide for a faster DOM
;;
;; o parse document should get passed a document instance, so that a user
;;   could pass his own DOM implementation
;;
;; o morph zstream into a context object and thus also get rid of
;;   special variables. Put the current DTD there too.

;; o the *scratch-pad* hack should become something much more
;;   reentrant, we could either define a system-wide resource
;;   or allocate some scratch-pads per context.

;; o only parse the DTD on an option

;; o make the invalid tests pass.
;;
;; o CR handling in utf-16 deocders
;;
;; o UCS-4 reader
;;
;; o max depth together with circle detection
;;   (or proof, that our circle detection is enough).
;;
;; o element definitions (with att definitions in the elements)
;;
;; o store entities in the DTD
;;
;; o better extensibility wrt character representation, one may want to
;;   have
;;    - UTF-8  in standard CL strings
;;    - UCS-2  in RODs
;;    - UTF-16 in RODs
;;    - UCS-4  in vectoren
;;
;; o xstreams auslagern, documententieren und dann auch in SGML und
;;   CSS parser verwenden. (halt alles was zeichen liest).
;;
;; o merge node representation with SGML module
;; 
;; o namespaces (this will get ugly).
;; 
;; o validation
;;
;; o line/column number recording
;;
;; o better error messages
;;
;; o recording of source locations for nodes.
;;
;; o make the *scratch-pad* hack safe
;;
;; o based on the DTD and xml:space attribute implement HTML white
;;   space rules.
;;
;; o on a parser option, do not expand external entities.
;;
;; o on a parser option, do not parse the DTD.
;;
;; o caching DTDs?
;;
;;   That is, if we parse a lot of documents all having the same DTD,
;;   we do not need to re-read it every time.
;;   But watch the file write date, since not doing so would be
;;   a good way to confuse a hell lot of users.
;;   But: What to do with declarations in the <!DOCTYPE header?
;;
;;
;; o does the user need the distinction between "&#20;" and " " ?
;;   That is literal and 'quoted' white space.
;;
;; o on an option merge CDATA section;
;;
;; o data in parse tree? extra nodes like in SGML?!
;;
;; o what to store in the node-gi field? Some name object or the
;;   string used?
;;

;; Test that fail:
;;
;; not-wf/sa/128        is false a alarm
;;

(in-package :xml)

#+ALLEGRO
(setf (excl:named-readtable :glisp) *readtable*)

(eval-when (eval compile load)
  (defparameter *fast* '(optimize (speed 3) (safety 0)))
  ;;(defparameter *fast* '(optimize (speed 2) (safety 3)))
  )

(defvar *expand-pe-p*)

;;;; ---------------------------------------------------------------------------
;;;; xstreams
;;;;


(defstruct (stream-name (:type list))
  entity-name
  entity-kind
  file-name)

(defun print-xstream (self sink depth)
  (declare (ignore depth))
  (format sink "#<~S ~S>" (type-of self) (mu (xstream-name self))))

(deftype read-element () 'rune)

;; (unsigned-byte 16)) ;;t)

(defmethod figure-encoding ((stream null))
  (values :utf-8 nil))

(defmethod figure-encoding ((stream stream))
  (let ((c0 (read-byte stream nil :eof)))
    (cond ((eq c0 :eof)
           (values :utf-8 nil))
          (t
           (let ((c1 (read-byte stream nil :eof)))
             (cond ((eq c1 :eof)
                    (values :utf-8 (list c0)))
                   (t
                    (cond ((and (= c0 #xFE) (= c1 #xFF)) (values :utf-16-big-endian nil))
                          ((and (= c0 #xFF) (= c1 #xFE)) (values :utf-16-little-endian nil))
                          (t
                           (values :utf-8 (list c0 c1)))))))))))

(defun call-with-open-xstream (continuation &rest open-args)
  (let ((input (apply #'open (car open-args) :element-type '(unsigned-byte 8) (cdr open-args))))
    (unwind-protect
        (progn
          (funcall continuation (make-xstream input)))
      (close input))))

(defmacro with-open-xstream ((stream &rest open-args) &body body)
  `(call-with-open-xstream (lambda (,stream) .,body) .,open-args))

;;; Decoders

;; The decoders share a common signature:
;;
;; DECODE input input-start input-end
;;        output output-start output-end
;;        eof-p
;; -> first-not-written ; first-not-read
;;
;; These decode functions should decode as much characters off `input'
;; into the `output' as possible and return the indexes to the first
;; not read and first not written element of `input' and `output'
;; respectively.  If there are not enough bytes in `input' to decode a
;; full character, decoding shold be abandomed; the caller has to
;; ensure that the remaining bytes of `input' are passed to the
;; decoder again with more bytes appended. 
;;
;; `eof-p' now in turn indicates, if the given input sequence, is all
;; the producer does have and might be used to produce error messages
;; in case of incomplete codes or decided what to do.
;;
;; Decoders are expected to handle the various CR/NL conventions and
;; canonicalize each end of line into a single NL rune (#xA) in good
;; old Lisp tradition.
;;

;; TODO: change this to an encoding class, which then might carry
;; additional state. Stateless encodings could been represented by
;; keywords. e.g.
;;
;;  defmethod DECODE-SEQUENCE ((encoding (eql :utf-8)) ...)
;;

;;;; ---------------------------------------------------------------------------
;;;; rod hashtable
;;;;

;;; make-rod-hashtable
;;; rod-hash-get hashtable rod &optional start end -> value ; successp
;;; (setf (rod-hash-get hashtable rod &optional start end) new-value
;;; 

(defstruct (rod-hashtable (:constructor make-rod-hashtable/low))
  size          ;size of table
  table         ;
  )

(defun make-rod-hashtable (&key (size 200))
  (setf size (glisp::nearest-greater-prime size))
  (make-rod-hashtable/low
   :size size
   :table (make-array size :initial-element nil)))

(eval-when (compile eval load)
  (defconstant +fixnum-bits+
      (1- (integer-length most-positive-fixnum))
    "Pessimistic approximation of the number of bits of fixnums.")

  (defconstant +fixnum-mask+
      (1- (expt 2 +fixnum-bits+))
    "Pessimistic approximation of the largest bit-mask, still being a fixnum."))

(defsubst stir (a b)
  (%and +fixnum-mask+
        (%xor (%ior (%ash (%and a #.(ash +fixnum-mask+ -5)) 5)
                    (%ash a #.(- 5 +fixnum-bits+)))
              b)))

(defsubst rod-hash (rod start end)
  "Compute a hash code out of a rod."
  (let ((res (%- end start)))
    (do ((i start (%+ i 1)))
        ((%= i end))
      (declare (type fixnum i))
      (setf res (stir res (%rune rod i))))
    res))

(defsubst rod=* (x y &key (start1 0) (end1 (length x))
                          (start2 0) (end2 (length y)))
  (and (%= (%- end1 start1) (%- end2 start2))
       (do ((i start1 (%+ i 1))
            (j start2 (%+ j 1)))
           ((%= i end1)
            t)
         (unless (= (%rune x i) (%rune y j))
           (return nil)))))

(defsubst rod=** (x y start1 end1 start2 end2)
  (and (%= (%- end1 start1) (%- end2 start2))
       (do ((i start1 (%+ i 1))
            (j start2 (%+ j 1)))
           ((%= i end1)
            t)
         (unless (= (%rune x i) (%rune y j))
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

(deftype ufixnum () `(unsigned-byte ,(integer-length most-positive-fixnum)))

(defun rod-subseq** (source start &optional (end (length source)))
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
        (when (= i 0)
          (return))
        (setf (%rune res i) (%rune source (the ufixnum (+ i start))))))
    res))

(defun (setf rod-hash-get) (new-value hashtable rod &optional (start 0) (end (length rod)))
  (rod-hash-set new-value hashtable rod start end))

(defparameter *name-hashtable* (make-rod-hashtable :size 2000))

(defun intern-name (rod &optional (start 0) (end (length rod)))
  (multiple-value-bind (value successp key) (rod-hash-get *name-hashtable* rod start end)
    (declare (ignore value))
    (if successp
        key
      (nth-value 1 (rod-hash-set t *name-hashtable* rod start end)))))

;;;; ---------------------------------------------------------------------------
;;;;
;;;;  rod collector
;;;;

(defparameter *scratch-pad*
    (make-array 1024 :element-type 'rune))

(defparameter *scratch-pad-2*
    (make-array 1024 :element-type 'rune))

(defparameter *scratch-pad-3*
    (make-array 1024 :element-type 'rune))

(defparameter *scratch-pad-4*
    (make-array 1024 :element-type 'rune))

(declaim (type (simple-array rune (*))
               *scratch-pad* *scratch-pad-2* *scratch-pad-3* *scratch-pad-4*))

(defmacro %put-rune (rune-var put)
  `(progn
     (cond ((%> ,rune-var #xFFFF)
          (,put (the (unsigned-byte 16) (%+ #xD7C0 (ash ,rune-var -10))))
          (,put (the (unsigned-byte 16) (%ior #xDC00 (%and ,rune-var #x3FF)))))
         (t
          (,put ,rune-var)))))

(defun adjust-array-by-copying (old-array new-size)
  "Adjust an array by copying and thus ensures, that result is a SIMPLE-ARRAY."
  (let ((res (make-array new-size :element-type (array-element-type old-array))))
    (replace res old-array
             :start1 0 :end1 (length old-array)
             :start2 0 :end2 (length old-array))
    res))

(defmacro with-rune-collector-aux (scratch collect body mode)
  (let ((rod (gensym))
        (n (gensym))
        (i (gensym))
        (b (gensym)))
    `(let ((,n (length ,scratch))
           (,i 0)
           (,b ,scratch))
       (declare (type fixnum ,n ,i))
       (macrolet 
           ((,collect (x) 
              `((lambda (x)
                  (locally
                      (declare #.*fast*)
                    (when (%>= ,',i ,',n)
                      (setf ,',n (* 2 ,',n))
                      (setf ,',b
                            (setf ,',scratch
                                  (adjust-array-by-copying ,',scratch ,',n))))
                    (setf (aref (the (simple-array rune (*)) ,',b) ,',i) x)
                    (incf ,',i)))
                ,x)))
         ,@body
         ,(ecase mode
            (:intern
             `(intern-name ,b 0 ,i))
            (:copy
             `(let ((,rod (make-rod ,i)))
                (while (not (%= ,i 0))
                       (setf ,i (%- ,i 1))
                       (setf (%rune ,rod ,i) 
                         (aref (the (simple-array rune (*)) ,b) ,i)))
                ,rod))
            (:raw
             `(values ,b 0 ,i))
            )))))

'(defmacro with-rune-collector-aux (scratch collect body mode)
  (let ((rod (gensym))
        (n (gensym))
        (i (gensym))
        (b (gensym)))
    `(let ((,n (length ,scratch))
           (,i 0))
       (declare (type fixnum ,n ,i))
       (macrolet 
           ((,collect (x) 
              `((lambda (x)
                  (locally
                      (declare #.*fast*)
                    (when (%>= ,',i ,',n)
                      (setf ,',n (* 2 ,',n))
                      (setf ,',scratch
                            (setf ,',scratch
                                  (adjust-array-by-copying ,',scratch ,',n))))
                    (setf (aref (the (simple-array rune (*)) ,',scratch) ,',i) x)
                    (incf ,',i)))
                ,x)))
         ,@body
         ,(ecase mode
            (:intern
             `(intern-name ,scratch 0 ,i))
            (:copy
             `(let ((,rod (make-rod ,i)))
                (while (%> ,i 0)
                       (setf ,i (%- ,i 1))
                       (setf (%rune ,rod ,i) 
                         (aref (the (simple-array rune (*)) ,scratch) ,i)))
                ,rod))
            (:raw
             `(values ,scratch 0 ,i))
            )))))

(defmacro with-rune-collector ((collect) &body body)
  `(with-rune-collector-aux *scratch-pad* ,collect ,body :copy))

(defmacro with-rune-collector-2 ((collect) &body body)
  `(with-rune-collector-aux *scratch-pad-2* ,collect ,body :copy))

(defmacro with-rune-collector-3 ((collect) &body body)
  `(with-rune-collector-aux *scratch-pad-3* ,collect ,body :copy))

(defmacro with-rune-collector-4 ((collect) &body body)
  `(with-rune-collector-aux *scratch-pad-4* ,collect ,body :copy))

(defmacro with-rune-collector/intern ((collect) &body body)
  `(with-rune-collector-aux *scratch-pad* ,collect ,body :intern))

(defmacro with-rune-collector/raw ((collect) &body body)
  `(with-rune-collector-aux *scratch-pad* ,collect ,body :raw))

#|
(defmacro while-reading-runes ((reader stream-in) &rest body)
  ;; Thou shalt not leave body via a non local exit
  (let ((stream (make-symbol "STREAM"))
        (rptr (make-symbol "RPTR"))
        (fptr (make-symbol "FPTR"))
        (buf  (make-symbol "BUF")) )
    `(let* ((,stream ,stream-in)
            (,rptr (xstream-read-ptr ,stream))
            (,fptr (xstream-fill-ptr ,stream))
            (,buf  (xstream-buffer ,stream)))
       (declare (type fixnum ,rptr ,fptr)
                (type xstream ,stream))
       (macrolet ((,reader (res-var)
                    `(cond ((%= ,',rptr ,',fptr)
                            (setf (xstream-read-ptr ,',stream) ,',rptr)
                            (setf ,res-var (xstream-underflow ,',stream))
                            (setf ,',rptr (xstream-read-ptr ,',stream))
                            (setf ,',fptr (xstream-fill-ptr ,',stream))
                            (setf ,',buf  (xstream-buffer ,',stream)))
                           (t
                            (setf ,res-var
                              (aref (the (simple-array read-element (*)) ,',buf)
                                    (the fixnum ,',rptr)))
                            (setf ,',rptr (%+ ,',rptr 1))))))
         (prog1
             (let () .,body)
           (setf (xstream-read-ptr ,stream) ,rptr) )))))
|#

;;;;  ---------------------------------------------------------------------------
;;;;  DTD
;;;;

(defparameter *entities* nil)
(defvar *dtd*)

(defun absolute-uri (sysid source-stream)
  (setq sysid (rod-string sysid))
  (let ((base-sysid
         (dolist (k (zstream-input-stack source-stream))
           (let ((base-sysid (stream-name-file-name (xstream-name k))))
             (when base-sysid (return base-sysid))))))
    (assert (not (null base-sysid)))
    (merge-sysid sysid base-sysid)))

(defun absolute-extid (source-stream extid)
  (case (car extid)
    (:system
     (list (car extid)
           (absolute-uri (cadr extid) source-stream)))
    (:public
     (list (car extid)
           (cadr extid)
           (absolute-uri (caddr extid) source-stream)))))

(defun define-entity (source-stream name kind def)
  (when (eq (car def) :external)
    (setf def
      (list (car def) (absolute-extid source-stream (cadr def)))))
  (setf name (intern-name name))
  (setf *entities*
    (append *entities*
            (list (cons (list kind name)
                        def)))))

#||
(defun define-element (zinput dtd element-name content-model)
  ;; zinput is for source code location recoding
  (let ((elmdef (make-elmdef :name element-name
                             :content content-model
                             )))
    ()))
||#

(defun entity->xstream (entity-name kind &optional zstream)
  ;; `zstream' is for error messages
  (let ((looked (assoc (list kind entity-name) *entities* :test #'equal)))
    (unless looked
      (if zstream 
          (perror zstream "Entity '~A' is not defined." (rod-string entity-name))
        (error "Entity '~A' is not defined." (rod-string entity-name))))
    (let (r)
      (ecase (cadr looked)
        (:internal 
         (setf r (make-rod-xstream (caddr looked)))
         (setf (xstream-name r)
           (make-stream-name :entity-name entity-name
                             :entity-kind kind
                             :file-name nil)))
        (:external
         (setf r (open-extid (caddr looked)))
         (setf (stream-name-entity-name (xstream-name r)) entity-name
               (stream-name-entity-kind (xstream-name r)) kind)))
      r)))

(defun entity-source-kind (name type)
  (let ((looked (assoc (list type name) *entities* :test #'equal)))
    (unless looked
      (error "Entity '~A' is not defined." (rod-string name)))
    (cadr looked)))

(defun open-extid (extid)
  (let ((nam (ecase (car extid)
               (:SYSTEM (cadr extid))
               (:PUBLIC (caddr extid)))))
    (make-xstream (open-sysid nam)
                  :name (make-stream-name :file-name nam)
                  :initial-speed 1)))

(defun call-with-entity-expansion-as-stream (zstream cont name kind)
  ;; `zstream' is for error messages -- we need something better!
  (let ((in (entity->xstream name kind zstream)))
    (unwind-protect
        (funcall cont in)
      (close-xstream in))))

(defun define-default-entities ()
  (define-entity nil '#.(string-rod "lt")   :general `(:internal #.(string-rod "&#60;")))
  (define-entity nil '#.(string-rod "gt")   :general `(:internal #.(string-rod ">")))
  (define-entity nil '#.(string-rod "amp")  :general `(:internal #.(string-rod "&#38;")))
  (define-entity nil '#.(string-rod "apos") :general `(:internal #.(string-rod "'")))
  (define-entity nil '#.(string-rod "quot") :general `(:internal #.(string-rod "\"")))
  ;;
  #||
  (define-entity nil '#.(string-rod "ouml") :general `(:internal #.(string-rod "ö")))
  (define-entity nil '#.(string-rod "uuml") :general `(:internal #.(string-rod "ü")))
  (define-entity nil '#.(string-rod "auml") :general `(:internal #.(string-rod "ä")))
  (define-entity nil '#.(string-rod "Ouml") :general `(:internal #.(string-rod "Ö")))
  (define-entity nil '#.(string-rod "Auml") :general `(:internal #.(string-rod "Ä")))
  (define-entity nil '#.(string-rod "Uuml") :general `(:internal #.(string-rod "Ü")))
  (define-entity nil '#.(string-rod "szlig") :general `(:internal #.(string-rod "ß")))
  ||#
  ;;
  #||
  (define-entity nil '#.(string-rod "nbsp") 
    :general `(:internal ,(let ((r (make-rod 1)))
                            (setf (aref r 0) #o240)
                            r)))
  ||#
  )

(defstruct attdef
  ;; an attribute definition
  element       ;name of element this attribute belongs to
  name          ;name of attribute
  type          ;type of attribute; either one of :CDATA, :ID, :IDREF, :IDREFS,
                ; :ENTITY, :ENTITIES, :NMTOKEN, :NMTOKENS, or
                ; (:NOTATION <name>*)
                ; (:ENUMERATION <name>*)
  default)      ;default value of attribute:
                ; :REQUIRED, :IMPLIED, (:FIXED content) or (:DEFAULT content)

(defstruct elmdef
  ;; an element definition
  name          ;name of the element
  content       ;content model
  attributes    ;list of defined attribtes
  defined-p)    ;is this element defined? [*]

;; [*] in XML it is possible to define attributes, before the element
;; itself is defined and since we hang attribute definitions into the
;; relevant element definitions, this flag indicates, whether an
;; element was actually defined.

(defstruct dtd
  elements      ;hashtable or whatnot of all elements
  attdefs       ;
  gentities     ;general entities
  pentities     ;parameter entities
  )

;;;;

(defun define-attribute (dtd element name type default)
  (let ((adef (make-attdef :element element
                           :name name
                           :type type
                           :default default)))
    (cond ((find-attribute dtd element name)
           (warn "Attribute \"~A\" of \"~A\" not redefined."
                 (rod-string name)
                 (rod-string element)))
          (t
           (push adef (dtd-attdefs dtd))))))

(defun find-attribute (dtd element name)
  (dolist (k (dtd-attdefs dtd))
    (cond ((and (eq element (attdef-element k))
                (eq name (attdef-name k)))
           (return k)))))

(defun map-all-attdefs-for-element (dtd element continuation)
  (declare (dynamic-extent continuation));this does not help under ACL
  (dolist (k (dtd-attdefs dtd))
    (cond ((eq element (attdef-element k))
           (funcall continuation k)))))

;;;; ---------------------------------------------------------------------------
;;;;  z streams and lexer
;;;;

(defstruct zstream
  token-category
  token-semantic
  input-stack)

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

(defvar *data-behaviour*
    )           ;either :DTD or :DOC

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
                  (values :name (read-name-token input)))
                 ((rune= #/# c)
                  (let ((q (read-name-token input)))
                    (cond ((equalp q '#.(string-rod "REQUIRED")) :|#REQUIRED|)
                          ((equalp q '#.(string-rod "IMPLIED")) :|#IMPLIED|)
                          ((equalp q '#.(string-rod "FIXED"))   :|#FIXED|)
                          ((equalp q '#.(string-rod "PCDATA"))  :|#PCDATA|)
                          (t
                           (error "Unknown token: ~S." q)))))
                 ((or (rune= c #/U+0020)
                      (rune= c #/U+0009)
                      (rune= c #/U+000D)
                      (rune= c #/U+000A))
                  (values :s nil))
                 ((rune= #/% c)
                  (cond ((name-start-rune-p (peek-rune input))
                         ;; an entity reference
                         (read-pe-reference zinput))
                        (t
                         (values :%))))
                 (t
                  (error "Unexpected character ~S." c))))
          (:DOC
           (cond 
            ((rune= c #/&)
             (multiple-value-bind (kind data) (read-entity-ref input)
               (cond ((eq kind :named)
                      (values :entity-ref data) )
                     ((eq kind :numeric)
                      (values :cdata
                              (with-rune-collector (collect)
                                (%put-rune data collect)))))))
            (t
             (unread-rune c input)
             (values :cdata (read-cdata input))) ))))))))

(defun read-pe-reference (zinput)
  (let* ((input (car (zstream-input-stack zinput)))
         (nam (read-name-token input)))
    (assert (rune= #/\; (read-rune input)))
    (cond (*expand-pe-p*
           ;; no external entities here!
           (let ((i2 (entity->xstream nam :parameter)))
             (zstream-push i2 zinput))
           (values :S nil) ;space before inserted PE expansion.
           )
          (t
           (values :pe-reference nam)) )))

(defun read-token-after-|<| (zinput input)
  (let ((d (read-rune input)))
    (cond ((eq d :eof)
           (error "EOF after '<'"))
          ((rune= #/! d)
           (read-token-after-|<!| input))
          ((rune= #/? d)
           (multiple-value-bind (target content) (read-pi input)
             (cond ((rod= target '#.(string-rod "xml"))
                    (values :xml-pi (cons target content)))
                   ((rod-equal target '#.(string-rod "XML"))
                    (error "You lost -- no XML processing instructions."))
		   ((and sax:*namespace-processing* (position #/: target))
		    (error "Processing instruction target ~S is not a valid NcName."
			   (mu target)))
                   (t
                    (values :pi (cons target content))))))
          ((rune= #// d)
           (let ((c (peek-rune input)))
             (cond ((name-start-rune-p c)
                    (read-tag-2 zinput input :etag))
                   (t
                    (error "Expecting name start rune after \"</\".")))))
          ((name-start-rune-p d)
           (unread-rune d input)
           (read-tag-2 zinput input :stag))
          (t
           (error "Expected '!' or '?' after '<' in DTD.")))))

(defun read-token-after-|<!| (input)
  (let ((d (read-rune input)))
    (cond ((eq d :eof)
           (error "EOF after \"<!\"."))
          ((name-start-rune-p d)
           (unread-rune d input)
           (let ((name (read-name-token input)))
             (cond ((rod= name '#.(string-rod "ELEMENT")) :|<!ELEMENT|)
                   ((rod= name '#.(string-rod "ENTITY")) :|<!ENTITY|)
                   ((rod= name '#.(string-rod "ATTLIST")) :|<!ATTLIST|)
                   ((rod= name '#.(string-rod "NOTATION")) :|<!NOTATION|)
                   ((rod= name '#.(string-rod "DOCTYPE")) :|<!DOCTYPE|)
                   (t
                    (error "`<!~A' unknown." (rod-string name))))))
          ((rune= #/\[ d)
           (values :|<![| nil))
          ((rune= #/- d)
           (setf d (read-rune input))
           (cond ((rune= #/- d)
                  (values
                   :COMMENT
                   (read-comment-content input)))
                 (t
                  (error "Bad character ~S after \"<!-\"" d))))
          (t
           (error "Bad character ~S after \"<!\"" d)))))

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

(defun read-entity-ref (input)
  "Read an entity reference off the xstream `input'. Returns two values:
   either :NAMED <interned-rod> in case of a named entity
   or     :NUMERIC <integer> in case of numeric entities.
   The initial #\\& is considered to be consumed already."
  (let ((c (peek-rune input)))
    (cond ((eq c :eof)
           (error "EOF after '&'"))
          ((rune= c #/#)
           (values :numeric (read-numeric-entity input)))
          (t
           (unless (name-start-rune-p (peek-rune input))
             (error "Expecting name after &."))
           (let ((name (read-name-token input)))
             (setf c (read-rune input))
             (unless (rune= c #/\;)
               (perror input "Expected \";\"."))
             (values :named name))))))

(defsubst read-S? (input)
  (while (member (peek-rune input) '(#/U+0020 #/U+0009 #/U+000A #/U+000D)
                 :test #'eq)
    (consume-rune input)))

(defun read-tag-2 (zinput input kind)
  (let ((name (read-name-token input))
        (atts nil))
    (setf atts (read-attribute-list zinput input nil))
    ;;(setf atts (nreverse atts))
    ;; care for atts
    ;;
    ;;zzz
    (let ((fn (lambda (adef &aux x)
                 (setf x (assoc (attdef-name adef) atts))
       
                 (when (and (consp (attdef-default adef))
                            (eq (car (attdef-default adef)) :default)
                            (not x))
                   (setf atts (cons (setf x (cons (attdef-name adef) (cadr (attdef-default adef))))
                                    atts)))
                 (when (and (consp (attdef-default adef))
                            (eq (car (attdef-default adef)) :fixed)
                            (not x))
                   (setf atts (cons (setf x (cons (attdef-name adef) (cadr (attdef-default adef))))
                                    atts)))
                 (unless (eq (attdef-type adef) :cdata)
                   (when x
                     (setf (cdr x) (canon-not-cdata-attval (cdr x)))))
       
                 ;; xxx more tests
                 )))
       (declare (dynamic-extent fn))
       (map-all-attdefs-for-element 
        *dtd* name fn))

    ;; check for double attributes
    (do ((q atts (cdr q)))
        ((null q))
      (cond ((find (caar q) (cdr q) :key #'car)
             (error "Attribute ~S has two definitions in element ~S."
                    (rod-string (caar q))
                    (rod-string name)))))

    (cond ((eq (peek-rune input) #/>)
           (consume-rune input)
           (values kind (cons name atts)))
          ((eq (peek-rune input) #//)
           (consume-rune input)
           (assert (rune= #/> (read-rune input)))
           (values :ztag (cons name atts)))
          (t
           (error "syntax error in read-tag-2.")) )))

(defun read-attribute (zinput input)
  (unless (name-start-rune-p (peek-rune input))
    (error "Expected name."))
  ;; arg thanks to the post mortem nature of name space declarations,
  ;; we could only process the attribute values post mortem.
  (let ((name (read-name-token input)))
    (while (let ((c (peek-rune input)))
             (and (not (eq c :eof))
                  (or (= c #/U+0020)
                      (= c #/U+0009)
                      (= c #/U+000A)
                      (= c #/U+000D))))
      (consume-rune input))
    (unless (eq (read-rune input) #/=)
      (perror zinput "Expected \"=\"."))
    (while (let ((c (peek-rune input)))
             (and (not (eq c :eof))
                  (or (= c #/U+0020)
                      (= c #/U+0009)
                      (= c #/U+000A)
                      (= c #/U+000D))))
      (consume-rune input))
    (cons name (read-att-value-2 input))
    ;;(cons name (read-att-value zinput input :att t))
    ))

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
                 (cond ((= c #x20)
                        (setf gimme-20 t))
                       (t
                        (when (and anything-seen-p gimme-20)
                          (collect #x20))
                        (setf gimme-20 nil)
                        (setf anything-seen-p t)
                        (collect c))))
           value))))

#||
(defun canon-not-cdata-attval (value)
  ;; | If the declared value is not CDATA, then the XML processor must
  ;; | further process the normalized attribute value by discarding any
  ;; | leading and trailing space (#x20) characters, and by replacing
  ;; | sequences of space (#x20) characters by a single space (#x20)
  ;; | character.
  value)
||#

(defsubst data-rune-p (c)
  ;; any Unicode character, excluding the surrogate blocks, FFFE, and FFFF.
  (or (= c #x9) (= c #xA) (= c #xD)
      (<= #x20 c #xD7FF)
      (<= #xE000 c #xFFFD)
      ;;
      (<= #xD800 c #xDBFF)
      (<= #xDC00 c #xDFFF)
      ;;
      ))

#||
(defsubst data-rune-p (c)
  t)
||#

(defun read-att-value (zinput input mode &optional canon-space-p (delim nil))
  (with-rune-collector-2 (collect)
    (labels ((muffle (input delim)
               (let (c)
                 (loop
                   (setf c (read-rune input))
                   (cond ((eql delim c)
                          (return))
                         ((eq c :eof)
                          (error "EOF"))
                         ((rune= c #/&)
                          (setf c (peek-rune input))
                          (cond ((rune= c #/#)
                                 (let ((c (read-numeric-entity input)))
                                   (%put-rune c collect)))
                                (t
                                 (unless (name-start-rune-p (peek-rune input))
                                   (error "Expecting name after &."))
                                 (let ((name (read-name-token input)))
                                   (setf c (read-rune input))
                                   (assert (rune= c #/\;))
                                   (ecase mode
                                     (:att
                                      (recurse-on-entity 
                                       zinput name :general
                                       (lambda (zinput)
                                         (muffle (car (zstream-input-stack zinput))
                                                 :eof))))
                                     (:ent
                                      ;; bypass, but never the less we
                                      ;; need to check for legal
                                      ;; syntax.
                                      ;; Must it be defined?
                                      ;; allerdings: unparsed sind verboten
                                      (collect #/&)
                                      (map nil (lambda (x) (collect x)) name)
                                      (collect #/\; )))))))
                         ((and (eq mode :ent) (rune= c #/%))
                          (unless (name-start-rune-p (peek-rune input))
                            (error "Expecting name after %."))
                          (let ((name (read-name-token input)))
                            (setf c (read-rune input))
                            (assert (rune= c #/\;))
                            (cond (*expand-pe-p*
                                   (recurse-on-entity 
                                    zinput name :parameter
                                    (lambda (zinput)
                                      (muffle (car (zstream-input-stack zinput))
                                              :eof))))
                                  (t
                                   (error "No PE here.")))))
                         ((and (eq mode :att) (rune= c #/<))
                          ;; xxx fix error message
                          (cerror "Eat them in spite of this."
                                  "For no apparent reason #\/< is forbidden in attribute values. ~
                           You lost -- next time choose SEXPR syntax.")
                          (collect c))
                         ((and canon-space-p (space-rune-p c))
                          (collect #/space))
                         ((not (data-rune-p c))
                          (error "illegal char: ~S." c))
                         (t
                          (collect c)))))))
      (declare (dynamic-extent #'muffle))
      (muffle input (or delim
                        (let ((delim (read-rune input)))
                          (assert (member delim '(#/\" #/\')))
                          delim))))))

(defun read-numeric-entity (input)
  ;; xxx eof handling
  ;; The #/& is already read
  (let ((res
         (let ((c (read-rune input)))
           (assert (rune= c #/#))
           (setq c (read-rune input))
           (cond ((rune= c #/x)
                  ;; hexadecimal
                  (setq c (read-rune input))
                  (assert (digit-rune-p c 16))
                  (prog1
                      (parse-integer
                       (with-output-to-string (sink)
                         (write-char (code-char c) sink)
                         (while (digit-rune-p (setq c (read-rune input)) 16)
                                (write-char (code-char c) sink)))
                       :radix 16)
                    (assert (rune= c #/\;)))
                  )
                 ((<= #/0 c #/9)
                  ;; decimal
                  (prog1
                      (parse-integer
                       (with-output-to-string (sink)
                         (write-char (code-char c) sink)
                         (while (<= #/0 (setq c (read-rune input)) #/9)
                                (write-char (code-char c) sink)))
                       :radix 10)
                    (assert (rune= c #/\;))) )
                 (t
                  (error "Bad char in numeric character entity.") )))))
    (unless (data-char-p res)
      (error "expansion of numeric character reference (#x~X) is no data char."
             res))
    res))

(defun read-pi (input)
  ;; "<?" is already read
  (let (name)
    (let ((c (peek-rune input)))
      (unless (name-start-rune-p c)
        (error "Expecting name after '<?'"))
      (setf name (read-name-token input)))
    (values name
            (read-pi-content input))))

(defun read-pi-content (input &aux d)
  (read-s? input)
  (with-rune-collector (collect)
    (block nil
      (tagbody
       state-1
        (setf d (read-rune input))
        (unless (data-rune-p d)
          (error "Illegal char: ~S." d))
        (when (rune= d #/?) (go state-2))
        (collect d)
        (go state-1)
       state-2 ;; #/? seen
        (setf d (read-rune input))
        (unless (data-rune-p d)
          (error "Illegal char: ~S." d))
        (when (rune= d #/>) (return))
        (when (rune= d #/?) 
          (collect #/?) 
          (go state-2))
        (collect #/?)
        (collect d)
        (go state-1)))))

(defun read-comment-content (input &aux d)
  (let ((warnedp nil))
    (with-rune-collector (collect)
      (block nil
        (tagbody
         state-1
          (setf d (read-rune input))
          (unless (data-rune-p d)
            (error "Illegal char: ~S." d))
          (when (rune= d #/-) (go state-2))
          (collect d)
          (go state-1)
         state-2 ;; #/- seen
          (setf d (read-rune input))
          (unless (data-rune-p d)
            (error "Illegal char: ~S." d))
          (when (rune= d #/-) (go state-3))
          (collect #/-)
          (collect d)
          (go state-1)
         state-3 ;; #/- #/- seen
          (setf d (read-rune input))
          (unless (data-rune-p d)
            (error "Illegal char: ~S." d))
          (when (rune= d #/>) (return))
          (unless warnedp
            (warn "WFC: no '--' in comments please.")
            (setf warnedp t))
          (when (rune= d #/-)
            (collect #/-)
            (go state-3))
          (collect #/-)
          (collect #/-)
          (collect d)
          (go state-1))))))

(defun read-cdata-sect (input &aux d)
  ;; <![CDATA[ is already read
  ;; read anything up to ]]>
  (with-rune-collector (collect)
    (block nil
      (tagbody
       state-1
        (setf d (read-rune input))
        (unless (data-rune-p d)
          (error "Illegal char: ~S." d))
        (when (rune= d #/\]) (go state-2))
        (collect d)
        (go state-1)
       state-2 ;; #/] seen
        (setf d (read-rune input))
        (unless (data-rune-p d)
          (error "Illegal char: ~S." d))
        (when (rune= d #/\]) (go state-3))
        (collect #/\])
        (collect d)
        (go state-1)
       state-3 ;; #/\] #/\] seen
        (setf d (read-rune input))
        (unless (data-rune-p d)
          (error "Illegal char: ~S." d))
        (when (rune= d #/>)
          (return))
        (when (rune= d #/\])
          (collect #/\])
          (go state-3))
        (collect #/\])
        (collect #/\])
        (collect d)
        (go state-1)))))

#+(or) ;; FIXME: There is another definition below that looks more reasonable.
(defun read-cdata (input initial-char &aux d)
  (cond ((not (data-rune-p initial-char))
         (error "Illegal char: ~S." initial-char)))
  (with-rune-collector (collect)
    (block nil
      (tagbody
        (cond ((rune= initial-char #/\])
               (go state-2))
              (t
               (collect initial-char)))
       state-1
        (setf d (peek-rune input))
        (when (or (eq d :eof) (rune= d #/<) (rune= d #/&))
          (return))
        (read-rune input)
        (unless (data-rune-p d)
          (error "Illegal char: ~S." d))
        (when (rune= d #/\]) (go state-2))
        (collect d)
        (go state-1)
        
       state-2 ;; #/\] seen
        (setf d (peek-rune input))
        (when (or (eq d :eof) (rune= d #/<) (rune= d #/&))
          (collect #/\])
          (return))
        (read-rune input)
        (unless (data-rune-p d)
          (error "Illegal char: ~S." d))
        (when (rune= d #/\]) (go state-3))
        (collect #/\])
        (collect d)
        (go state-1)
        
       state-3 ;; #/\] #/\] seen
        (setf d (peek-rune input))
        (when (or (eq d :eof) (rune= d #/<) (rune= d #/&))
          (collect #/\])
          (collect #/\])
          (return))
        (read-rune input)
        (unless (data-rune-p d)
          (error "Illegal char: ~S." d))
        (when (rune= d #/>) 
          (error "For no apparent reason ']]>' in not allowed in a CharData token -- you lost."))
        (when (rune= d #/\])
          (collect #/\])
          (go state-3))
        (collect #/\])
        (collect #/\])
        (collect d)
        (go state-1)))))


;; some character categories

#||
(defun name-start-rune-p (rune)
  (or (<= #x0041 rune #x005A)
      (<= #x0061 rune #x007A)
      ;; lots more
      (>= rune #x0080)
      (rune= rune #/_)
      (rune= rune #/:)))

(defun name-rune-p (rune)
  (or (name-start-rune-p rune)
      (rune= rune #/.)
      (rune= rune #/-)
      (rune<= #/0 rune #/9)))
||#

(defun space-rune-p (rune)
  (declare (type rune rune))
  (or (rune= rune #/U+0020)
      (rune= rune #/U+0009)
      (rune= rune #/U+000A)
      (rune= rune #/U+000D)))

(defun data-char-p (c)
  ;; any Unicode character, excluding the surrogate blocks, FFFE, and FFFF.
  (or (= c #x9) (= c #xA) (= c #xD)
      (<= #x20 c #xD7FF)
      (<= #xE000 c #xFFFD)
      (<= #x10000 c #x10FFFF)))

(defun pubid-char-p (c)
  (or (= c #x20) (= c #xD) (= c #xA)
      (<= #/a c #/z)
      (<= #/A c #/Z)
      (<= #/0 c #/9)
      (member c '(#/- #/' #/\( #/\) #/+ #/, #/. #//
                  #/: #/= #/? #/\; #/! #/* #/#
                  #/@ #/$ #/_ #/%))))


(defun expect (input category)
  (multiple-value-bind (cat sem) (read-token input)
    (unless (eq cat category)
      (error "Expected ~S saw ~S [~S]" category cat sem))
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

(defun p/name (input)
  (nth-value 1 (expect input :name)))

(defun p/attlist-decl (input)
  ;; [52] AttlistDecl ::= '<!ATTLIST' S Name (S AttDef)* S? '>'
  (let (elm-name)
    (expect input :|<!ATTLIST|)
    (p/S input)
    (setf elm-name (p/name input))
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
                    (define-attribute *dtd* elm-name name type default)) )))
          (:>
           (return))
          (otherwise
           (error "Expected either another AttDef or end of \"<!ATTLIST\". -- saw ~S."
                  tok)) )) )))

(defun p/attdef (input)
  ;; [53] AttDef ::= Name S AttType S DefaultDecl
  (let (name type default)
    (setf name (p/name input))
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
    (cond ((eq cat :name)
           (cond ((equalp sem '#.(string-rod "CDATA"))    :cdata)
                 ((equalp sem '#.(string-rod "ID"))       :id)
                 ((equalp sem '#.(string-rod "IDREF"))    :idrefs)
                 ((equalp sem '#.(string-rod "IDREFS"))   :idrefs)
                 ((equalp sem '#.(string-rod "ENTITY"))   :entity)
                 ((equalp sem '#.(string-rod "ENTITIES")) :entities)
                 ((equalp sem '#.(string-rod "NMTOKEN"))  :nmtoken)
                 ((equalp sem '#.(string-rod "NMTOKENS")) :nmtokens)
                 ((equalp sem '#.(string-rod "NOTATION"))
                  ;; xxx nmtoken vs name
                  (let (names)
                    (p/S input)
                    (expect input :\()
                    (setf names (p/list input #'p/name :\| ))
                    (expect input :\))
                    (cons :notation names)))
                 (t
                  (error "In p/att-type: ~S ~S." cat sem))))
          ((eq cat :\()
           ;; xxx nmtoken vs name
           (let (names)
             ;;(expect input :\()
             (setf names (p/list input #'p/name :\| ))
             (expect input :\))
             (cons :enumeration names)))
          (t
           (error "In p/att-type: ~S ~S." cat sem)) )))

(defun p/default-decl (input)
  ;; [60] DefaultDecl ::= '#REQUIRED' | '#IMPLIED'
  ;;                       | (('#FIXED' S)? AttValue) /* VC: Required Attribute */
  ;;                       
  ;; /* VC: Attribute Default Legal */
  ;; /* WFC: No < in Attribute Values */
  ;; /* VC: Fixed Attribute Default */
  (multiple-value-bind (cat sem) (peek-token input)
    (cond ((eq cat :|#REQUIRED|) 
           (consume-token input) :required)
          ((eq cat :|#IMPLIED|)  
           (consume-token input) :implied)
          ((eq cat :|#FIXED|)
           (consume-token input)
           (p/S input)
           (list :fixed (p/att-value input)))
          ((or (eq cat :\') (eq cat :\"))
           (list :default (p/att-value input)))
          (t
           (error "p/default-decl: ~S ~S." cat sem)) )))
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
    (define-entity input name kind def)
    (p/S? input)
    (expect input :\>)))

(defun p/entity-def (input kind)
  (multiple-value-bind (cat sem) (peek-token input)
    (cond ((member cat '(:\" :\'))
           (list :internal (p/entity-value input)))
          ((and (eq cat :name)
                (or (equalp sem '#.(string-rod "SYSTEM"))
                    (equalp sem '#.(string-rod "PUBLIC"))))
           (let (extid ndata)
             (setf extid (p/external-id input nil))
             (when (eq kind :general)   ;NDATA allowed at all?
               (cond ((eq (peek-token input) :S)
                      (p/S? input)
                      (when (and (eq (peek-token input) :name)
                                 (equalp (nth-value 1 (peek-token input))
                                         '#.(string-rod "NDATA")))
                        (consume-token input)
                        (p/S input)
                        (setf ndata (p/name input))))))
             (list :external extid ndata)))
          (t
           (error "p/entity-def: ~S / ~S." cat sem)) )))

(defun p/entity-value (input)
  (let ((delim (if (eq (read-token input) :\") #/\" #/\')))
    (read-att-value input
                    (car (zstream-input-stack input))
                    :ent
                    nil
                    delim)))

(defun p/att-value (input)
  (let ((delim (if (eq (read-token input) :\") #/\" #/\')))
    (read-att-value input
                    (car (zstream-input-stack input))
                    :att
                    t
                    delim)))

(defun p/external-id (input &optional (public-only-ok-p nil))
  ;; xxx public-only-ok-p
  (multiple-value-bind (cat sem) (read-token input)
    (cond ((and (eq cat :name) (equalp sem '#.(string-rod "SYSTEM")))
           (p/S input)
           (list :system (p/system-literal input))
           )
          ((and (eq cat :name) (equalp sem '#.(string-rod "PUBLIC")))
           (let (pub sys)
             (p/S input)
             (setf pub (p/pubid-literal input))
             (when (eq (peek-token input) :S)
               (p/S input)
               (when (member (peek-token input) '(:\" :\'))
                 (setf sys (p/system-literal input))))
             (unless (every #'pubid-char-p pub)
               (error "Illegal pubid: ~S." (rod-string pub)))
             (when (and (not public-only-ok-p)
                        (null sys))
               (error "System identifier needed for this PUBLIC external identifier."))
             (list :public pub sys)))
          (t
           (error "Expected external-id: ~S / ~S." cat sem)))))


;;  [11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
;;  [12]  PubidLiteral ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
;;  [13]     PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9]
;;                         | [-'()+,./:=?;!*#@$_%]

(defun p/system-literal (input)
  (multiple-value-bind (cat) (read-token input)
    (cond ((member cat '(:\" :\'))
           (let ((delim (if (eq cat :\") #/\" #/\')))
             (with-rune-collector (collect)
               (loop
                 (let ((c (read-rune (car (zstream-input-stack input)))))
                   (cond ((eq c :eof)
                          (error "EOF in system literal."))
                         ((rune= c delim)
                          (return))
                         (t
                          (collect c))))))))
          (t
           (error "Expect either \" or \'.")))))

(defun p/pubid-literal (input)
  ;; xxx check for valid chars
  (p/system-literal input))


;;;;

(defun p/element-decl (input)
  (let (name content)
    (expect input :|<!ELEMENT|)
    (p/S input)
    (setf name (p/name input))
    (p/S input)
    (setf content (p/cspec input))
    (unless (legal-content-model-p content)
      '(error "Illegal content model: ~S." (mu content))
      (warn "Illegal content model: ~S." (mu content)))
    (p/S? input)
    (expect input :\>)
    (list :element name content)))

(defun legal-content-model-p (cspec)
  (or (eq cspec :PCDATA)
      (eq cspec :ANY)
      (eq cspec :EMPTY)
      (and (consp cspec)
           (eq (car cspec) '*)
           (consp (cadr cspec))
           (eq (car (cadr cspec)) 'or)
           (eq (cadr (cadr cspec)) :pcdata)
           (every #'vectorp (cddr (cadr cspec))))
      (labels ((walk (x)
                 (cond ((member x '(:PCDATA :ANY :EMPTY))
                        nil)
                       ((atom x) t)
                       ((and (walk (car x))
                             (walk (cdr x)))))))
        (walk cspec))))
                 
;; wir fahren besser, wenn wir machen:

;; cspec ::= 'EMPTY' | 'ANY' | '#PCDATA' 
;;         | Name
;;         | cs
;;    cs ::= '(' S? cspec ( S? '|' S? cs)* S? ')' ('?' | '*' | '+')?
;; und eine post mortem analyse

(defun p/cspec (input &optional (level 0) (only-names-p nil))
  (let ((term
         (let ((names nil) op-cat op res)
           (multiple-value-bind (cat sem) (peek-token input)
             (cond ((eq cat :name) 
                    (consume-token input) 
                    (cond ((rod= sem '#.(string-rod "EMPTY"))
                           :empty)
                          ((rod= sem '#.(string-rod "ANY"))
                           :any)
                          (t
                           sem)))
                   ((and (eq cat :\#PCDATA) (not only-names-p))
                    (unless (= level 1)
                      (error "#PCDATA only on top level in content modell."))
                    (consume-token input)
                    :pcdata)
                   ((and (eq cat :\() (not only-names-p))
                    (consume-token input)
                    (p/S? input)
                    (setq names (list (p/cspec input (+ level 1))))
                    (p/S? input)
                    (let ((on? (eq (car names) :pcdata)))
                      (cond ((member (peek-token input) '(:\| :\,))
                             (setf op-cat (peek-token input))
                             (setf op (if (eq op-cat :\,) 'and 'or))
                             (while (eq (peek-token input) op-cat)
                                    (consume-token input)
                                    (p/S? input)
                                    (push (p/cspec input (+ level 1) on?) names)
                                    (p/S? input))
                             (setf res (cons op (reverse names))))
                            (t
                             (setf res (car names)))))
                    (p/S? input)
                    (expect input :\))
                    res)
                   (t
                    (error "p/cspec - ~s / ~s" cat sem)))))))
    (cond ((eq (peek-token input) :?) (consume-token input) (list '? term))
          ((eq (peek-token input) :+) (consume-token input) (list '+ term))
          ((eq (peek-token input) :*) (consume-token input) (list '* term))
          (t
           term))))
   
;; [52] AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'
    

;; [52] AttlistDecl ::= '<!ATTLIST' S Name AttDefs S? '>'
;; [52] AttlistDecl ::= '<!ATTLIST' S Name S? '>'
;; [53] AttDefs ::= S Name S AttType S DefaultDecl AttDefs
;; [53] AttDefs ::= 

(defun p/notation-decl (input)
  (let (name id)
    (expect input :|<!NOTATION|)
    (p/S input)
    (setf name (p/name input))
    (p/S input)
    (setf id (p/external-id input t))
    (p/S? input)
    (expect input :\>)
    (list :notation-decl name id)))

;;;

(defun p/conditional-sect (input)
  (expect input :<!\[ )
  (p/S? input)
  (multiple-value-bind (cat sem) (read-token input)
    (cond ((and (eq cat :name)
                (rod= sem '#.(string-rod "INCLUDE")))
           (p/include-sect input))
          ((and (eq cat :name)
                (rod= sem '#.(string-rod "IGNORE")))
           (p/ignore-sect input))
          (t
           (error "Expected INCLUDE or IGNORE after \"<![\".")))))

(defun p/include-sect (input)
  ;; <![INCLUDE is already read.
  (p/S? input)
  (expect input :\[)
  (p/ext-subset-decl input)
  (expect input :\])
  (expect input :\])
  (expect input :\>) )

(defun p/ignore-sect (input)
  ;; <![IGNORE is already read.
  (p/S? input)
  (expect input :\[)
  (let ((input (car (zstream-input-stack input))))
    (let ((level 0))
      (do ((c1 (read-rune input) (read-rune input))
           (c2 0 c1)
           (c3 0 c2))
          ((= level -1))
        (declare (type fixnum level))
        (cond ((eq c1 :eof)
               (error "EOF in <![IGNORE ... >")))
        (cond ((and (rune= c3 #/<) (rune= c2 #/!) (rune= c1 #/\[))
               (incf level)))
        (cond ((and (rune= c3 #/\]) (rune= c2 #/\]) (rune= c1 #/>))
               (decf level))) ))))

(defun p/ext-subset-decl (input)
  ;; ( markupdecl | conditionalSect | S )*
  (loop
    (case (let ((*expand-pe-p* nil)) (peek-token input))
      (:|<![| (let ((*expand-pe-p* t)) (p/conditional-sect input)))
      (:S     (consume-token input))
      (:eof   (return))
      ((:|<!ELEMENT| :|<!ATTLIST| :|<!ENTITY| :|<!NOTATION| :PI :COMMENT)
       (let ((*expand-pe-p* t))
         (p/markup-decl input)))
      ((:pe-reference)
       (let ((name (nth-value 1 (read-token input))))
         (recurse-on-entity input name :parameter
                            (lambda (input)
                              (ecase (entity-source-kind name :parameter)
                                (:external
                                 (p/ext-subset input))
                                (:internal
                                 (p/ext-subset-decl input)))
                              (unless (eq :eof (peek-token input))
                                (error "Trailing garbage."))))))
      (otherwise (return)))) )

(defun p/markup-decl (input)
  ;; markupdecl ::= elementdecl | AttlistDecl       /* VC: Proper Declaration/PE Nesting */
  ;;              | EntityDecl | NotationDecl 
  ;;              | PI | Comment               /* WFC: PEs in Internal Subset */
  (case (peek-token input)
    (:|<!ELEMENT|  (p/element-decl input))
    (:|<!ATTLIST|  (p/attlist-decl input))
    (:|<!ENTITY|   (p/entity-decl input))
    (:|<!NOTATION| (p/notation-decl input))
    (:PI           (consume-token input))
    (:COMMENT      (consume-token input))
    (otherwise
     (error "p/markup-decl ~S" (peek-token input)))))

(defun setup-encoding (input xml-header)
  (when (xml-header-encoding xml-header)
    (let ((enc (find-encoding (xml-header-encoding xml-header))))
      (cond (enc
             (setf (xstream-encoding (car (zstream-input-stack input)))
               enc))
            (t
             (warn "There is no such encoding: ~S." (xml-header-encoding xml-header)))))))

(defun set-full-speed (input)
  (let ((xstream (car (zstream-input-stack input))))
    (when xstream
      (setf (xstream-speed xstream)
        (length (xstream-os-buffer xstream))))
    '(warn "Reverting ~S to full speed." input)
    ))

(defun p/ext-subset (input)
  (cond ((eq (peek-token input) :xml-pi)
         (let ((hd (parse-xml-pi (cdr (nth-value 1 (peek-token input))) nil)))
           (setup-encoding input hd))
         (consume-token input)))
  (set-full-speed input)
  (p/ext-subset-decl input)
  (unless (eq (peek-token input) :eof)
    (error "Trailing garbage - ~S." (peek-token input))))

(defun p/doctype-decl (input)
  (let ((*expand-pe-p* nil))
    (let (name extid)
      (expect input :|<!DOCTYPE|)
      (p/S input)
      (setq name (p/name input))
      (when (eq (peek-token input) :S)
        (p/S input)
        (unless (or (eq (peek-token input) :\[ )
                    (eq (peek-token input) :\> ))
          (setf extid (p/external-id input t))))
      (p/S? input)
      (when (eq (peek-token input) :\[ )
        (consume-token input)
        (while (progn (p/S? input)
                      (not (eq (peek-token input) :\] )))
          (if (eq (peek-token input) :pe-reference)
              (let ((name (nth-value 1 (read-token input))))
                (recurse-on-entity input name :parameter
                                   (lambda (input)
                                     (ecase (entity-source-kind name :parameter)
                                       (:external
                                        (p/ext-subset input))
                                       (:internal
                                        (p/ext-subset-decl input)))
                                     (unless (eq :eof (peek-token input))
                                       (error "Trailing garbage.")))))
            (p/markup-decl input)))
        (consume-token input)
        (p/S? input))
      (expect input :>)
      (when extid
        (let* ((xi2 (open-extid (absolute-extid input extid)))
               (zi2 (make-zstream :input-stack (list xi2))))
          (let ()
            (p/ext-subset zi2))))
      (list :doctype name extid))))

(defun p/misc*-2 (input)
  ;; Misc*
  (while (member (peek-token input) '(:comment :pi :s))
    (when (eq (peek-token input) :pi)
      (sax:processing-instruction 
             *handler*
             (car (nth-value 1 (peek-token input)))
             (cdr (nth-value 1 (peek-token input)))))
      (consume-token input)))
  

(defvar *handler*)

(defun p/document (input handler)
  (let ((*handler* handler)
	(*namespace-bindings* *default-namespace-bindings*))
    (setf *entities* nil)
    (setf *dtd* (make-dtd))
    (define-default-entities)
    (sax:start-document *handler*)
    ;; document ::= XMLDecl? Misc* (doctypedecl Misc*)? element Misc*
    ;; Misc ::= Comment | PI |  S
    ;; xmldecl::='<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
    ;; sddecl::= S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
    ;;
    ;; we will use the attribute-value parser for the xml decl.
    (let ((*data-behaviour* :DTD))
      ;; optional XMLDecl?
      (cond ((eq (peek-token input) :xml-pi)
             (let ((hd (parse-xml-pi (cdr (nth-value 1 (peek-token input))) t)))
               (setup-encoding input hd))
	     ;; FIXME: Ceci n'est pas un pi. Should probably go away.
	     ;; (hmot 30/06/03)
             (sax:processing-instruction
                    *handler*
                    (car (nth-value 1 (peek-token input)))
                    (cdr (nth-value 1 (peek-token input))))
             (read-token input)))
      (set-full-speed input)
      ;; Misc*
      (p/misc*-2 input)
      ;; (doctypedecl Misc*)?
      (when (eq (peek-token input) :<!doctype)
        (p/doctype-decl input)
        (p/misc*-2 input))
      ;; element
      (let ((*data-behaviour* :doc))
        (p/element input))
      ;; optional Misc*
      (p/misc*-2 input)
      (unless (eq (peek-token input) :eof)
        (error "Garbage at end of document."))
      (sax:end-document *handler*))))

(defun p/element (input)
  (if sax:*namespace-processing*
      (p/element-ns input)
      (p/element-no-ns input)))
    
(defun p/element-no-ns (input)
  ;;    [39] element ::= EmptyElemTag | STag content ETag
  (multiple-value-bind (cat sem) (read-token input)
    (cond ((eq cat :ztag)
	   (sax:start-element *handler* nil nil (car sem) (build-attribute-list-no-ns (cdr sem)))
	   (sax:end-element *handler* nil nil (car sem)))

          ((eq cat :stag)
	   (sax:start-element *handler* nil nil (car sem) (build-attribute-list-no-ns (cdr sem)))
	   (p/content input)
	   (multiple-value-bind (cat2 sem2) (read-token input)
               (unless (and (eq cat2 :etag)
                            (eq (car sem2) (car sem)))
                 (perror input "Bad nesting. ~S / ~S" (mu sem) (mu (cons cat2 sem2)))))
	   (sax:end-element *handler* nil nil (car sem)))

          (t
           (error "Expecting element.")))))

(defun p/element-ns (input)
  (destructuring-bind (cat (name &rest attrs))
      (multiple-value-list (read-token input))
    (let ((ns-decls (declare-namespaces attrs)))
      (multiple-value-bind (ns-uri prefix local-name) (decode-qname name)
	(declare (ignore prefix))
	(let ((attlist (build-attribute-list-ns attrs)))
	  (cond ((eq cat :ztag)
		 (sax:start-element *handler* ns-uri local-name name attlist)
		 (sax:end-element *handler* ns-uri local-name name))
		
		((eq cat :stag)
		 (sax:start-element *handler* ns-uri local-name name attlist)
		 (p/content input)
		 (multiple-value-bind (cat2 sem2) (read-token input)
		   (unless (and (eq cat2 :etag)
				(eq (car sem2) name))
		     (perror input "Bad nesting. ~S / ~S" (mu name) (mu (cons cat2 sem2)))))
		 (sax:end-element *handler* ns-uri local-name name))
		
		(t
		 (error "Expecting element.")))))
      (undeclare-namespaces ns-decls))))
      
(defun perror (stream format-string &rest format-args)
  (when (zstream-p stream)
    (setf stream (car (zstream-input-stack stream))))
  (error "Parse error at line ~D column ~D: ~A" 
         (xstream-line-number stream)
         (xstream-column-number stream)
         (apply #'format nil format-string format-args)))

(defun p/content (input)
  ;; [43] content ::= (element | CharData | Reference | CDSect | PI | Comment)*
  (multiple-value-bind (cat sem) (peek-token input)
    (case cat
      ((:stag :ztag)
       (p/element input)
       (p/content input))
      ((:cdata)
       (consume-token input)
       (sax:characters *handler* sem)
       (p/content input))
      ((:entity-ref)
       (let ((name sem))
         (consume-token input)
         (append ;; nil  #+(OR)
          (recurse-on-entity input name :general
                             (lambda (input)
                               (prog1
                                   (ecase (entity-source-kind name :general)
                                     (:internal (p/content input))
                                     (:external (p/ext-parsed-ent input)))
                                 (unless (eq (peek-token input) :eof)
                                   (error "Trailing garbage. - ~S" (peek-token input))))))
          (p/content input))))
      ((:<!\[)
       (consume-token input)
       (cons 
        (let ((input (car (zstream-input-stack input))))
          (unless (and (rune= #/C (read-rune input))
                       (rune= #/D (read-rune input))
                       (rune= #/A (read-rune input))
                       (rune= #/T (read-rune input))
                       (rune= #/A (read-rune input))
                       (rune= #/\[ (read-rune input)))
            (error "After '<![', 'CDATA[' is expected."))
	  (sax:start-cdata *handler*)
	  (sax:characters  *handler* (read-cdata-sect input))
	  (sax:end-cdata *handler*))
        (p/content input)))
      ((:pi)
       (consume-token input)
       (sax:processing-instruction *handler* (car sem) (cdr sem))
       (p/content input))
      ((:comment) ;; FIXME: should call sax:comment. How does this work?
       (consume-token input)
       (p/content input))
      (otherwise
       nil))))

;; [78] extParsedEnt ::= TextDecl? contentw
;; [79]        extPE ::= TextDecl? extSubsetDecl

(defstruct xml-header
  version
  encoding
  (standalone-p nil))

(defun p/ext-parsed-ent (input)
  ;; [78] extParsedEnt ::= '<?xml' VersionInfo? EncodingDecl S? '?>' content
  (when (eq (peek-token input) :xml-pi)
    (let ((hd (parse-xml-pi (cdr (nth-value 1 (peek-token input))) nil)))
      (setup-encoding input hd))
    (consume-token input) )
  (set-full-speed input)
  (p/content input))

(defun parse-xml-pi (content sd-ok-p)
  ;; --> xml-header
  ;;(make-xml-header))
  (let* ((res (make-xml-header))
         (i (make-rod-xstream content))
         (atts (read-attribute-list 'foo i t))) ;xxx on 'foo
    (unless (eq (peek-rune i) :eof)
      (error "Garbage at end of XML PI."))
    ;; versioninfo muss da sein
    ;; dann ? encodingdecl 
    ;; dann ? sddecl
    ;; dann ende
    (when (and (not (eq (caar atts) (intern-name '#.(string-rod "version"))))
               sd-ok-p)
      (error "XML PI needs version."))
    (when (eq (caar atts) (intern-name '#.(string-rod "version")))
      (unless (and (>= (length (cdar atts)) 1)
                   (every (lambda (x)
                            (or (<= #/a x #/z)
                                (<= #/A x #/Z)
                                (<= #/0 x #/9)
                                (rune= x #/_)
                                (rune= x #/.)
                                (rune= x #/:)
                                (rune= x #/-)))
                          (cdar atts)))
        (error "Bad XML version number: ~S." (rod-string (cdar atts))))
      (setf (xml-header-version res) (rod-string (cdar atts)))
      (pop atts))
    (when (eq (caar atts) (intern-name '#.(string-rod "encoding")))
      (unless (and (>= (length (cdar atts)) 1)
                   (every (lambda (x)
                            (or (<= #/a x #/z)
                                (<= #/A x #/Z)
                                (<= #/0 x #/9)
                                (rune= x #/_)
                                (rune= x #/.)
                                (rune= x #/-)))
                          (cdar atts))
                   ((lambda (x)
                      (or (<= #/a x #/z)
                          (<= #/A x #/Z)
                          (<= #/0 x #/9)))
                    (aref (cdar atts) 0)))
        (error "Bad XML encoding name: ~S." (rod-string (cdar atts))))
      (setf (xml-header-encoding res) (rod-string (cdar atts)))
      (pop atts))
    (when (and sd-ok-p (eq (caar atts) (intern-name '#.(string-rod "standalone"))))
      (unless (or (rod= (cdar atts) '#.(string-rod "yes"))
                  (rod= (cdar atts) '#.(string-rod "no")))
        (error "Hypersensitivity pitfall: ~
                XML PI's 'standalone' attribute must be exactly \"yes\" or \"no\" and not ~S."
               (rod-string (cdar atts))))
      (setf (xml-header-standalone-p res) 
        (if (rod-equal '#.(string-rod "yes") (cdar atts))
            :yes
          :no))
      (pop atts))
    (when atts
      (error "XML designers decided to disallow future extensions to the set ~
              of allowed XML PI's attributes -- you might have lost big on ~S (~S)"
             (rod-string content) sd-ok-p
             )) 
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
;;;;
;;;;  canonical XML according to James Clark
;;;;

;;;; User inteface ;;;;

(defun parse-file (filename &optional (handler (make-instance 'dom-impl::dom-builder)))
  (with-open-xstream (input filename)
    (setf (xstream-name input)
      (make-stream-name
       :entity-name "main document"
       :entity-kind :main
       :file-name filename))
    (let ((zstream (make-zstream :input-stack (list input))))
      (peek-rune input)
      (progn 'time
       (p/document zstream handler)))))

(defun parse-stream (stream &optional (handler (make-instance 'dom-impl::dom-builder)))
  (let* ((xstream 
          (make-xstream 
           stream
           :name (make-stream-name
                  :entity-name "main document"
                  :entity-kind :main
                  :file-name (or (ignore-errors (pathname *standard-output*))
                                 *default-pathname-defaults*))
           :initial-speed 1))
         (zstream (make-zstream :input-stack (list xstream))))
    (p/document zstream handler)))

(defun parse-string (string &optional (handler (make-instance 'dom-impl::dom-builder)))
  (let* ((x (string->xstream string))
         (z (make-zstream :input-stack (list x))))
    (p/document z handler)))

(defun string->xstream (string)
  (make-rod-xstream (string-rod string)))

;;;;

#+ALLEGRO
(defmacro sp (&body body)
  `(progn
     (prof:with-profiling (:type :space) .,body)
     (prof:show-flat-profile)))

#+ALLEGRO
(defmacro tm (&body body)
  `(progn
     (prof:with-profiling (:type :time) .,body)
     (prof:show-flat-profile)))

;;;;

(defun zstream-push (new-xstream zstream)
  (cond ((find-if (lambda (x)
                    (and (xstream-p x)
                         (eql (stream-name-entity-name (xstream-name x))
                              (stream-name-entity-name (xstream-name new-xstream)))
                         (eql (stream-name-entity-kind (xstream-name x))
                              (stream-name-entity-kind (xstream-name new-xstream)))))
                  (zstream-input-stack zstream))
         (error "Infinite recursion.")))
  (push new-xstream (zstream-input-stack zstream))
  zstream)

(defun recurse-on-entity (zstream name kind continuation)
  (assert (not (zstream-token-category zstream)))
  ;;(sleep .2)
  ;;(warn "~S / ~S[~S]." (zstream-input-stack zstream) (mu name) kind)
  (call-with-entity-expansion-as-stream
   zstream
   (lambda (new-xstream)
     (push :stop (zstream-input-stack zstream))
     (zstream-push new-xstream zstream)
     (prog1
         (funcall continuation zstream)
       (assert (eq (peek-token zstream) :eof))
       (assert (eq (pop (zstream-input-stack zstream)) new-xstream))
       (close-xstream new-xstream)
       (assert (eq (pop (zstream-input-stack zstream)) :stop))
       (setf (zstream-token-category zstream) nil)
       '(consume-token zstream)) )
   name kind))

(defun merge-sysid (sysid base)
  (merge-pathnames sysid base))

(defun open-sysid (sysid)
  (open sysid  :element-type '(unsigned-byte 8) :direction :input))


;;;;

(defparameter *test-files*
    '(;;"jclark:xmltest;not-wf;*;*.xml"
      "jclark:xmltest;valid;*;*.xml" 
      ;;"jclark:xmltest;invalid;*.xml"
      ))

(defun run-all-tests (&optional (test-files *test-files*))
  (let ((failed nil))
    (dolist (k test-files)
      (dolist (j (sort (directory k) #'string< :key #'pathname-name))
        (unless (test-file j)
          (push j failed))))
    (fresh-line)
    (cond (failed
           (write-string "**** Test failed on")
           (dolist (k failed)
             (format t "~%****  ~S." k))
           nil)
          (t
           (write-string "**** Test passed!")
           t))))

(defun test-file (filename)
  (let ((out-filename (merge-pathnames "out/" filename)))
    (if (probe-file out-filename)
        (positive-test-file filename out-filename)
      (negative-test-file filename))))

(defun positive-test-file (filename out-filename)
  (multiple-value-bind (nodes condition) 
      (ignore-errors (parse-file filename))
    (cond (condition
           (warn "**** Error in ~S: ~A." filename condition)
           nil)
          (t
           (let (res equal?)
             (setf res (with-output-to-string (sink)
                         (unparse-document nodes sink)))
             (setf equal?
               (with-open-file (in out-filename :direction :input :element-type 'character)
                 (do ((i 0 (+ i 1))
                      (c (read-char in nil nil) (read-char in nil nil)))
                     ((or (eq c nil) (= i (length res)))
                      (and (eq c nil) (= i (length res))))
                   (unless (eql c (char res i))
                     (return nil)))))
             (cond ((not equal?)
                    (format t "~&**** Test failed on ~S." filename)
                    (fresh-line)
                    (format t "** me: ~A" res)
                    (fresh-line)
                    (format t "** he: " res)
                    (finish-output)
                    (with-open-file (in out-filename :direction :input :element-type 'character)
                      (do ((c (read-char in nil nil) (read-char in nil nil)))
                          ((eq c nil))
                        (write-char c)))
                    nil)
                   (t
                    t)))))))

(defun negative-test-file (filename)
  (multiple-value-bind (nodes condition) 
      (ignore-errors (parse-file filename))
    (declare (ignore nodes))
    (cond (condition
           t)
          (t
           (warn "**** negative test failed on ~S." filename)))))

;;;;

(progn

  (defmethod dom:create-processing-instruction ((document null) target data)
    (declare (ignorable document target data))
    nil)

  (defmethod dom:append-child ((node null) child)
    (declare (ignorable node child))
    nil)

  (defmethod dom:create-element ((document null) name)
    (declare (ignorable document name))
    nil)

  (defmethod dom:set-attribute ((document null) name value)
    (declare (ignorable document name value))
    nil)

  (defmethod dom:create-text-node ((document null) data)
    (declare (ignorable document data))
    nil)

  (defmethod dom:create-cdata-section ((document null) data)
    (declare (ignorable document data))
    nil)
  )


;;; Implementation of a simple but faster DOM.

(defclass simple-document () 
  ((children :initform nil :accessor simple-document-children)))

(defstruct node 
  parent)

(defstruct (processing-instruction (:include node))
  target
  data)

(defstruct (text (:include node)
                 (:constructor make-text-boa (parent data)))
  data)

(defstruct (element (:include node))
  gi
  attributes
  children)

(defmethod dom:create-processing-instruction ((document simple-document) target data)
  (make-processing-instruction :target target :data data))

(defmethod dom:append-child ((node element) child)
  (setf (node-parent child) node)
  (push child (element-children node)))

(defmethod dom:append-child ((node simple-document) child)
  (push child (simple-document-children node))
  nil)

(defmethod dom:create-element ((document simple-document) name)
  (make-element :gi name))

(defmethod dom:set-attribute ((node element) name value)
  (push (cons name value)
        (element-attributes node)))

(defmethod dom:create-text-node ((document simple-document) data)
  (make-text-boa nil data))

(defmethod dom:create-cdata-section ((document simple-document) data)
  (make-text-boa nil data))

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
             ;; d.h. besser wäre hier auch while-reading zu benutzen.
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
             ;; we continue
             (setf rptr (%+ rptr 1))) ))
    ,@body )) 
||#

;(defun read-data-until (predicate input continuation)
;  )

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
    `(LET ((,input-var ,input))
       (MULTIPLE-VALUE-BIND (,res ,res-start ,res-end) 
           (WITH-RUNE-COLLECTOR/RAW (,collect)
             (LOOP
               (LET ((,c (PEEK-RUNE ,input-var)))
                 (COND ((EQ ,c :EOF) 
                        ;; xxx error message
                        (RETURN))
                       ((FUNCALL ,predicate ,c)
                        (RETURN))
                       (t
                        (,collect ,c)
                        (CONSUME-RUNE ,input-var))))))
         (LOCALLY
           ,@body)))))
  
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
                       (or (%= rune #/<) (%= rune #/&)))
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

(defun internal-entity-expansion (name)
  (let ((e (assoc (list :general name) *entities* :test #'equal)))
    (unless e
      (error "Entity '~A' is not defined." (rod-string name)))
    (unless (eq :internal (cadr e))
      (error "Entity '~A' is not an internal entity."))
    (or (cadddr e)
        (car
         (setf (cdddr e)
           (cons (find-internal-entity-expansion name) nil))))))

(defun find-internal-entity-expansion (name)
  (let ((zinput (make-zstream)))
    (with-rune-collector-3 (collect)
      (labels ((muffle (input)
                 (let (c)
                   (loop
                     (setf c (read-rune input))
                     (cond ((eq c :eof)
                            (return))
                           ((rune= c #/&)
                            (setf c (peek-rune input))
                            (cond ((rune= c #/#)
                                   (let ((c (read-numeric-entity input)))
                                     (%put-rune c collect)))
                                  (t
                                   (unless (name-start-rune-p (peek-rune input))
                                     (error "Expecting name after &."))
                                   (let ((name (read-name-token input)))
                                     (setf c (read-rune input))
                                     (assert (rune= c #/\;))
                                     (recurse-on-entity 
                                      zinput name :general
                                      (lambda (zinput)
                                        (muffle (car (zstream-input-stack zinput)))))))))
                           ((and (rune= c #/<))
                            ;; xxx fix error message
                            (cerror "Eat them in spite of this."
                                    "For no apparent reason #\/< is forbidden in attribute values. ~
                                     You lost -- next time choose SEXPR syntax.")
                            (collect c))
                           ((space-rune-p c)
                            (collect #/space))
                           ((not (data-rune-p c))
                            (error "illegal char: ~S." c))
                           (t
                            (collect c)))))))
        (declare (dynamic-extent #'muffle))
        (recurse-on-entity 
         zinput name :general
         (lambda (zinput)
           (muffle (car (zstream-input-stack zinput))))) ))))

#+(or) ;; Do we need this? Not called anywhere
(defun ff (name)
  (let ((input (make-zstream)))
    (let ((*data-behaviour* :doc)
          (*document* (make-instance 'simple-document)))
      (recurse-on-entity
       input name :general
       (lambda (input)
         (prog1
             (ecase (entity-source-kind name :general)
               (:internal (p/content input))
               (:external (p/ext-parsed-ent input)))
           (unless (eq (peek-token input) :eof)
             (error "Trailing garbage. - ~S" (peek-token input)))))))))

(defun read-att-value-2 (input)
  (let ((delim (read-rune input)))
    (unless (member delim '(#/\" #/\') :test #'eql)
      (error "Bad attribute value delimiter ~S, must be either #\\\" or #\\\'."
             (if (< delim char-code-limit) (code-char delim) delim)))
    (with-rune-collector-4 (collect)
      (loop
        (let ((c (read-rune input)))
          (cond ((eq c :eof)
                 (error "EOF"))
                ((rune= c delim)
                 (return))
                ((rune= #/& c)
                 (multiple-value-bind (kind sem) (read-entity-ref input)
                   (ecase kind
                     (:numeric
                      (%put-rune sem collect))
                     (:named
                      (let* ((exp (internal-entity-expansion sem))
                             (n (length exp)))
                        (declare (type (simple-array rune (*)) exp))
                        (do ((i 0 (%+ i 1)))
                            ((%= i n))
                          (collect (%rune exp i))))))))
                ((space-rune-p c)
                 (collect #x20))
                (t
                 (collect c))))))))

;;;;;;;;;;;;;;;;;

;;; Namespace stuff

(defvar *namespace-bindings* ())
(defvar *default-namespace-bindings*
  '((#"" . nil)
    (#"xmlns" . #"http://www.w3.org/2000/xmlns/")
    (#"xml" . #"http://www.w3.org/XML/1998/namespace")))
    
;; We already know that name is part of a valid XML name, so all we
;; have to check is that the first rune is a name-start-rune and that
;; there is not colon in it.
(defun nc-name-p (name)
  (and (name-start-rune-p (rune name 0))
       (notany #'(lambda (rune) (rune= #/: rune)) name)))

(defun split-qname (qname)
  (declare (type glisp:simple-rod qname))
  (let ((pos (position  #/: qname)))
    (if pos
	(let ((prefix (subseq qname 0 pos))
	      (local-name (subseq qname (1+ pos))))
	  (if (nc-name-p local-name)
	      (values prefix local-name)
	      (error "~S is not a valid NcName." local-name)))
	(values () qname))))
		 
(defun decode-qname (qname)
  "decode-qname name => namespace-uri, prefix, local-name"
  (declare (type glisp:simple-rod qname))
  (multiple-value-bind (prefix local-name) (split-qname qname)
    (let ((uri (find-namespace-binding prefix)))
      (if uri
	  (values uri prefix local-name)
	  (values nil nil nil)))))


(defun find-namespace-binding (prefix)
  (cdr (or (assoc prefix *namespace-bindings* :test #'rod=)
	   (error "Undeclared namespace prefix: ~A" (rod-string prefix)))))

;; FIXME: Should probably be refactored by adding :start and :end to rod=/rod-equal
(defun rod-starts-with (prefix rod)
  (and (<= (length prefix) (length rod))
       (dotimes (i (length prefix) t)
         (unless (rune= (rune prefix i) (rune rod i))
           (return nil)))))

(defun xmlns-attr-p (attr-name)
  (rod-starts-with #.(string-rod "xmlns") attr-name))

(defun attrname->prefix (attrname)
  (if (< 5 (length attrname))
      (subseq attrname 6)
      nil))

(defun find-namespace-declarations (attr-alist)
  (mapcar #'(lambda (attr)
	      (cons (attrname->prefix (car attr)) (cdr attr)))
	  (remove-if-not #'xmlns-attr-p attr-alist :key #'car)))

(defun declare-namespaces (attr-alist)
  (let ((ns-decls (find-namespace-declarations attr-alist)))
    (dolist (ns-decl ns-decls )
      ;; check some namespace validity constraints
      ;; FIXME: Would be nice to add "this is insane, go ahead" restarts
      (let ((prefix (car ns-decl))
	    (uri (if (rod= #"" (cdr ns-decl))
		     nil
		     (cdr ns-decl))))
	(cond
	  ((and (rod= prefix #"xml")
		(not (rod= uri #"http://www.w3.org/XML/1998/namespace")))
	   (error "Attempt to rebind the prefix \"xml\" to ~S." (mu uri)))
	  ((and (rod= uri #"http://www.w3.org/XML/1998/namespace")
		(not (rod= prefix #"xml")))
	   (error "The namespace URI \"http://www.w3.org/XML/1998/namespace\" ~
                   may not be bound to the prefix ~S, only \"xml\" is legal."
		  (mu prefix)))
	  ((and (rod= prefix #"xmlns")
		(rod= uri #"http://www.w3.org/2000/xmlns/"))
	   (error "Attempt to bind the prefix \"xmlns\" to its predefined ~
                   URI \"http://www.w3.org/2000/xmlns/\", which is ~
                   forbidden for no good reason."))
	  ((rod= prefix #"xmlns")
	   (error "Attempt to bind the prefix \"xmlns\" to the URI ~S, ~
                   but it may not be declared." (mu uri)))
	  ((rod= uri #"http://www.w3.org/2000/xmlns/")
	   (error "The namespace URI \"http://www.w3.org/2000/xmlns/\" may ~
                   not be bound to prefix ~S (or any other)." (mu prefix)))
	  ((and (rod= uri #"") prefix)
	   (error "Only the default namespace (the one without a prefix) may ~
                   be bound to an empty namespace URI, thus undeclaring it."))
	  (t
	   (push (cons prefix uri) *namespace-bindings*)
	   (sax:start-prefix-mapping *handler* (car ns-decl) (cdr ns-decl))))))
    ns-decls))

(defun undeclare-namespaces (ns-decls)
  (dolist (ns-decl ns-decls)
    (setq *namespace-bindings* (delete ns-decl *namespace-bindings*))
    (sax:end-prefix-mapping *handler* (car ns-decl))))

(defstruct attribute
  namespace-uri
  local-name
  qname
  value)

(defun build-attribute-list-no-ns (attr-alist)
  (mapcar #'(lambda (pair) (make-attribute :qname (car pair) :value (cdr pair)))
	  attr-alist))

;; FIXME: Use a non-braindead way to enforce attribute uniqueness
(defun build-attribute-list-ns (attr-alist)
  (let (attributes)
    (dolist (pair attr-alist)
      (when (or (not (xmlns-attr-p (car pair)))
		sax:*include-xmlns-attributes*)
	(push (build-attribute (car pair) (cdr pair)) attributes)))
    
    ;; 5.3 Uniqueness of Attributes
    ;; In XML documents conforming to [the xmlns] specification, no
    ;; tag may contain two attributes which:
    ;; 1. have identical names, or
    ;; 2. have qualified names with the same local part and with
    ;; prefixes which have been bound to namespace names that are
    ;; identical.
    ;;
    ;; 1. is checked by read-tag-2, so we only deal with 2 here
    (do ((sublist attributes (cdr sublist)))
	((null sublist) attributes)
      (let ((attr-1 (car sublist)))
	(when (and (attribute-namespace-uri attr-1)
		   (find-if #'(lambda (attr-2)
				(and (rod= (attribute-namespace-uri attr-1)
					   (attribute-namespace-uri attr-2))
				     (rod= (attribute-local-name attr-1)
					   (attribute-local-name attr-2))))
		       (cdr sublist)))
	  (error "Multiple definitions of attribute ~S in namespace ~S."
		 (mu (attribute-local-name attr-1))
		 (mu (attribute-namespace-uri attr-1))))))))
    
(defun build-attribute (name value)
  (multiple-value-bind (prefix local-name) (split-qname name)
    (declare (ignorable local-name))
    (if (or (not prefix) ;; default namespace doesn't apply to attributes
	    (and (rod= #"xmlns" prefix) (not sax:*use-xmlns-namespace*)))
	(make-attribute :qname name :value value)
	(multiple-value-bind (uri prefix local-name)
	    (decode-qname name)
	  (declare (ignore prefix))
	  (make-attribute :qname name
			  :value value
			  :namespace-uri uri
			  :local-name local-name)))))
    
;;; Faster constructors

;; Since using the general DOM interface to construct the parsed trees
;; may turn out to be quite expensive (That depends on the underlying
;; DOM implementation). A particular DOM implementation may choose to
;; implement an XML:FAST-CONSTRUCTORS method:

;; XML:FAST-CONSTRUCTORS document                               [method]
;;
;; Return an alist of constructors suitable for the document `document'.
;;
;;  (:MAKE-TEXT document parent data)
;;  (:MAKE-PROCESSING-INSTRUCTION document parent target content)
;;  (:MAKE-NODE document parent attributes content)
;;  [`attributes' now in turn is an alist]
;;  (:MAKE-CDATA document parent data)
;;  (:MAKE-COMMENT document parent data)
;;

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

;; xstream Controller Protocol
;;
;; 


#||
(defun xml-parse (system-id &key document standalone-p)
  )
||#
