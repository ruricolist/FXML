(in-package :encoding)

;;;; ---------------------------------------------------------------------------
;;;; Encoding names
;;;;

(defvar *names* (make-hash-table :test #'eq))

(defun canon-name (string)
  (with-output-to-string (bag)
    (map nil (lambda (ch)
               (cond ((char= ch #\_) (write-char #\- bag))
                     (t (write-char (char-upcase ch) bag))))
         string)))

(defun canon-name-2 (string)
  (with-output-to-string (bag)
    (map nil (lambda (ch)
               (cond ((char= ch #\_))
                     ((char= ch #\-))
                     (t (write-char (char-upcase ch) bag))))
         string)))

(defmethod encoding-names ((encoding symbol))
  (gethash encoding *names*))

(defmethod (setf encoding-names) (new-value (encoding symbol))
  (setf (gethash encoding *names*) new-value))

(defun add-name (encoding name)
  (pushnew (canon-name name) (encoding-names encoding) :test #'string=))

(defun resolve-name (string)
  (cond ((symbolp string)
         string)
        (t
         (setq string (canon-name string))
         (or
          (block nil
            (maphash (lambda (x y) 
                       (when (member string y :test #'string=)
                         (return x)))
                     *names*)
            nil)
          (block nil
            (maphash (lambda (x y) 
                       (when (member string y 
                                     :test #'(lambda (x y)
                                               (string= (canon-name-2 x) 
                                                        (canon-name-2 y))))
                         (return x)))
                     *names*)
            nil)))))

;;;; ---------------------------------------------------------------------------
;;;;  Encodings
;;;;

(defvar *encodings* (make-hash-table :test #'eq))

(defmacro define-encoding (name init-form)
  `(progn
     (setf (gethash ',name *encodings*)
       (list nil (lambda () ,init-form)))
     ',name))

(defun find-encoding (name)
  (let ((x (gethash (resolve-name name) *encodings*)))
    (and x
         (or (first x)
             (setf (first x) (funcall (second x)))))))

(defclass encoding () ())

(defclass simple-8-bit-encoding (encoding)
  ((table :initarg :table)))

(defun make-simple-8-bit-encoding (&key charset)
  (make-instance 'simple-8-bit-encoding
    :table (coerce (to-unicode-table charset) '(simple-array (unsigned-byte 16) (256)))))

;;;;;;;

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

(defmethod decode-sequence ((encoding (eql :utf-16-big-endian))
                            in in-start in-end out out-start out-end eof?)
  ;; -> new wptr, new rptr
  (let ((wptr out-start)
        (rptr in-start))
    (loop
      (when (%= wptr out-end)
        (return))
      (when (>= (%+ rptr 1) in-end)
        (return))
      (let ((hi (aref in rptr))
            (lo (aref in (%+ 1 rptr))))
        (setf rptr (%+ 2 rptr))
        (setf (aref out wptr) (logior (ash hi 8) lo))
        (setf wptr (%+ 1 wptr))))
    (values wptr rptr)))

(defmethod decode-sequence ((encoding (eql :utf-16-little-endian))
                            in in-start in-end out out-start out-end eof?)
  ;; -> new wptr, new rptr
  (let ((wptr out-start)
        (rptr in-start))
    (loop
      (when (%= wptr out-end)
        (return))
      (when (>= (%+ rptr 1) in-end)
        (return))
      (let ((lo (aref in (%+ 0 rptr)))
            (hi (aref in (%+ 1 rptr))))
        (setf rptr (%+ 2 rptr))
        (setf (aref out wptr) (logior (ash hi 8) lo))
        (setf wptr (%+ 1 wptr))))
    (values wptr rptr)))

(defmethod decode-sequence ((encoding (eql :utf-8))
                            in in-start in-end out out-start out-end eof?)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) in)
           (type (simple-array (unsigned-byte 16) (*)) out)
           (type fixnum in-start in-end out-start out-end))
  (let ((wptr out-start)
        (rptr in-start)
        byte0)
    (macrolet ((put (x)
                 `((lambda (x)
                     (cond ((or (<= #xD800 x #xDBFF)
                                (<= #xDC00 x #xDFFF))
                            (error "Encoding UTF-16 in UTF-8? : #x~x." x)))
                     '(unless (data-char-p x)
                       (error "#x~x is not a data character." x))
                     ;;(fresh-line)
                     ;;(prin1 x) (princ "-> ")
                     (cond ((%> x #xFFFF)
                            (setf (aref out (%+ 0 wptr)) (%+ #xD7C0 (ash x -10))
                                  (aref out (%+ 1 wptr)) (%ior #xDC00 (%and x #x3FF)))
                            (setf wptr (%+ wptr 2)))
                           (t
                            (setf (aref out wptr) x)
                            (setf wptr (%+ wptr 1)))))
                   ,x))
               (put1 (x)
                 `(progn
                    (setf (aref out wptr) ,x)
                    (setf wptr (%+ wptr 1)))))
      (loop
        (when (%= (+ wptr 1) out-end) (return))
        (when (%>= rptr in-end) (return))
        (setq byte0 (aref in rptr))
        (cond ((= byte0 #x0D)
               ;; CR handling
               ;; we need to know the following character
               (cond ((>= (%+ rptr 1) in-end)
                      ;; no characters in buffer
                      (cond (eof?
                             ;; at EOF, pass it as NL
                             (put #x0A)
                             (setf rptr (%+ rptr 1)))
                            (t
                             ;; demand more characters
                             (return))))
                     ((= (aref in (%+ rptr 1)) #x0A)
                      ;; we see CR NL, so forget this CR and the next NL will be
                      ;; inserted literally
                      (setf rptr (%+ rptr 1)))
                     (t
                      ;; singleton CR, pass it as NL
                      (put #x0A)
                      (setf rptr (%+ rptr 1)))))
                    
              ((%<= #|#b00000000|# byte0 #b01111111)
               (put1 byte0)
               (setf rptr (%+ rptr 1)))
            
              ((%<= #|#b10000000|# byte0 #b10111111)
               (error "Corrupted UTF-8 input (initial byte was #b~8,'0B)" byte0)
               (setf rptr (%+ rptr 1)))
            
              ((%<= #|#b11000000|# byte0 #b11011111)
               (cond ((< (%+ rptr 2) in-end)
                      (put
                       (dpb (ldb (byte 5 0) byte0) (byte 5 6)
                            (dpb (ldb (byte 6 0) (aref in (%+ rptr 1))) (byte 6 0)
                                 0)))
                      (setf rptr (%+ rptr 2)))
                     (t
                      (return))))
            
              ((%<= #|#b11100000|# byte0 #b11101111)
               (cond ((< (%+ rptr 3) in-end)
                      (put
                       (dpb (ldb (byte 4 0) byte0) (byte 4 12)
                            (dpb (ldb (byte 6 0) (aref in (%+ 1 rptr))) (byte 6 6)
                                 (dpb (ldb (byte 6 0) (aref in (%+ 2 rptr))) (byte 6 0)
                                      0))))
                      (setf rptr (%+ rptr 3)))
                     (t
                      (return))))
            
              ((%<= #|#b11110000|# byte0 #b11110111)
               (cond ((< (%+ rptr 4) in-end)
                      (put
                       (dpb (ldb (byte 3 0) byte0) (byte 3 18)
                            (dpb (ldb (byte 6 0) (aref in (%+ 1 rptr))) (byte 6 12)
                                 (dpb (ldb (byte 6 0) (aref in (%+ 2 rptr))) (byte 6 6)
                                      (dpb (ldb (byte 6 0) (aref in (%+ 3 rptr))) (byte 6 0)
                                           0)))))
                      (setf rptr (%+ rptr 4)))
                     (t
                      (return))))
            
              ((%<= #|#b11111000|# byte0 #b11111011)
               (cond ((< (%+ rptr 5) in-end)
                      (put
                       (dpb (ldb (byte 2 0) byte0) (byte 2 24)
                            (dpb (ldb (byte 6 0) (aref in (%+ 1 rptr))) (byte 6 18)
                                 (dpb (ldb (byte 6 0) (aref in (%+ 2 rptr))) (byte 6 12)
                                      (dpb (ldb (byte 6 0) (aref in (%+ 3 rptr))) (byte 6 6)
                                           (dpb (ldb (byte 6 0) (aref in (%+ 4 rptr))) (byte 6 0)
                                                0))))))
                      (setf rptr (%+ rptr 5)))
                     (t
                      (return))))
            
              ((%<= #|#b11111100|# byte0 #b11111101)
               (cond ((< (%+ rptr 6) in-end)
                      (put
                       (dpb (ldb (byte 1 0) byte0) (byte 1 30)
                            (dpb (ldb (byte 6 0) (aref in (%+ 1 rptr))) (byte 6 24)
                                 (dpb (ldb (byte 6 0) (aref in (%+ 2 rptr))) (byte 6 18)
                                      (dpb (ldb (byte 6 0) (aref in (%+ 3 rptr))) (byte 6 12)
                                           (dpb (ldb (byte 6 0) (aref in (%+ 4 rptr))) (byte 6 6)
                                                (dpb (ldb (byte 6 0) (aref in (%+ 5 rptr))) (byte 6 0)
                                                     0)))))))
                      (setf rptr (%+ rptr 6)))
                     (t
                      (return))))
            
              (t
               (error "Corrupted UTF-8 input (initial byte was #b~8,'0B)" byte0)) ) )) 
    (values wptr rptr))  )

(defmethod encoding-p ((object (eql :utf-16-little-endian))) t)
(defmethod encoding-p ((object (eql :utf-16-big-endian))) t)
(defmethod encoding-p ((object (eql :utf-8))) t)

(defmethod encoding-p ((object encoding)) t)

(defmethod decode-sequence ((encoding simple-8-bit-encoding)
                            in in-start in-end
                            out out-start out-end 
                            eof?)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) in)
           (type (simple-array (unsigned-byte 16) (*)) out)
           (type fixnum in-start in-end out-start out-end))
  (let ((wptr out-start)
        (rptr in-start)
        (byte 0)
        (table (slot-value encoding 'table))) 
    (declare (type fixnum wptr rptr)
             (type (unsigned-byte 8) byte)
             (type (simple-array (unsigned-byte 16) (*)) table))
    (loop
      (when (%= wptr out-end) (return))
      (when (%>= rptr in-end) (return))
      (setq byte (aref in rptr))
      (cond ((= byte #x0D)
             ;; CR handling
             ;; we need to know the following character
             (cond ((>= (%+ rptr 1) in-end)
                    ;; no characters in buffer
                    (cond (eof?
                           ;; at EOF, pass it as NL
                           (setf (aref out wptr) #x0A)
                           (setf wptr (%+ wptr 1))
                           (setf rptr (%+ rptr 1)))
                          (t
                           ;; demand more characters
                           (return))))
                   ((= (aref in (%+ rptr 1)) #x0A)
                    ;; we see CR NL, so forget this CR and the next NL will be
                    ;; inserted literally
                    (setf rptr (%+ rptr 1)))
                   (t
                    ;; singleton CR, pass it as NL
                    (setf (aref out wptr) #x0A)
                    (setf wptr (%+ wptr 1))
                    (setf rptr (%+ rptr 1)))))
                    
            (t
             (setf (aref out wptr) (aref table byte))
             (setf wptr (%+ wptr 1))
             (setf rptr (%+ rptr 1))) ))
    (values wptr rptr)))

;;;; ---------------------------------------------------------------------------
;;;;  Character sets
;;;;

(defvar *charsets* (make-hash-table :test #'eq))

(defclass 8-bit-charset ()
  ((name :initarg :name)
   (to-unicode-table 
    :initarg :to-unicode-table
    :reader to-unicode-table)))

(defmacro define-8-bit-charset (name &rest codes)
  (assert (= 256 (length codes)))
  `(progn
     (setf (gethash ',name *charsets*)
         (make-instance '8-bit-charset
           :name ',name
           :to-unicode-table
           ',(make-array 256 
                         :element-type '(unsigned-byte 16)
                         :initial-contents codes)))
     ',name))

(defun find-charset (name)
  (or (gethash name *charsets*)
      (error "There is no character set named ~S." name)))

