(in-package #:fxml.html5)

(def meta-charset
  (list (fxml.sax:make-attribute :namespace-uri xhtml-ns
                                 :local-name "charset"
                                 :qname "charset"
                                 :value "UTF-8"
                                 :specified-p t)))

(defclass html5-sink (fxml.sax:default-handler)
  ((stream :initarg :stream :accessor stream-of)
   (comments :initarg :keep-comments :accessor keep-comments?)
   (meta-blocked :initform nil :accessor meta-blocked?)
   (keep-whitespace :initarg :keep-whitespace :accessor keep-whitespace?)
   (close :initarg :close :accessor must-close?))
  (:default-initargs :keep-whitespace t
                     :keep-comments nil
                     :close t
                     :stream  (make-string-output-stream)))

(defun omit-whitespace? (sink)
  (not (keep-whitespace? sink)))

(defun make-html5-sink (&rest args)
  "Create an HTML5 sink.
The arguments are KEEP-COMMENTS (default `nil'),
KEEP-WHITESPACE (default `t'), STREAM, and CLOSE.

STREAM can be used to supply a string stream to write to. The stream
will be closed when serialization is finished. If you want it left
open, pass `:close nil`.

The stream is returned as a second value."
  (apply #'make 'html5-sink args))

(def text-escapes
  (lambda (c)
    (case c
      (#\& "&amp;")
      (#\No-break_space "&nbsp;")
      (#\< "&lt;")
      (#\> "&gt")))
  "Characters to escape in text.")

(def attribute-escapes
  (lambda (c)
    (case c
      (#\& "&amp;")
      (#\No-break_space "&nbsp;")
      (#\" "&quot;")))
  "Characters to escape in attributes.")

(defmethod close-sink ((self html5-sink))
  (prog1 (get-output-stream-string (stream-of self))
    (close (stream-of self))))

(defmethod fxml.sax:end-document ((self html5-sink))
  (when (must-close? self)
    (close-sink self)))

(defmethod fxml.sax:start-dtd ((self html5-sink) name public-id system-id)
  (declare (ignore name public-id system-id))
  (write-string "<!doctype html>" (stream-of self)))

(defmethod fxml.sax:characters ((self html5-sink) text)
  (when (and (omit-whitespace? self) (blankp text))
    (return-from fxml.sax:characters))
  (escape text text-escapes :stream (stream-of self)))

(defmethod fxml.sax:comment ((self html5-sink) text)
  (when (keep-comments? self)
    (format (stream-of self) "<!-- ~a -->" text)))

(defun charset-declaration? (attr)
  "Is ATTR a charset declaration from the original document?"
  (and (not (fxml.sax:attribute-specified-p attr))
       (let ((lname (fxml.sax:attribute-local-name attr)))
         (or (string= lname "charset")
             (and (string= lname "http-equiv")
                  (string-equal (fxml.sax:attribute-value attr)
                                "content-type"))))))

(defmethod fxml.sax:start-element ((self html5-sink) ns lname qname attrs)
  (declare (ignore qname))
  ;; Suppress charset declarations from the original document.
  (when (string= lname "meta")
    (when (some #'charset-declaration? attrs)
      (setf (meta-blocked? self) t)
      (return-from fxml.sax:start-element)))
  (let ((*print-pretty*))
    (cond ((no attrs)
           (format (stream-of self) "<~a>" lname))
          (t (format (stream-of self) "<~a" lname)
             (dolist (attr attrs)
               (let ((name (fxml.sax:attribute-qname attr))
                     (value (fxml.sax:attribute-value attr)))
                 (unless (string^= "xmlns" name)
                   (format (stream-of self) " ~a=\"" name)
                   (escape value attribute-escapes :stream (stream-of self))
                   (write-char #\" (stream-of self)))))
             (write-char #\> (stream-of self)))))
  ;; Force a UTF-8 charset declaration where it belongs, at the top of
  ;; HEAD.
  (when (string= lname "head")
    (fxml.sax:start-element self ns "meta" "meta" meta-charset)
    (fxml.sax:end-element self ns "meta" "meta")))

(defmethod fxml.sax:end-element ((self html5-sink) ns lname qname)
  (declare (ignore ns qname))
  ;; Finish suppressing the meta.
  (when (and (string= lname "meta")
             (meta-blocked? self))
    (setf (meta-blocked? self) nil)
    (return-from fxml.sax:end-element))
  (let (*print-pretty*)
    (format (stream-of self) "</~a>" lname)))
