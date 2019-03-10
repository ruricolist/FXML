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
                     :stream (make-string-output-stream)))

(defun omit-whitespace? (sink)
  (not (keep-whitespace? sink)))

(defun make-html5-sink (&rest args &key keep-comments keep-whitespace stream close)
  "Create an HTML5 sink.
The arguments are KEEP-COMMENTS (default `nil'),
KEEP-WHITESPACE (default `t'), STREAM, and CLOSE.

STREAM can be used to supply a string stream to write to. The stream
will be closed when serialization is finished. If you want it left
open, pass `:close nil`.

The stream is returned as a second value."
  (declare (ignore keep-comments keep-whitespace stream close))
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

(defmethods html5-sink (self stream meta-blocked)
  (:method close-sink (self)
    (prog1 (get-output-stream-string stream)
      (close stream)))
  (:method fxml.sax:end-document (self)
    (when (must-close? self)
      (close-sink self)))
  (:method fxml.sax:start-dtd (self name public-id system-id)
    (declare (ignore name public-id system-id))
    (write-string "<!doctype html>" stream))
  (:method fxml.sax:characters (self (text string))
    (when (and (omit-whitespace? self) (blankp text))
      (return-from fxml.sax:characters))
    (escape text text-escapes :stream stream))
  (:method fxml.sax:comment (self text)
    (when (keep-comments? self)
      (format stream "<!-- ~a -->" text)))

  (:method fxml.sax:start-element (self ns lname qname attrs)
    (unless lname
      (setf lname (nth-value 1 (fxml:split-qname qname))))
    ;; Suppress charset declarations from the original document.
    (when (string= lname "meta")
      (when (some #'charset-declaration? attrs)
        (setf meta-blocked t)
        (return-from fxml.sax:start-element)))
    (let ((*print-pretty*))
      (cond ((no attrs)
             (format stream "<~a>" lname))
            (t (format stream "<~a" lname)
               (dolist (attr attrs)
                 (let ((name (fxml.sax:attribute-qname attr))
                       (value (fxml.sax:attribute-value attr)))
                   (unless (string^= "xmlns" name)
                     (format stream " ~a=\"" name)
                     (escape value attribute-escapes :stream stream)
                     (write-char #\" stream))))
               (write-char #\> stream))))
    ;; Force a UTF-8 charset declaration where it belongs, at the top of
    ;; HEAD.
    (when (string= lname "head")
      (fxml.sax:start-element self ns "meta" "meta" meta-charset)
      (fxml.sax:end-element self ns "meta" "meta")))

  (:method fxml.sax:end-element (self ns lname qname)
    (declare (ignore ns))
    (unless lname
      (setf lname (nth-value 1 (fxml:split-qname qname))))
    ;; Finish suppressing the meta.
    (when (and (string= lname "meta")
               meta-blocked)
      (setf meta-blocked nil)
      (return-from fxml.sax:end-element))
    (unless (string-case lname
              (("br"
                "hr" "area" "base" "col" "command"
                "embed" "img" "input" "keygen" "link" "param"
                "source" "track" "wbr" "nobr")
               t))
      (let (*print-pretty*)
        (format stream "</~a>" lname)))))

(defun charset-declaration? (attr)
  "Is ATTR a charset declaration from the original document?"
  (and (not (fxml.sax:attribute-specified-p attr))
       (let ((lname (fxml.sax:attribute-local-name attr)))
         (or (string= lname "charset")
             (and (string= lname "http-equiv")
                  (string-equal (fxml.sax:attribute-value attr)
                                "content-type"))))))
