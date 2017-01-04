;;;; sax-sanitize.lisp

(in-package #:fxml.sanitize)

;;; "sax-sanitize" goes here. Hacks and glory await!

(defconst pass nil)

(deftype protocol ()
  '(member :http :https :ftp :mailto :relative))

(def html-ns "http://www.w3.org/1999/xhtml"
  "The XHTML namespace.")

(def default-whitespace-elements
  '("address" "article" "aside" "blockquote" "br" "dd" "div" "dl"
    "dt" "footer" "h1" "h2" "h3" "h4" "h5" "h6" "header" "hgroup"
    "hr" "li" "nav" "ol" "p" "pre" "section" "ul")
  "Replace these elements with whitespace.")

(defstruct-read-only (element-rule (:conc-name element.))
  "How to sanitize an element."
  (allowed nil :type boolean)
  (removed nil :type boolean)
  (whitespace nil :type boolean)
  (attributes '() :type list)
  (protocols '() :type list)
  (add-attributes '() :type list))

(def null-rule (make-element-rule)
  "Rule for unknown elements.")

(defclass text-only-sanitizer (fxml.sax:default-handler)
  ((chained-handler :initarg :chained-handler :reader chained-handler))
  (:documentation "Trivial sanitizer for extracting the text."))

(defmethod fxml.sax:characters ((self text-only-sanitizer) data)
  (fxml.sax:characters (chained-handler self) data))

(defmethod fxml.sax:start-document ((self text-only-sanitizer))
  (fxml.sax:start-document (chained-handler self)))

(defmethod fxml.sax:end-document ((self text-only-sanitizer))
  (fxml.sax:end-document (chained-handler self)))

(defclass sanitizer (fxml:sax-proxy)
  ((rules :type hash-table :accessor sanitizer.rules)
   (removed :initform 0 :type fixnum :accessor sanitizer.removed-count)
   (stack :initform '() :type list :accessor sanitizer.stack)
   (allow-comments :accessor sanitizer.allow-comments?)
   (allow-data-attributes :accessor sanitizer.allow-data-attributes?))
  (:documentation "The state of a sanitizer."))

(defmethod initialize-instance :after ((self sanitizer) &key mode)
  (setf (sanitizer.rules self) (mode.rules mode)
        (sanitizer.allow-comments? self) (mode.allow-comments? mode)
        (sanitizer.allow-data-attributes? self) (mode.allow-data-attributes? mode)))

(defclass sanitizer/comments (sanitizer)
  ()
  (:documentation "A sanitizer that allows comments."))

(defclass sanitizer/no-comments (sanitizer)
  ()
  (:documentation "A sanitizer that removes comments."))

(defclass base-mode () ())

(defclass mode (base-mode)
  ((name :initarg :name :type symbol :accessor mode.name)
   (rules :initarg :rules :type hash-table :accessor mode.rules)
   (allow-comments :initarg :allow-comments :accessor mode.allow-comments?)
   (allow-data-attributes :initarg :allow-data-attributes :accessor mode.allow-data-attributes?))
  (:default-initargs
   :allow-comments nil
   :allow-data-attributes nil
   :rules (make-hash-table :test 'equalp)
   :whitespace-elements default-whitespace-elements)
  (:documentation "Definition of a sanitizer (immutable)."))

(defclass text-only-mode (mode) ())

(defmethod print-object ((self mode) stream)
  (print-unreadable-object (self stream :type t)
    (princ (mode.name self) stream)))

(defmethod print-object ((self text-only-mode) stream)
  (print-unreadable-object (self stream :type t :identity t)))

(defun attrs->sax (attrs)
  "Convert ATTRS, an alist of (name . value), to SAX attributes."
  (loop for (name . value) in attrs
        collect (fxml.sax:make-attribute
                 :namespace-uri html-ns
                 :local-name name
                 :qname name
                 :value value
                 :specified-p t)))

(defmethod initialize-instance :after ((self mode)
                                       &key elements remove-elements
                                            whitespace-elements
                                            attributes add-attributes protocols
                                       &allow-other-keys)
  "Build a set of rules from the arguments."
  (let ((all-elements (nub (append elements
                                   remove-elements
                                   whitespace-elements))))
    (dolist (element all-elements)
      (let ((ok (string-member element elements))
            (removed (string-member element remove-elements))
            (ws (string-member element whitespace-elements))
            (attrs (append (cdr (assoc-string element attributes))
                           (cdr (assoc :all attributes))))
            (protocols (cdr (assoc-string element protocols)))
            (add-attrs (cdr (assoc-string element add-attributes))))
        (setf (gethash element (mode.rules self))
              (make-element-rule :allowed (and ok t)
                                 :removed (and removed t)
                                 :whitespace (and ws t)
                                 :attributes attrs
                                 :protocols protocols
                                 :add-attributes (attrs->sax add-attrs)))))))

(defun tag-rule (self tag)
  "Lookup the rule for TAG in SELF."
  (gethash tag (sanitizer.rules self) null-rule))

(defun string-member (string list)
  "Like `member' with `string-equal'."
  (loop for tail on list
        when (string-equal string (car tail))
          return tail))

(defun assoc-string (string alist)
  "Like `assoc' with `string-equal'."
  (assoc string alist :test #'string-equal))

(defun uri-protocol (uri)
  (multiple-value-bind (data start end got-scheme)
      (quri:parse-scheme uri)
    (if data 
        (let ((scheme (or got-scheme (nsubseq uri start end))))
          (if scheme
              (string-case scheme
                ("http" :http)
                ("https" :https)
                ("ftp" :ftp)
                ("mailto" :mailto)
                (otherwise :relative))
              :relative))
        :relative)))

(defmethod fxml.sax:characters ((self sanitizer) data)
  (declare (ignore data))
  (unless (plusp (sanitizer.removed-count self))
    (when-let (parent (car (sanitizer.stack self)))
      (incf (cdr parent)))
    (call-next-method)))

(defmethod fxml.sax:comment ((self sanitizer) data)
  (declare (ignore data))
  (when (sanitizer.allow-comments? self)
    (unless (plusp (sanitizer.removed-count self))
      (call-next-method))))

(defun check-protocol (value protocols)
  "Check VALUE, a URI, against a list of allowed protocols."
  (and value
       (or (no protocols)
           (member (uri-protocol value) protocols))))

(defun attribute-ok? (self rule name value)
  (or (and (sanitizer.allow-data-attributes? self)
           (string^= "data-" name))
      (and (string-member name (element.attributes rule))
           (let ((protocols (assoc-string name (element.protocols rule))))
             (check-protocol value protocols)))))

(defmethod fxml.sax:start-element ((self sanitizer) ns lname qname attrs)
  (when-let (parent (car (sanitizer.stack self)))
    (incf (cdr parent)))
  (let ((rule (tag-rule self lname)))
    (push (cons rule 0) (sanitizer.stack self))
    (cond ((element.removed rule)
           (incf (sanitizer.removed-count self)))
          ((plusp (sanitizer.removed-count self)))
          ((not (element.allowed rule))
           (when (element.whitespace rule)
             (fxml.sax:characters self " ")))
          (t (let ((attrs-out
                     (append (element.add-attributes rule)
                             (filter
                              (lambda (attr)
                                (let ((name (fxml.sax:attribute-local-name attr))
                                      (value (fxml.sax:attribute-value attr)))
                                  (attribute-ok? self rule name value)))
                              attrs))))
               (call-next-method self ns lname qname attrs-out))))))

(defmethod fxml.sax:end-element ((self sanitizer) ns lname qname)
  (declare (ignore lname ns qname))
  (destructuring-bind (rule . child-count)
      (pop (sanitizer.stack self))
    (cond ((element.removed rule)
           (decf (sanitizer.removed-count self)))
          ((plusp (sanitizer.removed-count self))
           pass)
          ((element.allowed rule)
           (call-next-method))
          ((element.whitespace rule)
           (when (plusp child-count)
             (fxml.sax:characters self " " ))))))

(defmacro define-sanitize-mode (name &key allow-comments allow-data-attributes
                                          add-attributes attributes elements
                                          protocols whitespace-elements remove-elements)
  `(def ,name
     (make 'mode
           :name ',name
           :allow-comments ,allow-comments
           :allow-data-attributes ,allow-data-attributes
           :add-attributes ',add-attributes
           :attributes ',attributes
           :elements ',elements
           :protocols ',protocols
           ,@(unsplice (and whitespace-elements
                            `(:whitespace-elements ',whitespace-elements)))
           :remove-elements ',remove-elements)))

(def default (make 'text-only-mode))

(define-sanitize-mode restricted
  :elements ("b" "em" "i" "strong" "u"))

(define-sanitize-mode basic
  :elements ("a" "abbr" "b" "blockquote" "br" "cite" "code" "dd" "dfn" "dl" "dt" "em" "i"
                 "kbd" "li" "mark" "ol" "p" "pre" "q" "s" "samp" "small" "strike" "strong"
                 "sub" "sup" "time" "u" "ul" "var")

  :attributes (("a"          . ("href"))
               ("abbr"       . ("title"))
               ("blockquote" . ("cite"))
               ("dfn"        . ("title"))
               ("q"          . ("cite"))
               ("time"       . ("datetime" "pubdate")))

  :add-attributes (("a" . (("rel" . "nofollow"))))

  :protocols (("a"           . (("href" . (:ftp :http :https :mailto :relative))))
              ("blockquote"  . (("cite" . (:http :https :relative))))
              ("q"           . (("cite" . (:http :https :relative))))))

(define-sanitize-mode relaxed
  :elements ("a" "abbr" "b" "bdo" "blockquote" "br" "caption" "cite" "code" "col"
                 "colgroup" "dd" "del" "dfn" "dl" "dt" "em" "figcaption" "figure" "h1" "h2"
                 "h3" "h4" "h5" "h6" "hgroup" "i" "img" "ins" "kbd" "li" "mark" "ol" "p" "pre"
                 "q" "rp" "rt" "ruby" "s" "samp" "small" "strike" "strong" "sub" "sup" "table"
                 "tbody" "td" "tfoot" "th" "thead" "time" "tr" "u" "ul" "var" "wbr" "font")

  :attributes ((:all         . ("dir" "lang" "title" "class"))
               ("a"          . ("href"))
               ("blockquote" . ("cite"))
               ("col"        . ("span" "width"))
               ("colgroup"   . ("span" "width"))
               ("del"        . ("cite" "datetime"))
               ("img"        . ("align" "alt" "height" "src" "width"))
               ("ins"        . ("cite" "datetime"))
               ("ol"         . ("start" "reversed" "type"))
               ("q"          . ("cite"))
               ("table"      . ("summary" "width"))
               ("td"         . ("abbr" "axis" "colspan" "rowspan" "width"))
               ("th"         . ("abbr" "axis" "colspan" "rowspan" "scope" "width"))
               ("time"       . ("datetime" "pubdate"))
               ("ul"         . ("type"))
               ("font"       . ("size")))

  :protocols (("a"           . (("href" . (:ftp :http :https :mailto :relative))))
              ("blockquote"  . (("cite" . (:http :https :relative))))
              ("del"         . (("cite" . (:http :https :relative))))
              ("img"         . (("src"  . (:http :https :relative))))
              ("ins"         . (("cite" . (:http :https :relative))))
              ("q"           . (("cite" . (:http :https :relative))))))

(defun wrap-sanitize (chained-handler &optional (mode default))
  (cond ((null mode) chained-handler)
        ((eq mode default)
         (make 'text-only-sanitizer :chained-handler chained-handler))
        (t (make 'sanitizer
                 :chained-handler chained-handler
                 :mode mode))))
