(in-package #:fxml.sanitize)

(defpackage #:fxml.sanitize.test
  (:use #:cl #:alexandria #:serapeum
        #:fxml.sanitize #:fiveam)
  (:export #:run-tests))

(in-package #:sax-sanitize.test)

(def-suite sanitize :description "Sanitize tests suite")

(defun run-tests ()
  (run! 'sanitize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; simple
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite simple :in sanitize)

(defun clean (string &optional (mode default))
  ;; Collapse whitespace.
  (~> string
      html5-parser:parse-html5
      (html5-sax:serialize-dom (wrap-sanitize (html5-sax:make-html5-sink)
                                              mode))
      (ppcre:regex-replace-all "\\s+" _ " ")))

;;; basic

(defparameter *basic-html*
  "<b>Lo<!-- comment -->rem</b> <a href=\"pants\" title=\"foo\">ipsum</a> <a href=\"http://foo.com/\"><strong>dolor</strong></a> sit<br>amet <script>alert(\"hello world\");</script>")

(test (basic-default :suite simple)
  (is (string= (clean *basic-html*)
               "Lorem ipsum dolor sit amet alert(&quot;hello world&quot;);")))

(test (basic-restricted :suite simple)
  (is (string= (clean *basic-html* restricted)
               "<b>Lorem</b> ipsum <strong>dolor</strong> sit amet alert(&quot;hello world&quot;);")))

(test (basic-basic :suite simple)
  (is (string= (clean *basic-html* basic)
               "<b>Lorem</b> <a href=\"pants\" rel=\"nofollow\">ipsum</a> <a href=\"http://foo.com/\" rel=\"nofollow\"><strong>dolor</strong></a> sit<br>amet alert(&quot;hello world&quot;);")))

(test (basic-relaxed :suite simple)
  (is (string= (clean *basic-html* relaxed)
               "<b>Lorem</b> <a href=\"pants\" title=\"foo\">ipsum</a> <a href=\"http://foo.com/\"><strong>dolor</strong></a> sit<br>amet alert(&quot;hello world&quot;);")))

;;; malformed

(defparameter *malformed-html*
  "Lo<!-- comment -->rem</b> <a href=pants title=\"foo>ipsum <a href=\"http://foo.com/\"><strong>dolor</a></strong> sit<br>amet <script>alert(\"hello world\");")

(test (malformed-default :suite simple)
  (is (string= (clean *malformed-html*)
               "Lorem dolor sit amet alert(&quot;hello world&quot;);")))

(test (malformed-restricted :suite simple)
  (is (string= (clean *malformed-html* restricted)
               "Lorem <strong>dolor</strong> sit amet alert(&quot;hello world&quot;);")))

(test (malformed-basic :suite simple)
  (is (string= (clean *malformed-html* basic)
               "Lorem <a href=\"pants\" rel=\"nofollow\"><strong>dolor</strong></a> sit<br>amet alert(&quot;hello world&quot;);")))


(test (malformed-relaxed :suite simple)
  (is (string= (clean *malformed-html* relaxed)
               "Lorem <a href=\"pants\" title=\"foo>ipsum <a href=\"><strong>dolor</strong></a> sit<br>amet alert(&quot;hello world&quot;);"
               #+ () "Lorem <a href=\"pants\" title=\"foo&gt;ipsum &lt;a href=\"><strong>dolor</strong></a> sit<br>amet alert(&quot;hello world&quot;);")))

;;; unclosed

(defparameter *unclosed-html*
  "<p>a</p><blockquote>b")

(test (unclosed-default :suite simple)
  (is (string= (clean *unclosed-html*)
               " a b ")))

(test (unclosed-restricted :suite simple)
  (is (string= (clean *unclosed-html* restricted)
               " a b ")))

(test (unclosed-basic :suite simple)
  (is (string= (clean *unclosed-html* basic)
               "<p>a</p><blockquote>b</blockquote>")))

(test (unclosed-relaxed :suite simple)
  (is (string= (clean *unclosed-html* relaxed)
               "<p>a</p><blockquote>b</blockquote>")))

;;; malicious

(defparameter *malicious-html*
  "<b>Lo<!-- comment -->rem</b> <a href=\"javascript:pants\" title=\"foo\">ipsum</a> <a href=\"http://foo.com/\"><strong>dolor</strong></a> sit<br>amet <<foo>script>alert(\"hello world\");</script>")

(test (malicious-default :suite simple)
  (is (string= (clean *malicious-html* default)
               "Lorem ipsum dolor sit amet &lt;script&gt;alert(&quot;hello world&quot;);")))

(test (malicious-restricted :suite simple)
  (is (string= (clean *malicious-html* restricted)
               "<b>Lorem</b> ipsum <strong>dolor</strong> sit amet &lt;script&gt;alert(&quot;hello world&quot;);")))

(test (malicious-basic :suite simple)
  (is (string= (clean *malicious-html* basic)
               "<b>Lorem</b> <a rel=\"nofollow\">ipsum</a> <a href=\"http://foo.com/\" rel=\"nofollow\"><strong>dolor</strong></a> sit<br>amet &lt;script&gt;alert(&quot;hello world&quot;);")))

(test (malicious-relaxed :suite simple)
  (is (string= (clean *malicious-html* relaxed)
               "<b>Lorem</b> <a title=\"foo\">ipsum</a> <a href=\"http://foo.com/\"><strong>dolor</strong></a> sit<br>amet &lt;script&gt;alert(&quot;hello world&quot;);")))

;;; raw-comment

(defparameter *raw-comment-html* "<!-- comment -->Hello")

(test (raw-comment-default :suite simple)
  (is (string= (clean *raw-comment-html*)
               "Hello")))

(test (raw-comment-restricted :suite simple)
  (is (string= (clean *raw-comment-html* restricted)
               "Hello")))

(test (raw-comment-basic :suite simple)
  (is (string= (clean *raw-comment-html* basic)
               "Hello")))

(test (raw-comment-relaxed :suite simple)
  (is (string= (clean *raw-comment-html* relaxed)
               "Hello")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tricky
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite tricky :in sanitize)

;;; protocol-based JS injection: simple, no spaces

(defparameter *js-injection-html-1*
  "<a href=\"javascript:alert(\'XSS\');\">foo</a>")

(test (js-injection-1-default :suite tricky)
  (is (string= (clean *js-injection-html-1*
                      default)
               "foo")))

(test (js-injection-1-restricted :suite tricky)
  (is (string= (clean *js-injection-html-1*
                      restricted)
               "foo")))

(test (js-injection-1-basic :suite tricky)
  (is (string= (clean *js-injection-html-1*
                      basic)
               "<a rel=\"nofollow\">foo</a>")))

(test (js-injection-1-relaxed :suite tricky)
  (is (string= (clean *js-injection-html-1*
                      relaxed)
               "<a>foo</a>")))

;;; protocol-based JS injection: simple, spaces before

(defparameter *js-injection-html-2*
  "<a href=\"javascript    :alert(\'XSS\');\">foo</a>")

(test (js-injection-2-default :suite tricky)
  (is (string= (clean *js-injection-html-2*
                      default)
               "foo")))

(test (js-injection-2-restricted :suite tricky)
  (is (string= (clean *js-injection-html-2*
                      restricted)
               "foo")))

(test (js-injection-2-basic :suite tricky)
  (is (string= (clean *js-injection-html-2*
                      basic)
               "<a rel=\"nofollow\">foo</a>")))

(test (js-injection-2-relaxed :suite tricky)
  (is (string= (clean *js-injection-html-2*
                      relaxed)
               "<a>foo</a>")))

;;; protocol-based JS injection: simple, spaces after

(defparameter *js-injection-html-3*
  "<a href=\"javascript:    alert(\'XSS\');\">foo</a>")

(test (js-injection-3-default :suite tricky)
  (is (string= (clean *js-injection-html-3*
                      default)
               "foo")))

(test (js-injection-3-restricted :suite tricky)
  (is (string= (clean *js-injection-html-3*
                      restricted)
               "foo")))

(test (js-injection-3-basic :suite tricky)
  (is (string= (clean *js-injection-html-3*
                      basic)
               "<a rel=\"nofollow\">foo</a>")))

(test (js-injection-3-relaxed :suite tricky)
  (is (string= (clean *js-injection-html-3*
                      relaxed)
               "<a>foo</a>")))

;;; protocol-based JS injection: simple, spaces before and after

(defparameter *js-injection-html-4*
  "<a href=\"javascript    :   alert(\'XSS\');\">foo</a>")

(test (js-injection-4-default :suite tricky)
  (is (string= (clean *js-injection-html-4*
                      default)
               "foo")))

(test (js-injection-4-restricted :suite tricky)
  (is (string= (clean *js-injection-html-4*
                      restricted)
               "foo")))

(test (js-injection-4-basic :suite tricky)
  (is (string= (clean *js-injection-html-4*
                      basic)
               "<a rel=\"nofollow\">foo</a>")))

(test (js-injection-4-relaxed :suite tricky)
  (is (string= (clean *js-injection-html-4*
                      relaxed)
               "<a>foo</a>")))

;;; protocol-based JS injection: preceding colon

(defparameter *js-injection-html-5*
  "<a href=\":javascript:alert(\'XSS\');\">foo</a>")

(test (js-injection-5-default :suite tricky)
  (is (string= (clean *js-injection-html-5*
                      default)
               "foo")))

(test (js-injection-5-restricted :suite tricky)
  (is (string= (clean *js-injection-html-5*
                      restricted)
               "foo")))

(test (js-injection-5-basic :suite tricky)
  (is (string= (clean *js-injection-html-5*
                      basic)
               "<a rel=\"nofollow\">foo</a>")))

(test (js-injection-5-relaxed :suite tricky)
  (is (string= (clean *js-injection-html-5*
                      relaxed)
               "<a>foo</a>")))

;;; protocol-based JS injection: UTF-8 encoding

(defparameter *js-injection-html-6*
  "<a href=\"javascript&#58;\">foo</a>")

(test (js-injection-6-default :suite tricky)
  (is (string= (clean *js-injection-html-6*
                      default)
               "foo")))

(test (js-injection-6-restricted :suite tricky)
  (is (string= (clean *js-injection-html-6*
                      restricted)
               "foo")))

(test (js-injection-6-basic :suite tricky)
  (is (string= (clean *js-injection-html-6*
                      basic)
               "<a rel=\"nofollow\">foo</a>")))

(test (js-injection-6-relaxed :suite tricky)
  (is (string= (clean *js-injection-html-6*
                      relaxed)
               "<a>foo</a>")))

;;; protocol-based JS injection: long UTF-8 encoding

(defparameter *js-injection-html-7*
  "<a href=\"javascript&#0058;\">foo</a>")

(test (js-injection-7-default :suite tricky)
  (is (string= (clean *js-injection-html-7*
                      default)
               "foo")))

(test (js-injection-7-restricted :suite tricky)
  (is (string= (clean *js-injection-html-7*
                      restricted)
               "foo")))

(test (js-injection-7-basic :suite tricky)
  (is (string= (clean *js-injection-html-7*
                      basic)
               "<a rel=\"nofollow\">foo</a>")))

(test (js-injection-7-relaxed :suite tricky)
  (is (string= (clean *js-injection-html-7*
                      relaxed)
               "<a>foo</a>")))

;;; protocol-based JS injection: long UTF-8 encoding without semicolons

(defparameter *js-injection-html-8*
  "<a href=&#0000106&#0000097&#0000118&#0000097&#0000115&#0000099&#0000114&#0000105&#0000112&#0000116&#0000058&#0000097&#0000108&#0000101&#0000114&#0000116&#0000040&#0000039&#0000088&#0000083&#0000083&#0000039&#0000041>foo</a>")

(test (js-injection-8-default :suite tricky)
  (is (string= (clean *js-injection-html-8*
                      default)
               "foo")))

(test (js-injection-8-restricted :suite tricky)
  (is (string= (clean *js-injection-html-8*
                      restricted)
               "foo")))

(test (js-injection-8-basic :suite tricky)
  (is (string= (clean *js-injection-html-8*
                      basic)
               "<a rel=\"nofollow\">foo</a>")))

(test (js-injection-8-relaxed :suite tricky)
  (is (string= (clean *js-injection-html-8*
                      relaxed)
               "<a>foo</a>")))

;;; protocol-based JS injection: hex encoding

(defparameter *js-injection-html-9*
  "<a href=\"javascript&#x3A;\">foo</a>")

(test (js-injection-9-default :suite tricky)
  (is (string= (clean *js-injection-html-9*
                      default)
               "foo")))

(test (js-injection-9-restricted :suite tricky)
  (is (string= (clean *js-injection-html-9*
                      restricted)
               "foo")))

(test (js-injection-9-basic :suite tricky)
  (is (string= (clean *js-injection-html-9*
                      basic)
               "<a rel=\"nofollow\">foo</a>")))

(test (js-injection-9-relaxed :suite tricky)
  (is (string= (clean *js-injection-html-9*
                      relaxed)
               "<a>foo</a>")))

;;; protocol-based JS injection: long hex encoding

(defparameter *js-injection-html-10*
  "<a href=\"javascript&#x003A;\">foo</a>")

(test (js-injection-10-default :suite tricky)
  (is (string= (clean *js-injection-html-10*
                      default)
               "foo")))

(test (js-injection-10-restricted :suite tricky)
  (is (string= (clean *js-injection-html-10*
                      restricted)
               "foo")))

(test (js-injection-10-basic :suite tricky)
  (is (string= (clean *js-injection-html-10*
                      basic)
               "<a rel=\"nofollow\">foo</a>")))

(test (js-injection-10-relaxed :suite tricky)
  (is (string= (clean *js-injection-html-10*
                      relaxed)
               "<a>foo</a>")))

;;; protocol-based JS injection: hex encoding without semicolons

(defparameter *js-injection-html-11*
  "<a href=&#x6A&#x61&#x76&#x61&#x73&#x63&#x72&#x69&#x70&#x74&#x3A&#x61&#x6C&#x65&#x72&#x74&#x28&#x27&#x58&#x53&#x53&#x27&#x29>foo</a>")

(test (js-injection-11-default :suite tricky)
  (is (string= (clean *js-injection-html-11*
                      default)
               "foo")))

(test (js-injection-11-restricted :suite tricky)
  (is (string= (clean *js-injection-html-11*
                      restricted)
               "foo")))

(test (js-injection-11-basic :suite tricky)
  (is (string= (clean *js-injection-html-11*
                      basic)
               "<a rel=\"nofollow\">foo</a>")))

(test (js-injection-11-relaxed :suite tricky)
  (is (string= (clean *js-injection-html-11*
                      relaxed)
               "<a>foo</a>")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; entity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite misc :in sanitize)

;;; should translate valid HTML entities

(test (misc-1 :suite misc)
  (is (string= (clean "Don&apos;t tas&eacute; me &amp; bro!")
               "Don't tasé me &amp; bro!")))

;;; should translate valid HTML entities while encoding unencoded ampersands

(test (misc-2 :suite misc)
  (is (string= (clean "cookies&sup2; & &frac14; cr&eacute;me")
               "cookies² &amp; ¼ créme")))

;;; should never output &apos;

(test (misc-3 :suite misc)
  (is (string= (clean "<a href='&apos;' class=\"' &#39;\">IE6 isn't a real browser</a>")
               "IE6 isn't a real browser")))

;;; should not choke on several instances of the same element in a row

(test (misc-4 :suite misc)
  (is (string= (clean "<img src=\"http://www.google.com/intl/en_ALL/images/logo.gif\"><img src=\"http://www.google.com/intl/en_ALL/images/logo.gif\"><img src=\"http://www.google.com/intl/en_ALL/images/logo.gif\"><img src=\"http://www.google.com/intl/en_ALL/images/logo.gif\">")
               "")))

;;; should surround the contents of :whitespace_elements with space characters when removing the element

(test (misc-5 :suite misc)
  (is (string= (clean "foo<div>bar</div>baz")
               "foo bar baz"))
  (is (string= (clean "foo<br>bar<br>baz")
               "foo bar baz"))
  (is (string= (clean "foo<hr>bar<hr>baz")
               "foo bar baz")))
