[![Build Status](https://travis-ci.org/ruricolist/FXML.svg?branch=master)](https://travis-ci.org/ruricolist/FXML)

FXML is a fork of [CXML][]. Like CXML, it is released under the
[Lisp-LGPL][].

You should use FXML instead of CXML if:

- You are parsing potentially ill-formed XML.
- You are parsing potentially malicious XML.
- You need to use [Klacks][] with namespaces.

FXML’s API is very close to CXML’s, and for the most part you can
refer to the [CXML documentation][CXML] for usage, but all package
names have been changed:

```
CXML     -> FXML
CXML-DOM -> FXML-DOM
DOM      -> FXML.DOM
KLACKS   -> FXML.KLACKS
SAX      -> FXML.SAX
STP      -> FXML.STP
```

On Lisps that support package-local nicknames, an easy way to switch
(if you use the package prefixes) is to shadow the CXML packages with
their FXML equivalents:

``` lisp
(:local-nicknames (:cxml :fxml)
                  (:cxml-dom :fxml-dom)
                  (:dom :fxml.dom)
                  (:klacks :fxml.klacks)
                  (:sax :fxml.sax)
                  (:stp :fxml.stp))
```

However, because the package names are different, it is possible to
load FXML and CXML in the same image. In fact, if you load the
`fxml/cxml` compatibility system, you can mix SAX sources and sinks
from FXML and CXML. Most usefully, you can keep any existing CXML code
for querying and manipulating documents but drop in the FXML parser:

    (fxml:parse #p"file.xml" (cxml-dom:make-dom-builder))

# Testing

FXML’s standard of behavior is that, if security restrictions are
relaxed, it should do the same thing as CXML. This is ensured with a
test suite which runs both FXML and CXML against the OASIS XML test
suite, and checks that they get the same results. (One exception –
because FXML uses QURI, it rejects some tests with invalid URIs that
CXML, using PURI, accepts.)

# Differences from CXML

Bug fixes:
- Klacks handles namespaces correctly.
- XML parser does not reverse attribute lists.

Security:
- DTDs are forbidden by default.
- Entity definitions in DTDs are forbidden by default.
- External references in DTDS are forbidden by default.

New features:
- Backward compatibility with CXML (system `fxml/cxml`).
- Restarts allow recovery from many simple XML problems.
- Inline (callback-based) SAX handlers.
- SAX handlers that return multiple values.
- Document fragment support in STP.
- DOM and STP are (partially) compatible.
- A [streaming sanitizer](sanitize/README.md) (system `fxml/sanitize`).
- Integration with [cl-html5-parser][] (system `fxml/html5`).
- Integration with [css-selectors][] (system `fxml/css-selectors`).

Removed features:
- FXML does not support HAX.

Implementation differences:
- FXML does not support Lisps that use UTF-16.
- Monolithic project (absorbs closure-common, cxml-stp).
- Uses named-readtables.
- Does not support [SCL][].
- Does not support non-Unicode Lisps.
- Uses QURI instead of PURI.

# XPath

In order to use [Plexippus XPath][xpath] with FXML, load the `fxml/xpath` system:

    (asdf:load-system "fxml/xpath")

This system implements the XPath protocol for both DOM and STP, so you can use XPath with all FXML documents.

# DOM and STP compatibility

DOM and STP overlap significantly in some respects. In particular,
read-only functions that return strings – functions that, for example,
look up attribute values or tag names – are effectively equivalent.
When the system `fxml/stp` is loaded, it specializes a number of STP
methods for DOM, and a number of DOM methods for STP. (At the moment,
looking at [the code](stp/dom.lisp) is the best way to see what is
defined.)

# Security

FXML tries to be *secure by default*, following the model of Python’s
[defusedxml][] library. CXML is secure by default in some respects –
it does not fetch external resources unless you tell it to – but it
still vulnerable to other attacks, like the [billion laughs][].

All of the new conditions are subtypes of `fxml:xml-security-error`,
which is *not* a subtype of any other FXML error: you must handle it
directly.

Three extra keyword arguments for `fxml:parse`:

## `forbid-dtd`

Default false. Signal `fxml:dtd-forbidden` if the document contains a
DTD processing declarations.

The name, pubid, and sysid can be read with `fxml:dtd-name`,
`fxml:dtd-pubid`, and `fxml:dtd-sysid`, respectively.

You can restart with `continue` if you want to parse the DTD anyway.

## `forbid-entities`

Default true. Signal `fxml:entities-forbidden` if the document’s DTD
contains entity definitions.

The name of the entity can be read with `fxml:entity-name` and, in the
case of an internal entity, the name can be read with
`fxml:entity-value`.

You can restart with `continue` to skip the entity being defined.

## `forbid-external`

Default true. Signal `fxml:external-reference-forbidden` if the
document’s DTD contains external references.

(This is actually an error by default in CXML, but it doesn’t have its
own condition.)

The pubid and sysid of the reference can be read with
`fxml:entity-reference-pubid` and `fxml:entity-reference-sysid`.

You can restart with `continue` if you want to let FXML try to fetch
the reference.

# Restarts

For most well-formedness violations, there is one and only one
reasonable way to proceed. We make this available as a `continue`
restart.

    (handler-bind ((fxml:well-formedness-violation #'continue))
      (fxml:parse ...))

This is enough to handle most practical problems with XML in the
wild: leading and trailing junk, unescaped ampersands, illegal
characters, and DTDs (a feature which is largely indistinguishable
from a bug).

(Although I do not care much about conformance, I do not think that
providing restarts can be said to be non-conforming. My invoking a
restart is no different than my stopping, editing the document, and
re-submitting it to the parser, except that it saves time. The fact
that the XML spec was written in an era when *programming language*
was a euphemism for *Java* does not mean we should have to write Java
in Lisp to deal with XML.)

## Undefined entities

Undefined entities are signaled as `undefined-entity`, a subtype of
`well-formedness-violation`. The name of the undefined entity can be
read with `undefined-entity-name`.

There are two restarts for undefined entities: you can omit the entity
with `continue`, or manually supply an expansion with `use-value`.

## Undeclared namespaces

Undeclared namespace prefixes are signaled as `undeclared-namespace`,
a subtype of `well-formedness-violation`. The prefix of the namespace
can be read with `undeclared-namespace-prefix`.

The only restart provided for `undeclared-namespace` is `store-value`:
to recover, you must provide a URI for the namespace. The XML is then
re-written exactly as if the element had the appropriate `xmlns`
attribute.

# SAX handlers

This fork provides two new classes of SAX handler: `values-handler`
and `callback-handler`.

*Callback handlers* let you create SAX handlers without having to
define classes. Instead of defining methods, you provide callbacks for
only the events that interest you.

*Values handlers* are just broadcast handlers that return, as multiple
values, the return value of each of their sub-handlers.

Values handlers and callback handlers can work together.

Suppose, for example, that you want to both parse an XML file, and
extract all of its text. Of course you could parse the DOM and then
recurse on it. But by combining callback handlers and values handlers,
you can do it in one pass:

    (multiple-value-bind (dom text)
        (fxml:parse document
                    (fxml:make-values-handler
                     (fxml.stp:make-builder)
                     (let ((text (make-string-output-stream)))
                       (fxml.sax:make-callback-handler
                        :characters (λ (data)
                                       (write-string data text))
                        :end-document (λ ()
                                         (get-output-stream-string text))))))
      ...)

[CXML]: http://common-lisp.net/project/cxml/
[defusedxml]: https://pypi.python.org/pypi/defusedxml
[billion laughs]: https://en.wikipedia.org/wiki/Billion_laughs
[sax]: https://common-lisp.net/project/cxml/saxoverview/
[Klacks]: http://lichteblau.blogspot.com/2007/03/klacks-parsing.html
[cl-html5-parser]: https://github.com/copyleft/cl-html5-parser
[css-selectors]: https://github.com/AccelerationNet/css-selectors
[Lisp-LGPL]: http://opensource.franz.com/preamble.html
[SCL]: http://www.scieneer.com/scl/
[xpath]: https://github.com/sharplispers/xpath
