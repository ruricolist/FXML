FXML is a fork of CXML emphasizing security and tolerance. If you are
dealing with well-formed XML from trusted sources, FXML has few if any
advantages over CXML proper. But if you are dealing with potentially
faulty XML, from untrusted sources – if, for example, you are running
a feed reader – then FXML is probably the better choice.

FXML does not conflict with CXML. They can both be loaded into the
same Lisp system, and in fact they can be made to interoperate. This
means that, although FXML’s API is very close to CXML’s, and for the
most part, you can refer to the [CXML documentation][cxml] for usage,
all package names have been changed:

```
CXML     -> FXML
CXML-DOM -> FXML-DOM
DOM      -> FXML.DOM
KLACKS   -> FXML.KLACKS
SAX      -> FXML.SAX
STP      -> FXML.STP
```

# Differences from CXML

Bug fixes:
- Klacks handles namespaces correctly.
- XML parser does not reverse attribute lists.

Security: 
- DTDs are forbidden by default.
- Entity definitions in DTDs are forbidden by default.
- External references in DTDS are forbidden by default.

New features:
- Restarts allow recovery from many simple XML problems.
- Inline (callback-based) SAX handlers.
- Multiplexing SAX handlers.
- Document fragment support in STP.

Removed features:
- FXML does not support HAX.

# CXML compatibility

You can make FXML and CXML interoperable by loading the system
`fxml/cxml`. This defines SAX methods for FXML, and FXML.SAX methods
for CXML. You can then mix CXML and FXML sinks and sources:

     (cxml:parse xml-document (fxml.stp:make-builder))
     (fxml:parse xml-document (stp:make-builder))

# Security

This fork of CXML tries to be *secure by default*, following the lead
of Python’s [`defusedxml`][defusedxml] library. CXML is secure by
default in some respects – it does not fetch external resources unless
you tell it to – but it still vulnerable to other attacks, like the
[billion laughs][].

All of the new conditions are subtypes of `fxml:xml-security-error`, which is *not* a subtype of any other FXML error: you must handle it directly.

Three extra keyword arguments for `fxml:parse`:

## `forbid-dtd`

Default false. Signal `fxml:dtd-forbidden` if the document contains a
DTD processing declarations.

The name, pubid, and sysid can be read with `cxl:dtd-name`,
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

(This is actually an error by default in FXML, but it doesn’t have its
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

This is enough to handle many characteristic problems with XML in the
wild: leading and trailing junk, unescaped ampersands, illegal
characters, and DTDs (a feature which is largely indistinguishable
from a bug).

Although I do not care much about conformance, I do not think that
providing restarts can be said to be non-conforming. My invoking a
restart is no different than my stopping, editing the document, and
re-submitting it to the parser, except that it saves time. The fact
that the XML spec was written in an era when *programming language*
was a euphemism for *Java* does not mean we should have to write Java
in Lisp to deal with XML.

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
recurse on it; but by combining callback handlers and values handlers,
you can do it in one pass:

    (multiple-value-bind (dom text)
        (fxml:parse document
                    (fxml:make-values-handler
                     (stp:make-builder)
                     (let ((text (make-string-output-stream)))
                       (sax:make-callback-handler
                        :characters (λ (data)
                                       (write-string data text))
                        :end-document (λ ()
                                         (get-output-stream-string text))))))
      ...)

[cxml]: http://common-lisp.net/project/fxml/
[defusedxml]: https://pypi.python.org/pypi/defusedxml
[billion laughs]: https://en.wikipedia.org/wiki/Billion_laughs
[sax]: https://common-lisp.net/project/cxml/saxoverview/
[klacks]: http://lichteblau.blogspot.com/2007/03/klacks-parsing.html
