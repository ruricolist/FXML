FXML/Sanitize is a rewrite of Moskvitin
Andrey's [CL-SANITIZE][cl-sanitize], an excellent whitelist-based HTML
sanitizer.

The difference is that, while CL-SANITIZE works on a DOM, FXML/Sanitize
works as a filter for SAX streams. This way, if you are already
streaming your documents as SAX events, you get sanitization almost
for free.

Instead of

    (fxml:parse string (fxml.stp:make-builder))

Write

    (fxml:parse string
                (fxml.sanitize:wrap-sanitize (fxml.stp:make-builder)
                                             default))

`wrap-sanitize` takes two arguments: the handler it wraps, and a
sanitizer mode.

The default mode, `default`, strips all HTML.

`restricted` allows a basic handful of inline formatting elements
(basically italics, bold, and underline).

`basic` allows basic block-level formatting, like `pre` and
`blockquote`, as well as links (with `rel=nofollow`).

`relaxed` allows tables, and does not add `nofollow`.

You can define other modes using `define-sanitize-mode`. The syntax is
the same as CL-SANITIZE, except that the name is bound a global
lexical (using `serapeum:def`) rather than with `defparameter`.

[cl-sanitize]: https://github.com/archimag/cl-sanitize
