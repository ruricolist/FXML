(defpackage :cxml-stp
  (:use :cl)
  (:nicknames :stp)
  (:export #:*check-uri-syntax*
	   #:stp-error

	   #:node
	   #:parent
	   #:root
	   #:base-uri
	   #:detach
	   #:string-value
	   #:copy
	   #:serialize
	   ;; #:query
	   #:map-children
	   #:do-children
	   #:list-children
	   #:nth-child
	   #:first-child
	   #:last-child
	   #:previous-sibling
	   #:next-sibling
	   #:find-child
	   #:find-child-if
	   #:child-position
	   #:child-position-if
	   #:number-of-children
	   #:count-children
	   #:count-children-if
	   #:filter-children
	   #:map-recursively
	   #:do-recursively
	   #:find-recursively
	   #:find-recursively-if
	   #:filter-recursively

	   #:document
	   #:make-document
	   #:document-element

	   #:parent-node
	   #:prepend-child
	   #:append-child
	   #:delete-child
	   #:delete-nth-child
	   #:insert-child
	   #:insert-child-before
	   #:insert-child-after
	   #:delete-child-if
	   #:delete-children
	   #:replace-child

	   ;; #:named-node
	   #:local-name
	   #:namespace-prefix
	   #:namespace-uri
	   #:of-name
	   #:qualified-name

	   #:element
	   #:make-element
	   #:add-attribute
	   #:remove-attribute
	   #:find-attribute-named
	   #:find-attribute-if
	   #:attribute-value
	   #:list-attributes
	   #:with-attributes
	   #:map-attributes
	   #:find-namespace
	   #:find-local-namespace
	   #:find-extra-namespace
	   #:map-extra-namespaces
	   #:add-extra-namespace
	   #:remove-extra-namespace

	   #:attribute
	   #:make-attribute
	   #:value
	   #:rename-attribute

	   #:comment
	   #:make-comment
	   #:data
	   
	   #:processing-instruction
	   #:make-processing-instruction
	   #:target

	   #:document-type
	   #:make-document-type
	   #:root-element-name
	   #:system-id
	   #:public-id
	   #:internal-subset
	   #:dtd

	   #:text
	   #:make-text

	   #:make-builder)
  (:documentation
   "STP is a data structure for well-formed XML documents.

    @begin[Parsing and Serializing]{section}
    To parse into STP, use an STP builder together with a function
    generating SAX events:

    @aboutfun{make-builder}
    Serialize STP by sending SAX events for the tree to a sink:

    @aboutfun{serialize}
    @end{section}
    @begin[Names and Namespace URIs]{section}
    STP represents namespace-well-formed documents only.  All functions
    accepting names take a local-name and and a namespace URI as an argument.
    Qualified names are accepted where it makes sense, and named nodes have
    a namespace prefix that is taken into account for well-formedness checks.

    There are two kinds of named nodes: @class{element} and @class{attribute}.
    Their slots are:

    @aboutfun{local-name}
    @aboutfun{namespace-uri}
    @aboutfun{namespace-prefix}
    For @code{element}, all of the above can be changed using SETF (subject
    to well-formedness checks).

    For attribute, @fun{local-name} can be changed directly, while URI and
    prefix always have to be changed in the same step using
    @fun{rename-attribute}.

    A node's qualified name can be be queried:

    @aboutfun{qualified-name}

    @code{of-name} is convenient when searching for elements or attributes:

    @aboutfun{of-name}
    @end{section}
    @begin[Subclasses of Node]{section}
    All STP nodes have a common superclass.

    @aboutclass{node}
    Documents and elements can have children:

    @aboutclass{parent-node}
    @aboutclass{document}
    @aboutclass{element}
    Attributes belong to an @code{element}:

    @aboutclass{attribute}
    Other kinds of nodes:

    @aboutclass{comment}
    @aboutclass{document-type}
    @aboutclass{processing-instruction}
    @aboutclass{text}
    @end{section}
    @begin[Creating nodes]{section}
    Nodes are created using the following functions:
     
    @aboutfun{make-attribute}
    @aboutfun{make-comment}
    @aboutfun{make-document}
    @aboutfun{make-document-type}
    @aboutfun{make-element}
    @aboutfun{make-processing-instruction}
    @aboutfun{make-text}
    In addition, nodes can be copied including all their children:

    @aboutfun{copy}
    @end{section}
    @begin[Listing Child Nodes]{section}
    Nodes have an optional parent node and can have children.

    @aboutfun{parent}
    If a node has a @class{document} as its ancestor, it can be found using
    the @fun{document} function.

    @aboutfun{document}
    Since the @code{parent} slot needs to be updated when children are added or
    removed, the sequence of children is not exposed as a mutable Common
    Lisp sequence.

    @aboutfun{list-children}
    @aboutfun{map-children}
    @aboutmacro{do-children}
    The following DOM-like functions are also offered:

    @aboutfun{nth-child}
    @aboutfun{first-child}
    @aboutfun{last-child}
    @aboutfun{previous-sibling}
    @aboutfun{next-sibling}
    A wide variety of sequence-related functions is offered that work
    like the Common Lisp functions of the same name, but without the need
    to call @fun{list-children} first:

    @aboutfun{find-child}
    @aboutfun{find-child-if}
    @aboutfun{child-position}
    @aboutfun{child-position-if}
    @aboutfun{count-children}
    @aboutfun{count-children-if}
    @aboutfun{filter-children}
    The functions listed above walk only across the direct children of the
    parent node.  In addition, the node hierarchy can be mapped recursively
    using these functions:

    @aboutfun{map-recursively}
    @aboutmacro{do-recursively}
    @aboutfun{find-recursively}
    @aboutfun{filter-recursively}

    @end{section}
    @begin[Adding and Removing Child Nodes]{section}
    While all nodes can be asked for their children, only documents and
    elements permit actually adding children.  (For all other nodes, the
    sequence of children appears as empty.)

    The most flexible function capable of changing the child nodes is
    @fun{replace-children}.  Perhaps more common is @fun{insert-child},
    a specialized version for only one new child.

    @aboutfun{replace-children}
    @aboutfun{insert-child}
    Various convenience functions are offered in addition:

    @aboutfun{prepend-child}
    @aboutfun{append-child}
    @aboutfun{delete-child}
    @aboutfun{delete-child-if}
    @aboutfun{delete-nth-child}
    @aboutfun{insert-child-before}
    @aboutfun{insert-child-after}
    @aboutfun{replace-child}
    A node can also be deleted from its parent directly using @fun{detach}.

    @aboutfun{detach}
    @fun{detach} also works for attributes.

    @end{section}
    @begin[Elements and their Attributes]{section}
    In addition to their children, elements have attributes and \"extra
    namespaces\".

    Attributes themselves are nodes and be accessed using these functions:

    @aboutfun{add-attribute}
    @aboutfun{remove-attribute}
    @aboutfun{find-attribute-named}
    @aboutfun{find-attribute-if}
    @aboutfun{list-attributes}
    @aboutfun{map-attributes}
    @aboutmacro{with-attributes}
    As a shortcut, the @fun{attribute-value} and its @code{setf} function
    allow access to attribute values by name, without having to look up the
    attribute node first:
   
    @aboutfun{attribute-value}
    There are three ways to declare a namespace: Using the name of the
    element, using the name of one of its attributes, or using an \"extra
    namespace\".  A prefix can be looked up from any of these local
    declarations.  It is also possible to look up a namespace while taking
    into account all declarations on parent elements.

    @aboutfun{find-local-namespace}
    @aboutfun{find-namespace}
    Extra namespaces are needed only when a namespace must be declared even
    though there is no element or attribute referencing it through its name.
    For example, an attribute declared with type @code{QName} using
    RelaxNG/XSD must reference a namespace in scope.

    @aboutfun{add-extra-namespace}
    @aboutfun{remove-extra-namespace}
    @aboutfun{find-extra-namespace}
    @aboutfun{map-extra-namespaces}
    @end{section}"))

(defpackage :cxml-stp-impl
  (:use :cl :stp)
  (:shadow #:document #:document-type)
  (:import-from :xpath-protocol #:define-default-method))
