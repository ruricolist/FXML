;;;; sax-proxy.lisp
;;;;
;;;; This file is part of the FXML parser, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Copyright (c) 2004 David Lichteblau
;;;; Copyright (c) 2014 Paul M. Rodriguez
;;;; Author: David Lichteblau

(in-package :fxml)

(defclass broadcast-handler (fxml.sax:abstract-handler)
  ((handlers :initform nil
             :initarg :handlers
             :accessor broadcast-handler-handlers))
  (:documentation
   "A SAX handler which passes every event it receives on to each of several
    chained handlers, somewhat similar to the way a @foo{broadcast-stream}
    works.

    You can subclass @foo{broadcast-handler} to modify the events
    before they are being passed on.  Define methods on your handler
    class for the events to be modified.  All other events will pass
    through to the chained handlers unmodified.

    Broadcast handler functions return the result of calling the event
    function on the last handler in the list.  In particular,
    the overall result from @foo{sax:end-document} will be ignored
    for all other handlers.

    @see-slot{broadcast-handler-handlers}"))

(setf (documentation #'broadcast-handler-handlers 'function)
      "@arg[instance]{A @class{broadcast-handler}}
       @return{A list of @class{SAX handler}s.}

       Returns the list of SAX handlers that arechained to this broadcast
       handler.")

(defclass values-handler (broadcast-handler)
  ()
  (:documentation
   "@class{values-handler} is a subclass of @class{broadcast-handler},
   which finally returns, as the overall result from
   @foo{sax:end-document}, a set of multiple values, one per
   handler."))

(defmethod fxml.sax:end-document ((self values-handler))
  (values-list (mapcar #'fxml.sax:end-document (broadcast-handler-handlers self))))

(defun make-broadcast-handler (&rest handlers)
  "@arg[handlers]{A list of @class{SAX handler}s.}
   @return{A @class{broadcast-handler}.}

   Creates a SAX handler which passes every event it receives on to each
   handler specified as an argument to this function.

   See @class{broadcast-handler} for details. "
  (make-instance 'broadcast-handler :handlers handlers))

(defun make-values-handler (&rest handlers)
  "@arg[handlers]{A list of @class{SAX handler}s.}
   @return{A @class{values-handler}.}

   Creates a SAX handler which passes every event it receives on to
   each handler specified as an argument to this function, and finally
   returns, as multiple values, the result of calling
   @fun{sax:end-document} on each.

   See @class{broadcast-handler} for details."
  (make-instance 'values-handler :handlers handlers))

(defclass sax-proxy (broadcast-handler)
  ()
  (:documentation
   "@class{sax-proxy} is a subclass of @class{broadcast-handler} which
    sends events to exactly one chained handler.

    This class is still included for compatibility with older versions of
    FXML which did not include the more general @class{broadcast-handler}
    yet, but has been retrofitted as a subclass of the latter.

    @see-slot{proxy-chained-handler}"))

(defmethod initialize-instance
    :after ((instance sax-proxy) &key chained-handler)
  (setf (proxy-chained-handler instance) chained-handler))

(defmethod proxy-chained-handler ((instance sax-proxy))
  "@arg[instance]{A @class{sax-proxy}.}
   @return{A @class{SAX handler}s.}

   Returns the SAX handler that is chained to this SAX proxy."
  (car (broadcast-handler-handlers instance)))

(defmethod (setf proxy-chained-handler) (newval (instance sax-proxy))
  (setf (broadcast-handler-handlers instance) (list newval)))

(macrolet ((define-proxy-method (name (&rest args))
             `(defmethod ,name ((handler broadcast-handler) ,@args)
                (let (result)
                  (dolist (next (broadcast-handler-handlers handler))
                    (setf result (,name next ,@args)))
                  result))))
  (define-proxy-method fxml.sax:start-document ())
  (define-proxy-method fxml.sax:start-element (uri lname qname attributes))
  (define-proxy-method fxml.sax:start-prefix-mapping (prefix uri))
  (define-proxy-method fxml.sax:characters (data))
  (define-proxy-method fxml.sax:unescaped (data))
  (define-proxy-method fxml.sax:processing-instruction (target data))
  (define-proxy-method fxml.sax:end-prefix-mapping (prefix))
  (define-proxy-method fxml.sax:end-element (namespace-uri local-name qname))
  (define-proxy-method fxml.sax:end-document ())
  (define-proxy-method fxml.sax:comment (data))
  (define-proxy-method fxml.sax:start-cdata ())
  (define-proxy-method fxml.sax:end-cdata ())
  (define-proxy-method fxml.sax:start-dtd (name public-id system-id))
  (define-proxy-method fxml.sax:end-dtd ())
  (define-proxy-method fxml.sax:start-internal-subset ())
  (define-proxy-method fxml.sax:end-internal-subset ())
  (define-proxy-method fxml.sax:unparsed-entity-declaration (name pub sys not))
  (define-proxy-method fxml.sax:external-entity-declaration (kind name pub sys))
  (define-proxy-method fxml.sax:internal-entity-declaration (kind name value))
  (define-proxy-method fxml.sax:notation-declaration (name public-id system-id))
  (define-proxy-method fxml.sax:element-declaration (name model))
  (define-proxy-method fxml.sax:attribute-declaration (elt attr type default))
  (define-proxy-method fxml.sax:entity-resolver (resolver))
  (define-proxy-method fxml.sax::dtd (dtd)))

(defmethod fxml.sax:register-sax-parser :after ((handler broadcast-handler) parser)
  (dolist (next (broadcast-handler-handlers handler))
    (fxml.sax:register-sax-parser next parser)))
