;;; -*- Mode: Lisp; readtable: runes; -*-
;;;  (c) copyright 2007 David Lichteblau

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :fxml)

(defun fxml.klacks:make-tapping-source (upstream-source &optional sax-handler)
  (make-instance 'fxml.klacks:tapping-source
		 :upstream-source upstream-source
		 :dribble-handler sax-handler))

(defclass fxml.klacks:tapping-source (fxml.klacks:source)
    ((upstream-source :initarg :upstream-source :accessor upstream-source)
     (dribble-handler :initarg :dribble-handler :accessor dribble-handler)
     (seen-event-p :initform nil :accessor seen-event-p)
     (document-done-p :initform nil :accessor document-done-p)))

(defmethod initialize-instance :after ((instance fxml.klacks:tapping-source) &key)
  (let ((s-p (make-instance 'klacksax :source (upstream-source instance))))
    (fxml.sax:register-sax-parser (dribble-handler instance) s-p)))


;;; event dribbling 

(defun maybe-dribble (source)
  (unless (or (seen-event-p source) (document-done-p source))
    (when (eq (fxml.klacks:peek (upstream-source source)) :end-document)
      (setf (document-done-p source) t))
    (fxml.klacks:serialize-event (upstream-source source)
			    (dribble-handler source)
			    :consume nil)
    (setf (seen-event-p source) t)))

(defmethod fxml.klacks:peek ((source fxml.klacks:tapping-source))
  (multiple-value-prog1
      (fxml.klacks:peek (upstream-source source))
    (maybe-dribble source)))

(defmethod fxml.klacks:peek-value ((source fxml.klacks:tapping-source))
  (multiple-value-prog1
      (fxml.klacks:peek-value (upstream-source source))
    (maybe-dribble source)))

(defmethod fxml.klacks:peek-next ((source fxml.klacks:tapping-source))
  (setf (seen-event-p source) nil)
  (multiple-value-prog1
      (fxml.klacks:peek-next (upstream-source source))
    (maybe-dribble source)))

(defmethod fxml.klacks:consume ((source fxml.klacks:tapping-source))
  (maybe-dribble source)
  (multiple-value-prog1
      (fxml.klacks:consume (upstream-source source))
    (setf (seen-event-p source) nil)))


;;; loop through

(defmethod fxml.klacks:close-source ((source fxml.klacks:tapping-source))
  (fxml.klacks:close-source (upstream-source source)))

(defmethod fxml.klacks:map-attributes (fn (source fxml.klacks:tapping-source))
  (fxml.klacks:map-attributes fn (upstream-source source)))

(defmethod fxml.klacks:map-current-namespace-declarations
    (fn (source fxml.klacks:tapping-source))
  (fxml.klacks:map-current-namespace-declarations fn (upstream-source source)))

(defmethod fxml.klacks:list-attributes ((source fxml.klacks:tapping-source))
  (fxml.klacks:list-attributes (upstream-source source)))

(defmethod fxml.klacks:current-line-number ((source fxml.klacks:tapping-source))
  (fxml.klacks:current-line-number (upstream-source source)))

(defmethod fxml.klacks:current-column-number ((source fxml.klacks:tapping-source))
  (fxml.klacks:current-column-number (upstream-source source)))

(defmethod fxml.klacks:current-system-id ((source fxml.klacks:tapping-source))
  (fxml.klacks:current-system-id (upstream-source source)))

(defmethod fxml.klacks:current-xml-base ((source fxml.klacks:tapping-source))
  (fxml.klacks:current-xml-base (upstream-source source)))

(defmethod fxml.klacks:current-cdata-section-p ((source fxml.klacks:tapping-source))
  (fxml.klacks:current-cdata-section-p (upstream-source source)))

(defmethod fxml.klacks:find-namespace-binding
    (prefix (source fxml.klacks:tapping-source))
  (fxml.klacks:find-namespace-binding prefix (upstream-source source)))

(defmethod fxml.klacks:decode-qname (qname (source fxml.klacks:tapping-source))
  (fxml.klacks:decode-qname qname (upstream-source source)))
