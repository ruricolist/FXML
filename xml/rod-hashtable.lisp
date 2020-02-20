;;;; rod-hashtable.lisp -- Specialized hash tables for interning rods.
;;;;
;;;; This file is part of the FXML parser, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;;  (c) copyright 1999 by Gilbert Baumann
;;;;  (c) copyright 2003 by Henrik Motakef
;;;;  (c) copyright 2004 knowledgeTools Int. GmbH
;;;;  (c) copyright 2004 David Lichteblau
;;;;  (c) copyright 2005 David Lichteblau
;;;;  (c) copyright 2014 Paul M. Rodriguez
;;;;  (c) copyright 2015 Paul M. Rodriguez
;;;;  (c) copyright 2018 Paul M. Rodriguez

;;; make-rod-hashtable
;;; rod-hash-get hashtable rod &optional start end -> value ; successp
;;; (setf (rod-hash-get hashtable rod &optional start end) new-value
;;;

(in-package :fxml)

(defstruct (rod-hashtable (:constructor make-rod-hashtable/low))
  (size (error "No size") :type alexandria:array-index) ;size of table
  (table (error "No table") :type simple-vector))

(defun make-rod-hashtable (&key (size 200) (real-size (next-pow2 size)))
  (make-rod-hashtable/low
   :size real-size
   :table (make-array real-size :initial-element nil)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +fixnum-bits+
    (1- (integer-length most-positive-fixnum))
    "Pessimistic approximation of the number of bits of fixnums.")

  (defconstant +fixnum-mask+
    (1- (expt 2 +fixnum-bits+))
    "Pessimistic approximation of the largest bit-mask, still being a fixnum."))

(definline stir (a b)
  (%and +fixnum-mask+
        (%xor (%ior (%ash (%and a #.(ash +fixnum-mask+ -5)) 5)
                    (%ash a #.(- 5 +fixnum-bits+)))
              b)))

(definline rod-hash (rod start end)
  "Compute a hash code out of a rod."
  (let ((res (%- end start)))
    (loop for i from start below end do
      (setf res (stir res (rune-code (%rune rod i)))))
    res))

(defmacro rod=* (x y &key (start1 0) (end1 (length x))
                          (start2 0) (end2 (length y)))
  `(rod=** ,x ,y ,start1 ,end1 ,start2 ,end2))

;;; NB This is *not* the same thing as string=. In particular, the
;;; start and end arguments may be called with invalid indices.
(definline rod=** (x y start1 end1 start2 end2)
  (declare (alexandria:array-index start1 start2)
           (alexandria:array-length end1 end2)
           (optimize speed (safety 1) (debug 0)))
  (and (%= (%- end1 start1) (%- end2 start2))
       (loop for i of-type alexandria:array-index from start1 below end1
             and j of-type alexandria:array-index from start2
             unless (rune= (%rune x i) (%rune y j)) do
               (return nil)
             finally (return t))))

(definline hash-index (hash size)
  (logand hash (1- size)))

(defun rod-hash-get (hashtable rod &optional (start 0) (end (length rod))
                                             (hash (rod-hash rod start end)))
  (declare (type (simple-array rune (*)) rod)
           (type (and unsigned-byte fixnum) hash))
  (let ((j (hash-index hash
                       (rod-hashtable-size hashtable))))
    (dolist (q (svref (rod-hashtable-table hashtable) j)
               (values nil nil nil))
      (declare (type cons q))
      (when (rod=** (car q) rod
                    0 (length (the (simple-array rune (*)) (car q)))
                    start end)
        (return (values (cdr q) t (car q)))))))

(definline rod-subseq* (source start &optional (end (length source)))
  (subseq source start end))

(defun rod-hash-set (new-value hashtable rod &optional (start 0) (end (length rod))
                                                       (hash (rod-hash rod start end)))
  (let ((j (hash-index hash
                       (rod-hashtable-size hashtable)))
        (key nil))
    (dolist (q (svref (rod-hashtable-table hashtable) j)
               (progn
                 (setf key (rod-subseq* rod start end))
                 (push (cons key new-value)
                       (aref (rod-hashtable-table hashtable) j))))
      (when (rod=* (car q) rod :start2 start :end2 end)
        (setf key (car q))
        (setf (cdr q) new-value)
        (return)))
    (values new-value key)))

(defun (setf rod-hash-get) (new-value hashtable rod &optional (start 0) (end (length rod))
                                                              (hash (rod-hash rod start end)))
  (rod-hash-set new-value hashtable rod start end hash))
