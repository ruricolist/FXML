;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CMUCL dependent stuff + fixups
;;;   Created: 1999-05-25 22:32
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: GPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 1999 by Gilbert Baumann

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

(export 'glisp::read-byte-sequence :glisp)
(export 'glisp::read-char-sequence :glisp)
(export 'glisp::run-unix-shell-command :glisp)

(export 'glisp::getenv :glisp)

(defun glisp::read-byte-sequence (&rest ap)
  (apply #'read-sequence ap))

(defun glisp::read-char-sequence (&rest ap)
  (apply #'read-sequence ap))

(defun glisp::read-byte-sequence (sequence input &key (start 0) (end (length sequence)))
  (let (c (i start))
    (loop
      (cond ((= i end) (return i)))
      (setq c (read-byte input nil :eof))
      (cond ((eql c :eof) (return i)))
      (setf (aref sequence i) c)
      (incf i) )))

(defun glisp::read-byte-sequence (sequence input &key (start 0) (end (length sequence)))
  (let ((r (read-sequence sequence input :start start :end end)))
    (cond ((and (= r start) (> end start))
           (let ((byte (read-byte input nil :eof)))
             (cond ((eq byte :eof)
                    r)
                   (t
                    (setf (aref sequence start) byte)
                    (incf start)
                    (if (> end start)
                        (glisp::read-byte-sequence sequence input :start start :end end)
                      start)))))
          (t
           r))))

#||
(defun glisp::read-char-sequence (sequence input &key (start 0) (end (length sequence)))
  (let (c (i start))
    (loop
      (cond ((= i end) (return i)))
      (setq c (read-byte input nil :eof))
      (cond ((eql c :eof) (return i)))
      (setf (aref sequence i) c)
      (incf i) )))
||#

(defmacro glisp::with-timeout ((&rest ignore) &body body)
  (declare (ignore ignore))
  `(progn
     ,@body))

(defun glisp::open-inet-socket (hostname port)
  (let ((fd (extensions:connect-to-inet-socket hostname port)))
    (values
     (sys:make-fd-stream fd
                         :input t
                         :output t
                         :element-type '(unsigned-byte 8)
                         :name (format nil "Network connection to ~A:~D" hostname port))
     :byte)))

(defun glisp::g/make-string (length &rest options)
  (apply #'make-array length :element-type 'base-char options))

#||

RUN-PROGRAM is an external symbol in the EXTENSIONS package.
Function: #<Function RUN-PROGRAM {12E7B79}>
Function arguments:
  (program args &key (env *environment-list*) (wait t) pty input
   if-input-does-not-exist output (if-output-exists :error) (error :output)
   (if-error-exists :error) status-hook)
Function documentation:
  Run-program creates a new process and runs the unix progam in the
   file specified by the simple-string program.  Args are the standard
   arguments that can be passed to a Unix program, for no arguments
   use NIL (which means just the name of the program is passed as arg 0).

   Run program will either return NIL or a PROCESS structure.  See the CMU
   Common Lisp Users Manual for details about the PROCESS structure.

   The keyword arguments have the following meanings:
     :env -
        An A-LIST mapping keyword environment variables to simple-string
        values.
     :wait -
        If non-NIL (default), wait until the created process finishes.  If
        NIL, continue running Lisp until the program finishes.
     :pty -
        Either T, NIL, or a stream.  Unless NIL, the subprocess is established
        under a PTY.  If :pty is a stream, all output to this pty is sent to
        this stream, otherwise the PROCESS-PTY slot is filled in with a stream
        connected to pty that can read output and write input.
     :input -
        Either T, NIL, a pathname, a stream, or :STREAM.  If T, the standard
        input for the current process is inherited.  If NIL, /dev/null
        is used.  If a pathname, the file so specified is used.  If a stream,
        all the input is read from that stream and send to the subprocess.  If
        :STREAM, the PROCESS-INPUT slot is filled in with a stream that sends 
        its output to the process. Defaults to NIL.
     :if-input-does-not-exist (when :input is the name of a file) -
        can be one of:
           :error - generate an error.
           :create - create an empty file.
           nil (default) - return nil from run-program.
     :output -
        Either T, NIL, a pathname, a stream, or :STREAM.  If T, the standard
        output for the current process is inherited.  If NIL, /dev/null
        is used.  If a pathname, the file so specified is used.  If a stream,
        all the output from the process is written to this stream. If
        :STREAM, the PROCESS-OUTPUT slot is filled in with a stream that can
        be read to get the output. Defaults to NIL.
     :if-output-exists (when :input is the name of a file) -
        can be one of:
           :error (default) - generates an error if the file already exists.
           :supersede - output from the program supersedes the file.
           :append - output from the program is appended to the file.
           nil - run-program returns nil without doing anything.
     :error and :if-error-exists - 
        Same as :output and :if-output-exists, except that :error can also be
        specified as :output in which case all error output is routed to the
        same place as normal output.
     :status-hook -
        This is a function the system calls whenever the status of the
        process changes.  The function takes the process as an argument.
Its defined argument types are:
  (T T &KEY (:ENV T) (:WAIT T) (:PTY T) (:INPUT T) (:IF-INPUT-DOES-NOT-EXIST T)
   (:OUTPUT T) (:IF-OUTPUT-EXISTS T) (:ERROR T) (:IF-ERROR-EXISTS T)
   (:STATUS-HOOK T))
Its result type is:
  (OR EXTENSIONS::PROCESS NULL)
On Wednesday, 7/1/98 12:48:51 pm [-1] it was compiled from:
target:code/run-program.lisp
  Created: Saturday, 6/20/98 07:13:08 pm [-1]
  Comment: $Header: /home/david/gitconversion/cvsroot/cxml/glisp/Attic/dep-cmucl-dtc.lisp,v 1.1 2005-03-13 18:02:10 david Exp $
||#

;; (process-exit-code (run-program "/bin/sh" (list "-c" "ls") :wait t :input nil :output nil))

(defun glisp:run-unix-shell-command (command)
  (ext:process-exit-code (ext:run-program "/bin/sh" (list "-c" command) :wait t :input nil :output nil)))

(defmacro glisp::defsubst (name args &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args .,body)))


;;; MP

(export 'glisp::mp/process-yield :glisp)
(export 'glisp::mp/process-wait :glisp)
(export 'glisp::mp/process-run-function :glisp)
(export 'glisp::mp/make-lock :glisp)
(export 'glisp::mp/current-process :glisp)
(export 'glisp::mp/process-kill :glisp)

(defun glisp::mp/make-lock (&key name)
  (pthread::make-lock name))

(defmacro glisp::mp/with-lock ((lock) &body body)
  `(pthread::with-lock-held (,lock)
     ,@body))

(defun glisp::mp/process-yield (&optional process-to-run)
  (declare (ignore process-to-run))
  (PTHREAD:SCHED-YIELD))

(defun glisp::mp/process-wait (whostate predicate)
  (do ()
      ((funcall predicate))
    (sleep .1)))

(defun glisp::mp/process-run-function (name fun &rest args)
  (pthread::thread-create
   (lambda ()
     (apply fun args))
   :name name))

(defun glisp::mp/current-process ()
  'blah)

(defun glisp::mp/process-kill (process)
  (warn "*** Define GLISP:MP/PROCESS-KILL for CMUCL."))

(defun glisp::getenv (string)
  (cdr (assoc string ext:*environment-list* :test #'string-equal)))

