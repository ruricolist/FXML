;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GLISP; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Second part of GCL dependent stuff
;;;   Created: 1999-05-25 22:31
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

(in-package :GLISP)

(lisp::clines
 "#include <stdio.h>"
 "#include <unistd.h>"
 "#include <sys/stat.h>"
 "#include <sys/socket.h>"
 "#include <netinet/in.h>"
 "#include <stdlib.h>"
 "#include <fcntl.h>"
 "#include <resolv.h>"
 )

(lisp::defcfun "static object open_inet_socket_aux (object x, object y, char *hostname, int port)" 2
               "FILE *fp;"
               "object stream;"

               "struct hostent *hostinfo;"
               "struct sockaddr_in addr;"
               "int sock;"
               "vs_mark;"
  
               "hostinfo = gethostbyname (hostname);"

               "if (hostinfo == 0)"
               "{"
               "  return Cnil;"
               "}"

               "addr.sin_family = AF_INET;"
               "addr.sin_port = htons (port);"
               "addr.sin_addr = *(struct in_addr*) hostinfo->h_addr;"
               ""
               "sock = socket (PF_INET, SOCK_STREAM, 0);"
               "if (sock < 0)"
               "  return Cnil;"
               ""
               "if (connect (sock, (struct sockaddr *) &addr, sizeof (addr)) != 0)"
               "{"
               "  close (sock);"
               "  return Cnil;"
               "}"


               "fp = fdopen (sock, \"rb+\");"
               "stream = (object)  alloc_object(t_stream);"
               "stream->sm.sm_mode = (short)smm_io;"
               "stream->sm.sm_fp = fp;"
               "stream->sm.sm_object0 = x;"
               "stream->sm.sm_object1 = y;"
               "stream->sm.sm_int0 = stream->sm.sm_int1 = 0;"
               "vs_push(stream);"
               "setup_stream_buffer(stream);"
               "vs_reset;"
               "return stream;"    
               )

(lisp::defentry open-inet-socket-aux (lisp::object lisp::object lisp::string lisp::int)
                (lisp::object "open_inet_socket_aux"))

(lisp::defentry unix/system (lisp::string)
                (lisp::int "system"))

(defun open-inet-socket (hostname port)
  (values (or (open-inet-socket-aux '(unsigned-byte 8)
                                    (format nil "Network connection to ~A:~D" hostname port)
                                    hostname port)
              (error "Cannot connect to `~A' on port ~D."
                     hostname port))
          :byte))
