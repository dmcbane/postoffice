;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          postoffice.asd
;;;; Purpose:       ASDF definition file for Postoffice
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Sep 2002
;;;;
;;;; $Id: postoffice.asd,v 1.2 2002/10/09 23:28:41 kevin Exp $
;;;;
;;;; This file, part of cl-postoffice, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; cl-postoffice users are granted the rights to distribute and use this software
;;;; as governed by the terms of the GNU Lesser General Public License 
;;;; (http://www.gnu.org/licenses/lgpl.html)
;;;; *************************************************************************

(in-package :asdf)

(defsystem :postoffice
  :name "cl-postoffice"
  :author "Franz, Inc"
  :version "CVS.2002.10.09"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "GNU Lesser General Public License"
  :description "Franz's Post Office Package"
  :long-description "Post Office provides an interface to the SMTP, POP, and IMAP servers. It uses the ACL-COMPAT library for use with non-Allegro CL implementations."
  
  :perform (load-op :after (op postoffice)
	    (pushnew :postoffice cl:*features*))
  
  :components
  ((:file "package")
   (:file "smtp" :depends-on ("package"))
   (:file "imap" :depends-on ("package")))
  #-allegro :depends-on #-allegro (:acl-compat)
  )

(when (ignore-errors (find-class 'load-compiled-op))
  (defmethod perform :after ((op load-compiled-op) (c (eql (find-system :postoffice))))
    (pushnew :postoffice cl:*features*)))

