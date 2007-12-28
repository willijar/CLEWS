;;;; CLEWS: Forms system -*- Lisp -*-
;;;; Copyright (C) 2003-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL) 
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;; $Id: clews.form.asd,v 1.1 2006/07/19 10:26:36 willijar Exp $

(in-package :asdf)
(defsystem clews.form
    :name "clews.form"
    :description "Form handling"
    :author "Dr. John A.R. Williams"
    :version "0.2"
    :licence "GPL"
    :long-description "Form handling for web applications"
    :depends-on (:inet.http :markup :jarw)
    :components
    ((:file "defpackage")
     (:file "element" :depends-on ("defpackage"))
     (:file "form" :depends-on ("element"))
     (:file "analysis" :depends-on ("defpackage" "element" "form") )))
