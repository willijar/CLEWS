;;;; CLEWS: -*- Lisp -*-
;;;; Copyright (C) 2004-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: clews.discussion.asd,v 1.1 2006/08/21 07:11:58 willijar Exp $

(in-package :asdf)

(defsystem :clews.discussion
    :name "discussion"
    :description "Discussion Groups CLEWS application"
    :author "Dr. John A.R. Williams"
    :version "0.1"
    :licence "GPL"
    :depends-on (:clews :inet :regex :docutils :docutils.extensions)
    :components
    ((:file "defpackage")
     (:file "group" :depends-on ("defpackage"))
     (:file "discussions" :depends-on ("group" "user" "formatting"))
     (:file "formatting" :depends-on ("defpackage"))
     (:file "user"  :depends-on ("defpackage"))))
