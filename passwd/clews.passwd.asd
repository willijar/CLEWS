;;;; CLEWS password management -*- Lisp -*-
;;;; Copyright (C) 2004-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL) 
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: clews.passwd.asd,v 1.1 2005/03/10 20:11:23 willijar Exp $

(in-package :asdf)

(defsystem :clews.passwd
    :name "discussion"
    :description "Discussion Groups CLEWS application"
    :author "Dr. John A.R. Williams"
    :version "0.1"
    :licence "GPL"
    :depends-on (:clews)
    :components 
    ((:file "defpackage")     
     (:file "passwd" :depends-on ("defpackage"))))
