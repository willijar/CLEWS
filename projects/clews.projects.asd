;;;; CLEWS Student Projects Management -*- Lisp -*-
;;;; Copyright (C) 2004-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL) 
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: clews.projects.asd,v 1.1 2005/03/10 20:14:01 willijar Exp $

(in-package :asdf)
(defsystem :clews.projects
    :name "Project Management"
    :description "Project Management CLEWS application"
    :author "Dr. John A.R. Williams"
    :version "0.1"
    :licence "GPL"
    :depends-on (:clews :inet)
    :components 
    ((:file "defpackage")
     (:file "forms" :depends-on ("defpackage"))
     (:file "project" :depends-on ("defpackage"))
     (:file "projects" :depends-on ("forms" "project"))))
