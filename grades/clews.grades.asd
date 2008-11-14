;;;; CLEWS MSc Grades handling -*- Lisp -*-
;;;; Copyright (C) 2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;; $Id: clews.grades.asd,v 1.1 2006/09/08 06:29:52 willijar Exp willijar $

(in-package :asdf)
(defsystem clews.grades
    :name "MSc modules and marks Management"
    :description "Modules and Marks EWAS application"
    :author "Dr. John A.R. Williams"
    :version "0.1"
    :licence "BSD Style"
    :depends-on (:clews :clsql :cl-ppcre :clsql-postgresql-socket
                        :flexi-streams)
    :components
    ((:file "defpackage")
     (:file "utility" :depends-on ("defpackage"))
     (:file "patches" :depends-on ("defpackage"))
     (:file "database" :depends-on ("utility"))
     (:file "grades" :depends-on ("database"))
     (:file "projects" :depends-on ("database"))
     (:file "specifications" :depends-on ("database"))
     (:file "reports" :depends-on ("specifications"))))
