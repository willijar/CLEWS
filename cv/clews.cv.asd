;;;; CLEWS: Annotations plugin ;;; -*- Lisp -*-
;;;; Copyright (C) 2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL) 
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: clews.cv.asd,v 1.1 2005/03/10 19:55:14 willijar Exp $
(in-package :asdf)
(defsystem clews.cv
    :name "CV Management"
    :description "CV Management CLEWS application"
    :author "Dr. John A.R. Williams"
    :version "0.1"
    :licence "BSD Style"
    :depends-on (:clews :inet :jarw :split-sequence :clews.form)
    :components 
    ((:file "defpackage")
     (:file "forms" :depends-on ("defpackage"))
     (:file "cvs" :depends-on ("forms"))))
