;;;; Publications Management Application -*- Lisp -*-
;;;; Copyright (C) 2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL) 
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: clews.publications.asd,v 1.1 2005/03/10 20:17:50 willijar Exp $

(in-package :asdf)
(defsystem :clews.publications
    :name "Publications Management"
    :description "Publication Management EWAS application"
    :author "Dr. John A.R. Williams"
    :version "0.1"
    :licence "GPL"
    :depends-on (:clews :inet :clews.table :clews.form)
    :components 
    ((:file "defpackage")
     (:file "tables" :depends-on ("defpackage"))
     (:file "publications" :depends-on ("tables"))))
