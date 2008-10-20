;;;; CLEWS: Annotations plugin -*- Lisp -*-
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL) 
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: clews.data-collection.asd,v 1.1 2005/03/10 19:56:27 willijar Exp $ 

(in-package :asdf)

(defsystem :clews.data-collection
    :name "CLEWS data-collection Application"
    :description "Web based data collection"
    :author "Dr. John A.R. Williams"
    :version "0.1"
    :licence "GPL"
    :depends-on (:clews :dictionary)
    :components
    ((:file "defpackage")
     (:file "data-collection-application" :depends-on ("defpackage"))
     (:file "form-handler"  :depends-on
	    ("data-collection-application" "defpackage"))))

