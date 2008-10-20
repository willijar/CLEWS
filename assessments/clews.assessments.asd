;;;; Adaptive-tutorial application -*- Lisp -*-
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL) 
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: clews.assessments.asd,v 1.1 2006/09/04 08:45:31 willijar Exp $

(in-package :asdf)
(defsystem clews.assessments
    :name "CLEWS Assessments"
    :description "Web based assessments application"
    :author "Dr. John A.R. Williams"
    :version "0.1"
    :licence "GPL"
    :depends-on (:clews :clews.form :clews.assessment :dictionary :jarw)
    :components
    ((:file "defpackage")	
     (:file "application" :depends-on ("defpackage"))))
