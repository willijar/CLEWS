;;;; CLEWS: Assessments  -*- Lisp -*-
;;;; Copyright (C) 2003-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL) 
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: clews.assessment.asd,v 1.1 2005/03/10 19:52:35 willijar Exp $

(in-package :asdf)
(defsystem :clews.assessment.discussion
    :name "clews.assessment.discussion"
    :description "Discussion Assessment Component for CLEWS"
    :author "Dr. John A.R. Williams"
    :version "0.1"
    :licence "GPL"
    :long-description "Assessments for EWAS"
    :depends-on (:clews.assessment :inet)
    :components
    ((:file "defpackage")
     (:file "discussion" :depends-on ("defpackage"))))
