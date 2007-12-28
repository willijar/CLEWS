;;;; CLEWS: Assessments  -*- Lisp -*-
;;;; Copyright (C) 2003-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL) 
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: clews.assessment.asd,v 1.1 2006/08/08 08:18:05 willijar Exp $

(in-package :asdf)
(defsystem clews.assessment
    :name "clews.assessment"
    :description "Assessment Component for ewas"
    :author "Dr. John A.R. Williams"
    :version "0.1"
    :licence "GPL"
    :long-description "Assessments for EWAS"
    :depends-on (:markup :clews.form :dictionary :inet.http)
    :components
    ((:file "defpackage")
     (:file "utility" :depends-on ("defpackage"))
     (:file "knowledge" :depends-on ("defpackage"))
     (:file "assessment" :depends-on ("defpackage" "utility" "knowledge"))
     (:file "questions" :depends-on ("defpackage" "knowledge"))
     (:file "questionnaire" :depends-on ("questions" "assessment"))))
