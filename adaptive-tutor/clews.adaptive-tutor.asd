;;;; Adaptive-tutorial application -*- Lisp -*-
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: clews.adaptive-tutor.asd,v 1.1 2006/07/30 17:40:24 willijar Exp $

(in-package :asdf)

(defsystem clews.adaptive-tutor
  :name "CLEWS Adaptive Tutor"
  :description "Support for Web based adaptive tutorials"
  :author "Dr. John A.R. Williams"
  :version "0.1"
  :licence "GPL"
  :depends-on (:clews :clews.form :clews.assessment)
  :components
  ((:file "defpackage")
   (:file "concept-class" :depends-on ("defpackage"))
   (:file "user" :depends-on ("concept"))
   (:file "adaptive-tutor-class" :depends-on ("defpackage"))
   (:file "status-plugin" :depends-on ("adaptive-tutor-class" "defpackage"))
   (:file "adaptive-tutor-handlers"
          :depends-on ("status-plugin" "user" "concept"
                                       "adaptive-tutor-class"))
   (:file "analysis-handlers"
          :depends-on ("user" "adaptive-tutor-handlers"))
   (:file "concept"
          :depends-on ("concept-class" "adaptive-tutor-class"
                                       "defpackage"))))
