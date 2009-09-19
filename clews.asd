;;;; Common Lisp Educational Web System (CLEWS) -*- Lisp -*-
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: clews.asd,v 1.1 2006/07/19 10:29:11 willijar Exp $

(in-package :asdf)
(asdf:defsystem clews
  :name "CLEWS"
  :description "Common Lisp Educational Web System (CLEWS)"
  :author "Dr. John A.R. Williams"
  :version "0.2"
  :licence "GPL v3"
  :long-description "A web application framework for dynamic
educational web based applications"
  :depends-on (:jarw :inet :inet.http :md5
                      :markup :dictionary :clews.form)
  :components
  ((:file "defpackage")
   (:file "component" :depends-on ("defpackage"))
   (:file "authentication" :depends-on ("defpackage"))
   (:file "application"
          :depends-on ("authentication" "user" "component" "plugin"))
   (:file "plugin" :depends-on ("defpackage" "component"))
   (:file "user" :depends-on ("defpackage"))
   (:module "plugins"
            :depends-on ("plugin" "defpackage" "user")
            :components ((:file "annotate")
                         (:file "chatterbox")
                         (:file "error-reporter")
                         (:file "quotes")
                         (:file "polls")
                         (:file "who")))))
