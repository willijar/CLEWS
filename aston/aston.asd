;;;; Aston Web Applications -*- lisp -*-
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: aston.asd,v 1.1 2005/03/10 19:54:25 willijar Exp $

(in-package :asdf)

(defsystem :aston
  :name "aston"
  :description
  "Configuration of the Aston CLEWS Server"
  :author "Dr. John A.R. Williams"
  :version "0.1"
  :licence "GPL"
  :depends-on (:clews
               :sql-dictionary :clsql-postgresql-socket :flexi-streams
               :clews.publications :clews.discussion
               :clews.peer-review :clews.passwd ;;:clews.articles
               :clews.data-collection :clews.grades
               :clews.assessments :clews.assessment.socratic :clews.media
               :clews.articles)
  :components
  ((:file "defpackage")
   (:file "configuration" :depends-on ("defpackage"))
   (:file "user"  :depends-on ("configuration"))
   (:file "server"
          :depends-on
          ("configuration" "peer-review" "cspeer-review" "tutorials"
                           "discussions" "publications" "directory"
                           "projects" "assessments" "media"
                           "passwd" "forms" "grades"))
   (:file "forms" :depends-on ("configuration" "defpackage") )
   (:file "directory" :depends-on ("configuration"))
   (:file "tutorials" :depends-on ("configuration" "convolutional-coding"))
   (:file "publications" :depends-on ("configuration"))
   (:file "discussions" :depends-on ("configuration"))
   (:file "peer-review" :depends-on ("configuration"))
   (:file "cspeer-review" :depends-on ("peer-review"))
   (:file "media" :depends-on ("configuration"))
   (:file "projects" :depends-on ("configuration" "grades"))
   (:file "grades" :depends-on ("configuration"))
   (:file "assessments" :depends-on ("configuration"))
   (:file "specifications" :depends-on ("configuration"))
   (:file "convolutional-coding")
   (:file "passwd" :depends-on ("configuration"))))