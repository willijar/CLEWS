;;;; -*- lisp -*-

(in-package :asdf)

(defsystem :clews.articles
  :name "CLEWS Articles"
  :description "Article handling system"
  :author "Dr. John A.R. Williams"
  :version "0.1"
  :licence "GPL"
  :depends-on (:jarw :media :clews :docutils :inet :clews.assessment
               :flexi-streams)
  :components
  ((:file "defpackage")
   (:file "docutils" :depends-on ("defpackage"))
   (:file "collection" :depends-on ("defpackage" "docutils"))
   (:file "article" :depends-on ("collection" "docutils" "defpackage"))
   (:file "application" :depends-on ("article" "collection"))
   (:file "questionnaires" :depends-on ("defpackage"))
   (:file "tutorial-articles" :depends-on ("article" "questionnaires"))
   (:file "tutorial-application"
          :depends-on ("application" "tutorial-articles"))))