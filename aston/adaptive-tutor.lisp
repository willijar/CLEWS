http://www.sics.se/contiki/;;;; Aston Web Applications
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: adaptive-tutor.lisp,v 1.15 2005/03/10 19:54:25 willijar Exp $

(in-package :aston)

(defvar *tutorial*
  (make-instance 'clews.adaptive-tutor:adaptive-tutor
                 :id :adaptive-tutor
                 :user-dictionary *user-source*
                 :acl '((:view . (:all))
                        (:student . (:all))
                        (:tutor . ("willijar") )
                        (:admin . ("willijar")))
                 :pwd-source *pwd-source*
                 :plugins (list *annotate-plugin* *polls-plugin* *who-plugin*
                                *chatterbox-plugin* *quotes-plugin*
                                *error-reporter-plugin*))
  "Our local tutorial")

(setq clews.adaptive-tutor::*adaptive-tutor* *tutorial*)

(setf (logical-pathname-translations "tutorials")
      '(("EE401B;**;*" "/home/willijar/teaching/modules/EE401B/**/*.lisp")
        ("EE4008;*;" "/home/willijar/teaching/modules/EE4008/*/*.lisp")
        ("*;*" "/home/willijar/teaching/tutorials/*/**/*.lisp") ))

(defvar *tutorial-sources*
  (mapcar #'(lambda(tut) (translate-logical-pathname
                          (strcat "tutorials:" tut ";*")))
	  '("help" ;
	    ;"photonics"
	    "EE401B;0-introduction"
	   "EE401B;1-modulation-basics"
	   "EE401B;2-continuous-wave-modulation"
	   "EE401B;3-pulse-modulation"
	   "EE401B;4-baseband-transmission"
	   "EE401B;5-passband-transmission"
	   "EE401B;6-advanced-modulation-applications"
	   ;"EE401B;labs"
	   ;"EE4008;assessment"
	    "EE4008;0-overview")))

(defpackage :aston-tutorial
  (:use :cl :markup)
  (:import-from :clews.assessment
                #:questionnaire #:def-dynamic-question
                #:simple-question #:multiple-choice-q
                #:multiple-answer-q #:numeric-q #:written-q)
  (:import-from :jarw.math #:erfc #:erfcinv)
  (:import-from :clews.adaptive-tutor #:defconcept))

(defun load-tutorial(name &key (compile t))
  (let ((*package* (find-package :aston-tutorial)))
    (load-concepts (translate-logical-pathname
                    (format nil "tutorials:~A;*" name))
                   *tutorial*
                   :run-external-programs compile)))

(defun load-tutorials(&optional (sources *tutorial-sources*)
                      run-external-programs)
  (let ((*package* (find-package :aston-tutorial)))
      (with-tutor(*tutorial*)
        (dolist (tutorial-path sources)
          (format t ";Loading tutorials ~S~%" tutorial-path)
          (load-concepts tutorial-path *tutorial*
                         :run-external-programs run-external-programs)))))

#|
(defvar *tutorial-collection*
  (make-instance
   'clews.articles:article-collection
   :directory #p"/home/willijar/teaching/modules/EE401B/rst-test/"
   :element-type 'clews.articles::tutorial-article
   :acl '((:view :all)
          (:add :tutor :admin)
          (:admin :admin))))

(defvar *tutorial-application*
  (make-instance
   'clews.articles:clews-articles
   :articles  *tutorial-collection*
   :id :tutorials
   :user-dictionary *user-source*
   :acl '((:view . (:all))
          (:student . (:all))
          (:tutor . ("willijar") )
          (:admin . ("willijar")))
   :pwd-source *pwd-source*))

(publish *tutorial-application* "/tutorials/")

|#