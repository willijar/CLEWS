;; $Id: defpackage.lisp,v 1.1 2006/07/31 07:15:13 willijar Exp willijar $
;; PPackage definition
;; Copyright (C) 2002-2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of CLEWS: Assessment

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :cl-user)

(defpackage :clews.assessment
  (:documentation "Generic online assessment handling")
  (:use :cl :markup :jarw.properties)
  (:import-from :clews.form #:markup-form #:form-data #:name #:form-mark
                #:is-form-element-p
                #:default-value #:element-markup #:datatype #:form-values)
  (:import-from :jarw.string  #:join-strings #:strcat)
  (:import-from :jarw.lib #:when-bind)
  (:import-from :jarw.parse #:format-time #:parse-time)
  (:import-from :jarw.math #:mean #:stddev)
  (:import-from :inet.acl #:access-controlled-entity #:acl #:has-permission
                #:assert-permission )
  (:import-from :dictionary #:initargs #:get-dictionary)
  (:export
   ;; generic assessment API
   #:assessment-attempt-p #:assessment-feedback-p #:assessment-count-p
   #:assessment-attempt-p-reason #:assessment-feedback-p-reason
   #:assessment-count-p-reason #:assessment-reset-p
   #:assessment-status-long #:assessment-should-not-attempt-reason
   #:assessment-status-short
   #:assessment-attempt-markup #:assessment-feedback-markup
   #:assessment-mark-markup
   #:assessment-mark #:assessment-distance-metric
   #:assessment-stub
   ;; dates
   #:deadline-date #:completed #:time-remaining #:timelimit #:timetaken
   #:start-date #:end-date
   #:started
   ;; questionnaire specific
   #:assessment #:questionnaire #:weighting
   #:simple-question #:multiple-choice-q #:multiple-answer-q #:numeric-q
   #:def-dynamic-question #:written-q #:compound-q
   ;;normalisation
   #:assessment-normalisation-weighting #:assessment-normalised-marks
   #:assessment-metadata #:assessment-detail-statistics))


