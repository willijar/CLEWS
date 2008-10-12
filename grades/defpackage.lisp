;; $Id: defpackage.lisp,v 1.1 2006/09/08 06:31:30 willijar Exp willijar $
;; Package definition
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of CLEWS Grades

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :cl-user)

(defpackage :clews.grades
  (:documentation "The EWAS project management system")
  (:use :cl :clews :dictionary :markup)
  (:import-from :clsql #:def-view-class #:update-records-from-instance
                #:instance-refreshed #:update-instance-from-records)
  (:import-from :jarw.lib #:when-bind)
  (:import-from :jarw.string #:split-string #:join-strings #:strcat)
  (:import-from :jarw.properties #:property-subset #:property)
  (:import-from :jarw.parse #:parse-number)
  (:import-from :inet.access-control #:has-permission #:assert-permission
                #:*current-user*
		#:permission-denied-error #:acl #:access-controlled-entity)
  (:import-from :inet.http #:form-values #:query-values #:response #:redirect)
  (:import-from :clews.form
		#:markup-form #:form-data #:submitted-action #:register-form
		#:find-form #:mark-form-data #:form-mark)
  (:export #:grades-manager #:projects-manager))