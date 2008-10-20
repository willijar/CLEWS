;; $Id: defpackage.lisp,v 1.1 2006/09/04 08:46:14 willijar Exp $
;; CLEWS Assessment package
;; Copyright (C) 2002-2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of CLEWS Assessments

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(defpackage :clews.assessments
  (:nicknames "CLEWS Assessments")
  (:documentation
"Package for managing and querying online assessments")
  (:use :cl :clews :markup :jarw.properties :clews.assessment)
  (:import-from :clews.assessment #:knowledge #:datestring #:datespec
		#:description #:default-start-date #:default-deadline-date
		#:default-feedback-date #:knowledge)
  (:import-from :jarw.lib #:while #:when-bind)
  (:import-from :jarw.math #:mean)
  (:import-from :jarw.string #:split-string #:strcat)
  (:import-from :dictionary
		#:dictionary #:serializable-dictionary #:initargs
		#:get-dictionary #:dictionary-keys #:map-dictionary
		#:search-dictionary)
  (:import-from :inet.http #:response #:remote-user #:form-values #:redirect)
  (:import-from :inet.access-control #:has-permission #:username
		#:*current-user* #:acl)
  (:import-from :clews.form #:form-data #:markup-form #:submitted-action)
  (:import-from :clews.assessment #:assessment-status-table #:completed)
  (:export #:assessment-application))