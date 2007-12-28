;; $Id: defpackage.lisp,v 1.2 2006/09/08 06:16:48 willijar Exp $
;; Package Definitions
;; Copyright (C) 2002-2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of CLEWS Form Handling Library

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(in-package :cl-user)

(defpackage :clews.form
  (:documentation "Generic form handling.")
  (:nicknames :form)
  (:use :cl :markup)
  (:import-from :inet.http #:request #:form-values)
  (:import-from :jarw.string #:split-string)
  (:import-from :jarw.parse #:parse-number #:parse-time #:format-time
                #:parse-input #:invalid-input #:format-output )
  (:import-from :jarw.math #:mean #:stddev)
  (:import-from :jarw.lib #:when-bind)
  (:export #:defform #:make-form #:register-form #:markup-form #:find-form
	   #:form-data #:date #:submitted-action #:input-validation
	   #:output-value #:do-form
	   #:form-mark #:mark-form-data #:form-analysis-markup
	   #:get-form-element #:validate-element-data #:invalid-input #:date
	   #:+countries+ #:+uk-countries+ #:+eu-countries+))