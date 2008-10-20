;;;; $Id: defpackage.lisp,v 1.5 2005/03/10 19:55:14 willijar Exp $
;;;; Copyright (C) 2004 Dr. John A.R. Williams
;;;; Permission to use with BSD-style license included in the LICENSE file

(in-package :cl-user)

(defpackage :clews.cv
  (:documentation "The EWAS CV management system")
  (:use :cl :clews :dictionary :markup)
  (:import-from :split-sequence #:split-sequence)
  (:import-from :jarw.properties #:property-subset #:property)
  (:import-from :jarw.parse #:format-time)
  (:import-from :jarw.string #:strcat)
  (:import-from :inet.access-control #:has-permission #:*current-user*)
  (:import-from :inet.http #:redirect #:form-values #:response)
  (:import-from #:clews.form #:markup-form #:form-data #:submitted-action
		#:find-form #:register-form #:element-markup #:+countries+)
  (:export #:cv-manager))
