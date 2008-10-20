;;;; $Id: defpackage.lisp,v 1.1 2005/03/10 20:11:23 willijar Exp $
;;;; Copyright (C) 2004 Dr. John A.R. Williams
;;;; Permission to use with BSD-style license included in the LICENSE file

(in-package :cl-user)

(defpackage :clews.passwd
  (:documentation "The EWAS password management system")
  (:use :cl :clews :dictionary :markup :inet.auth)
  (:import-from :inet.rfc2821 #:send-mail)
  (:import-from :split-sequence #:split-sequence)
  (:import-from :inet.access-control #:has-permission #:*current-user*
		#:permission-denied-error #:access-controlled-entity)
  (:import-from :inet.http #:remote-user #:response)
  (:import-from :inet.uri #:url)
  (:import-from #:clews.form #:markup-form #:form-data #:submitted-action)
  (:export #:passwd-manager))

