;;;; CLEWS Student Projects Management 
;;;; Copyright (C) 2004-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL) 
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: defpackage.lisp,v 1.3 2005/03/10 20:14:01 willijar Exp $

(in-package :cl-user)

(defpackage :clews.projects
  (:documentation "The EWAS project management system")
  (:use :cl :clews :dictionary :markup)
  (:import-from :split-sequence #:split-sequence)
  (:import-from :jarw.lib #:when-bind)
  (:import-from :jarw.parse #:parse-number #:format-time)
  (:import-from :jarw.properties #:property-subset #:property)
  (:import-from :jarw.string #:split-string #:join-strings #:strcat)
  (:import-from :inet.access-control
		#:has-permission #:*current-user*
		#:permission-denied-error #:acl)
  (:import-from :inet.http #:form-values #:query-values #:response #:redirect)
  (:import-from :clews.form
		#:markup-form #:form-data #:submitted-action #:register-form
		#:find-form #:mark-form-data)
  (:export #:project-manager))
