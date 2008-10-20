;;;; CLEWS: Data Collection Application
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL) 
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: defpackage.lisp,v 1.1 2005/03/10 19:56:27 willijar Exp $

(in-package :cl)

(defpackage :clews.data-collection
  (:documentation
   "Web based data collection application")
  (:use :cl :clews :clews.form :markup)
  (:import-from :inet.access-control #:has-permission
		#:access-controlled-entity)
  (:import-from :inet.http #:remote-user #:response #:form-values)
  (:import-from :jarw.io #:write-log)
  (:import-from :jarw.string #:split-string)
  (:import-from :data-storage  #:data-store-store #:data-store-retrieve)
  (:export #:data-collection-application #:add-form-handler
	   #:form-handler #:static-form #:vote-form #:get-form #:get-data
	   #:can-submit-data #:load-forms))
