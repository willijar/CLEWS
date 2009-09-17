;;;; $Id: defpackage.lisp,v 1.1 2006/12/03 08:37:21 willijar Exp willijar $
;;;; Copyright (C) 2003 Dr. John A.R. Williams, J.A.R.Williams@blueyonder.co.uk

;;;; This file is part of the Aston EWAS application configuration


(in-package :cl-user)

(defpackage :aston
  (:documentation
   "Top level web server package building on the inet server")
  (:use :cl :dictionary :jarw.properties :markup)
  (:import-from :inet.http #:httpd #:remote-user #:publish-handler
		#:form-values)
  (:import-from :inet.server #:start-server #:stop-server)
  (:import-from :inet.uri #:url #:merge-url #:hostname #:port)
  (:import-from :inet.smtp #:send-mail)
  (:import-from :jarw.string #:strcat)
  (:import-from :jarw.parse #:format-time)
  (:import-from :data-storage #:file-data-store #:data-store-retrieve)
  (:import-from :clews #:user-component-properties #:user-source #:usernames
		#:user #:username #:roles)
  (:import-from :clews.form #:find-form #:defform #:register-form #:make-form)
  #+nil(:import-from :clews.adaptive-tutor
                     #:load-concepts #:adaptive-tutor #:with-tutor)
  (:import-from :clews.data-collection
		#:data-collection-application #:static-form
		#:form-handler #:get-form #:add-form-handler #:get-data
		#:get-extra-data #:can-submit-data #:storage #:load-forms)
  (:import-from :clews.grades #:grades-manager #:projects-manager #:*db*)
  (:import-from :clews.assessments #:assessment-application)
  #+nil(:import-from :clews.db-manager #:db-manager #:table))
