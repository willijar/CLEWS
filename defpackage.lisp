;;;; Common Lisp Educational Web Application Serve (CLEWS)
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: defpackage.lisp,v 1.2 2007/07/19 09:42:37 willijar Exp $

(in-package :cl-user)

(defpackage :clews
  (:documentation
   "Top level web server package building on the (modified) araneida server")
  (:use :cl :markup :dictionary :jarw.properties)
  (:import-from :inet.acl
                #:access-controlled-entity #:acl
                #:has-permission #:assert-permission #:permission-denied-error
                #:username #:roles  #:has-role #:*current-user*)
  (:import-from :inet.authentication
                #:encrypted-authenticator #:stored-credentials
                #:credentials-valid-p)
  (:import-from :jarw.string #:split-string)
  (:import-from :jarw.io  #:write-log)
  (:import-from :jarw.parse #:format-time #:parse-time)
  (:import-from :jarw.lib #:when-bind #:while)
  (:import-from :clews.form #:defform #:submitted-action #:form-data
                #:make-form #:markup-form)
  (:import-from :inet.uri #:url #:merge-url)
  (:shadowing-import-from :inet.uri  #:path)
  (:import-from #:inet.header #:header #:header-field)
  (:import-from #:inet.body #:content)
  (:import-from :inet.http
                #:request #:response #:form-values #:cookie #:publish-handler
                #:make-response	#:remote-user #:remote-addr
                #:with-form-values)
  (:export #:application
           #:publish #:published-methods
           #:users #:groups #:username #:get-users
           #:display-name #:email-address #:make-email
           #:usernames #:roles #:add-application-handler #:add-file-handler
           #:create-password #:create-crypt-pwd
           #:make-simple-user-source #:make-simple-pwd-source
           #:add-user #:get-user #:username #:add-user-login #:check-login
           #:add-roles #:rem-roles #:user #:user-component-properties
           #:user-component-preferences #:application-handler
           #:has-role #:get-users #:user-source
           #:www-authentication-handler #:cookie-authentication-handler
           #:authorization-handler #:error-handler
           #:no-authentication-handler #:response-handler #:log-handler
           #:render-page #:id #:user-preference
           ;;plugins
           #:plugin #:plugin-markup #:user-component-preferences
           #:annotate-plugin #:chatterbox-plugin #:who-plugin
           #:error-reporter-plugin #:quotes-plugin #:polls-plugin
           #:handle-plugins))

