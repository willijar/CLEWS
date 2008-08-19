;;;; CLEWS Peer Review Application
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: defpackage.lisp,v 1.1 2006/07/30 17:42:36 willijar Exp $

(defpackage :jarw.command
  (:use :cl)
  (:import-from :inet.access-control
                #:*current-user* #:username #:has-permission
                #:permission-denied-error)
  (:import-from  :jarw.parse #:parse-input #:format-output #:date)
  (:export #:command-processor #:defcommand
           #:*log-commands* #:command-log #:log-command
           #:playback-commands #:undo #:redo #:configure
           #:can-execute-command-p))

(defpackage :clews.views
  (:use :cl :clews :markup)
  (:import-from :inet.access-control
                #:*current-user* #:username #:has-permission)
  (:import-from :jarw.mop #:class-precedence-list)
  (:export #:views #:get-view #:defview #:can-view-p #:handle-view))

(defpackage :clews.groups
  (:documentation "The Group assessment application")
  (:use :cl :clews :dictionary :markup :jarw.command :clews.views)
  (:import-from :inet.access-control
                #:*current-user* #:username #:assert-permission
                #:permission-denied-error)
  (:import-from :clews.form
                #:find-form #:defform #:markup-form #:form-data
                #:form-mark #:submitted-action
                #:markup-tag #:do-form-with-confirmation)
  (:import-from :inet.http
                #:remote-user #:response #:form-values #:redirect
                #:query-values #:url)
  (:import-from :jarw.parse #:parse-input #:format-output)
  (:import-from :jarw.string #:split-string #:strcat)
  (:import-from :inet.access-control
                #:has-permission #:has-permission #:username)
  (:import-from :jarw.math #:mean #:stddev)
  (:export #:group-assessment ))

