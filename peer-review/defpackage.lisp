;;;; CLEWS Peer Review Application
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: defpackage.lisp,v 1.1 2006/07/30 17:42:36 willijar Exp $

(defpackage :clews.peer-review
  (:documentation "The peer review application")
  (:use :cl :clews :dictionary :markup)
  (:shadow #:title)
  (:import-from :clews.form
                #:find-form #:defform #:markup-form #:form-data
                #:mark-form-data #:form-mark #:submitted-action
                #:markup-tag #:do-form)
  (:import-from :inet.header #:header-field #:content)
  (:import-from :rfc2822 #:unquoted #:skip-spaces #:token #:quoted-string)
  (:import-from :inet.http
                #:remote-user #:response #:form-values #:redirect
                #:query-values #:url)
  (:import-from :regex #:regex-substitute-string
                #:compile-str #:regex-quote-string)
  (:import-from :jarw.parse #:format-time #:parse-input)
  (:import-from :jarw.string #:split-string)
  (:import-from :jarw.properties #:property)
  (:import-from :jarw.search
                #:index #:unindex #:search-index #:clrindex
                #:keywords #:keyword-index #:ignore-word-p)
  (:import-from :inet.access-control #:has-permission)
  (:import-from :jarw.math #:mean #:stddev)
  (:export #:peer-review #:articles #:reviews #:add-article #:add-review
           #:get-article #:background-index-process #:rem-review
           #:rem-article))

