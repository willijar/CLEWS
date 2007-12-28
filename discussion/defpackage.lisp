;;;; CLEWS Discussion System
;;;; Copyright (C) 2004-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL) 
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: defpackage.lisp,v 1.1 2006/08/21 07:12:02 willijar Exp $

(in-package :cl-user)

(defpackage :clews.discussion
  (:documentation "The CLEWS discussion system")
  (:use :cl :clews :dictionary :markup)
  (:import-from :jarw.properties #:property-subset #:property)
  (:import-from :inet.access-control #:has-permission #:*current-user*
		#:permission-denied-error #:access-controlled-entity)
  (:import-from :inet.nntp #:news-source #:group)
  (:import-from :jarw.string #:split-string #:write-folded)
  (:import-from :jarw.parse #:format-time)
  (:import-from :inet.http #:form-values #:query-values #:redirect)
  (:import-from #:clews.form #:markup-form #:form-data #:submitted-action)
  (:import-from :inet.message #:message)
  (:import-from :inet.mbox #:mbox #:unix-mbox #:mbox-threading
		#:article  #:index
		#:first-article-number #:last-article-number #:number-articles
		#:field-search #:post-article #:stat
		#:cancel-article #:indexed-fields #:replies #:in-reply-to)
  (:shadowing-import-from :inet.mbox #:head)
  (:import-from :inet.header #:header-field #:header
		#:make-field-specifications #:merge-field-specifications)
  (:import-from :inet.body #:content)
  (:shadow #:author)
  (:shadowing-import-from :inet.body #:body)
  (:export #:discussion-groups #:directory-news-source))

