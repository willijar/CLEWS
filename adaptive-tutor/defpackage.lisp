;;;; Adaptive Tutorial Application
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: defpackage.lisp,v 1.1 2006/07/30 17:39:22 willijar Exp $

(in-package :cl-user)

(defpackage :clews.adaptive-tutor
  (:nicknames "Adaptive Tutor")
  (:documentation
   "Package for managing and querying knowledge domain and user domain
 information for the Adaptive Tutor")
  (:use :cl :clews :markup :jarw.properties :clews.assessment)
  (:import-from :jarw.math #:mean #:stddev)
  (:import-from :jarw.string #:split-string)
  (:import-from :jarw.parse #:format-time)
  (:import-from :jarw.io #:write-log)
  (:import-from :jarw.search
                #:keywords #:keyword-index #:index #:search-index
                #:ignore-word-p)
  (:import-from :jarw.lib #:while #:when-bind)
  (:import-from :dictionary
                #:get-dictionary #:map-dictionary
                #:dictionary-count #:dictionary)
  (:import-from :inet.http #:response #:remote-user #:form-values #:redirect)
  (:import-from :inet.access-control #:has-permission)
  (:import-from :clews.form #:form-data #:markup-form #:submitted-action)
  (:export #:adaptive-tutor #:concept #:defconcept #:load-concepts
           #:compile-concepts ))