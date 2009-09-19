;; $Id: defpackage.lisp,v 1.1 2007/07/26 08:54:00 willijar Exp willijar $
;; Package definition for article applications library
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of the CLEWS Article Application library

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;

;;; Code:

(defpackage :clews.articles
  (:documentation "Article type handling for Clews")
  (:use :cl)
  (:import-from :jarw.io #:read-headers #:write-headers #:*search-path*
                #:find-file)
  (:import-from :dictionary #:get-dictionary #:dictionary-keys #:rem-dictionary
                #:update-record-from-instance #:update-instance-from-record
                #:map-dictionary #:dictionary-count
                #:search-dictionary #:logfile-dictionary
                #:directory-dictionary #:deferrable-dictionary
                #:with-delayed-updates #:child-dictionary)
  (:import-from :data-format-validation
                #:format-output #:parse-input #:date #:invalid-input
                #:filename)
  (:import-from :jarw.parse
                 #:parsed-fields #:standard-parsed-class #:field-value
                #:field-values #:unparsed-values  #:field-missing)
  (:import-from :jarw.string #:split-string #:strcat)
  (:import-from :jarw.lib #:when-bind)
  (:import-from :jarw.debug #:debug-log #:debug-on #:debug-off)
  (:import-from :jarw.port  #:make-weak-pointer #:weak-pointer-value
                #:make-mutex #:with-lock)
  (:import-from :inet.http #:query-values)
  (:import-from :docutils #:add-child #:report #:document #:read-document
                #:read-lines #:visit-node #:copy-of-node #:transforms #:node
                #:part-append #:new-document #:*unknown-reference-resolvers*
                #:settings)
  (:import-from :docutils.parser.rst  #:rst-reader #:recursive-rst-reader
                #:def-directive #:&option #:&content)
  (:import-from :docutils.writer.html #:html-writer-using-media-server)
  (:import-from :docutils.writer.latex #:latex-writer #:latex-output-stream)
  (:import-from :docutils.transform #:evaluate)
  (:import-from :rfc2822 #:skip-spaces #:token
                #:quoted-string #:unquoted #:quoted)
  (:import-from :inet.access-control #:access-controlled-entity #:acl
                #:has-permission #:assert-permission #:*current-user*)
  (:import-from :inet.uri #:uri-escape #:uri-unescape)
  (:import-from #:clews
                #:application #:published-methods #:users
                #:user-preference #:response-handler
                #:user-component-preferences #:user-component-properties)
  (:import-from :clews.form #:markup-form #:form-data)
  (:import-from :clews.assessment #:assessment-stub #:deadline-date
                #:feedback-date #:default-deadline-date
                #:default-feedback-date #:assessment-student-status)
  (:import-from :jarw.search #:keywords #:keyword-index #:search-index
                #:index #:unindex)
  (:import-from :clews :user)
  (:import-from :split-sequence #:split-sequence)
  (:export #:article #:article-collection #:get-article
           #:clews-tutorial #:tutorial-article #:tutorial-collection))