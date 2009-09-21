;;;; Publications Application
;;;; Copyright (C) 2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: defpackage.lisp,v 1.4 2005/08/07 08:27:43 willijar Exp $

(in-package :cl-user)

(defpackage :clews.publications
  (:documentation "The CLEWS project management system")
  (:use :cl :clews :dictionary :markup)
  (:import-from :split-sequence #:split-sequence)
  (:import-from :jarw.lib #:when-bind)
  (:import-from :data-format-validation
                #:parse-number #:parse-input #:format-time
                #:is-nil-string #:invalid-input)
  (:import-from :jarw.parse #:is-prefix-p)
  (:import-from :jarw.properties #:property-subset #:property)
  (:import-from :clews.table #:table-row-markup)
  (:import-from :jarw.string #:split-string #:join-strings #:strcat)
  (:import-from :jarw.search #:keywords)
  (:import-from :inet.access-control
                #:has-permission #:*current-user* #:permission-denied-error
                #:acl #:access-controlled-entity)
  (:import-from :inet.http #:cookie #:form-values)
  (:import-from :clews.form
                #:markup-form #:form-data #:submitted-action
                #:input-validation)
  (:import-from :clews.table
                #:web-table #:field-specifications #:accept-form-data
                #:changes-markup #:table-datatype #:table-search-func
                #:table-form #:field-markup #:table-sort-func #:search-form)
  (:export #:publications-manager #:publication-source))
