;; $Id$
;; Package definition for figures and math server
;; Copyright (C) 2006 Dr. John A.R. Williams
;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords: LaTeX, figures

;; This file is part of CLEWS

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; A CLEWS web application using the docutils.figures image server

;;; Code:

(in-package :cl-user)

(defpackage :clews.media
  (:documentation "CLEWS math and figure server")
  (:use :cl :clews :jarw.media)
  (:import-from :clews.form #:form-data #:markup-form)
  (:import-from :inet.access-control #:has-permission)
  (:export #:media-manager))
