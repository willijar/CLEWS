;; -*- Lisp -*-
;; $Id$
;; System definition for figures and math server
;; Copyright (C) 2006 Dr. John A.R. Williams
;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords: LaTeX, figures

;; This file is part of CLEWS

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; A CLEWS web application using the docutils.figures image server

;;; Code:

(in-package :asdf)
(defsystem clews.media
    :name "CLEWS Media"
    :description "CLEWS media  and math server"
    :author "Dr. John A.R. Williams"
    :version "0.2"
    :licence "GPL"
    :depends-on (:clews :clews.form :media)
    :components
    ((:file "defpackage")
     (:file "figures" :depends-on ("defpackage"))))
