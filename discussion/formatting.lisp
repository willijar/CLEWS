;; $Id: formatting.lisp,v 1.1 2006/08/21 07:12:10 willijar Exp willijar $
;; REstructured text formatting for message bodies
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords: CLEWS, discussion

;; This file is part of CLEWS Peer Review Application

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; Specialisation of docutils restructured text reader and writers

;;; Code:


(in-package :clews.discussion)

(defclass rst-reader(docutils.parser.rst:rst-reader)
  ()
  (:documentation "Specialised restructured article text reader for peer review"))


(defmethod docutils:transforms((reader rst-reader))
    '(docutils.transform:final-checks
      docutils.transform:filter-messages
      docutils.transform:substitutions
	    docutils.transform:chained-targets
	    docutils.transform:anonymous-hyperlinks
	    docutils.transform:indirect-hyperlinks
	    docutils.transform:footnotes
	    docutils.transform:external-targets
	    docutils.transform:internal-targets))

(defclass html-writer(docutils.writer.html:html-writer)
  ()
  (:documentation ""))


(defmethod docutils.utilities::read-lines((msg message))
  (docutils.utilities::read-lines (body msg)))

;;(defmethod docutils::new-document((article article))
;;  (make-instance 'docutils.nodes::document :source-path  ?? :settings ??))

(defmethod parsed-body((msg message))
  (let* ((reader (make-instance 'rst-reader))
         (writer (make-instance 'html-writer))
         (docutils.writer.html::*section-level* 2))
    (setf (docutils:document writer) (docutils:read-document msg reader))
    (with-output-to-string(os)
      (docutils:write-part writer 'docutils.writer.html:body os))))

