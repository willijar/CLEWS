;; $Id: collection.lisp,v 1.1 2007/07/26 08:53:56 willijar Exp willijar $
;; Docutils extensions for articles
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords:

;; This file is part of the CLEWS Article application library

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;;; We will have one reader and writer associated witrh each collection
;;; which willuse locks to protect against reentrance


(defvar *document-error-hook*  nil
  "When *document-error-hook* is non-nil it is called prior to normal handlking of document errors")

(in-package :clews.articles)

(defclass article-rst-reader(docutils.parser.rst:rst-reader)
  ()
  (:documentation "Specialisation parser for articles"))

(defmethod docutils::read-document(source (reader article-rst-reader))
  (let ((docutils.parser.rst::*default-reader-package* (find-package :markup)))
    (call-next-method)))

(defmethod transforms((reader article-rst-reader))
  (nconc (list 'docutils.transform:fignum)
         (call-next-method)))

(defclass article-writer()
  ((lock :initform (make-mutex) :reader mutex)
   (errors :initarg :show-errors :reader errors :initform nil :type boolean))
  (:documentation "Mix in for article docutils writers"))

(defclass article-html-writer(html-writer-using-media-server article-writer)
  ()
  (:documentation "Article specific writer does not output errors"))

(defmethod visit-node((writer article-html-writer)
                      (node docutils.nodes:section))
  (when (or (errors writer)
            (not (equalp "Docutils System Messages" (docutils::title node))))
    (call-next-method)))

(defmethod visit-node  ((writer article-html-writer)
                        (node docutils.nodes:system-message))
  (if  (errors writer)
       (call-next-method)
       (part-append
        "
<p class=\"error\">System Message.
You need editorial access to view the details</p>
")))

(defmethod visit-node ((writer article-html-writer)
                       (node docutils.nodes:problematic))
  (if  (errors writer)
       (call-next-method)
       (part-append
        "
<span class=\"error\">Problematic.
You need editorial access to view the details</p>
")))

(defgeneric write-sections(writer document list-of-sections)
  (:documentation "Given a list of sections made of a stream and a
list of part names, write these sections to the streams")
  (:method(writer  document sections)
    (docutils:visit-node writer document)
    (dolist(section sections)
      (let ((os (car section)))
        (dolist(part (rest section))
          (docutils:write-part writer part os)))))
  (:method((writer article-writer) document sections)
    (with-lock((mutex writer)) (call-next-method))))

(defclass article-latex-writer(latex-writer article-writer)
  ()
  (:documentation "latex specific writer for articles"))

(defgeneric document-reader(entity)
  (:documentation "Return a document reader for an entity")
  (:method(entity)
    (declare (ignore entity))
    (make-instance 'article-rst-reader)))

(defgeneric document-writer(format entity &key &allow-other-keys)
  (:documentation "Return a document writer for an entity and format")
  (:method((format (eql :html)) article &key &allow-other-keys)
    (make-instance 'article-html-writer
                   :show-errors (has-permission :edit article)))
  (:method((format (eql :latex)) article &key &allow-other-keys)
    (make-instance 'article-latex-writer
                   :show-errors (has-permission :edit article))))
