;; $Id: formatting.lisp,v 1.3 2007/10/02 13:42:02 willijar Exp willijar $
;; REstructured text formatting for text articles and reviews
;; Copyright (C) 2006 Dr. John A.R. Williams

;; Author: Dr. John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;; Keywords: CLEWS, peer-review

;; This file is part of CLEWS Peer Review Application

;; This is free software released under the GNU General Public License (GPL)
;; See <http://www.gnu.org/copyleft/gpl.html>

;;; Commentary:

;; Specialisation of docutils restructured text reader and writers

;;; Code:

(in-package :clews.peer-review)

(defclass peer-review-rst-reader(docutils.parser.rst:rst-reader)
  ()
  (:documentation "Specialised restructured article text reader for
peer review"))

(defmethod docutils:transforms((reader peer-review-rst-reader))
  '(docutils.transform:final-checks
    docutils.transform:filter-messages
    docutils.transform:substitutions
    docutils.transform:chained-targets
    docutils.transform:anonymous-hyperlinks
    docutils.transform:indirect-hyperlinks
    docutils.transform:footnotes
    docutils.transform:external-targets
    docutils.transform:internal-targets))

(defun fix-image-links(document prefix)
  (docutils:with-nodes(node document)
    (when (typep node 'docutils.nodes:image)
      (setf (docutils:attribute node :uri)
            (concatenate 'string prefix (docutils:attribute node :uri))))))

(defclass peer-review-html-writer(docutils.writer.html:html-writer)
  ()
  (:documentation ""))

(defun refnames(app)
  "Return a hash table mapping reference names to article ids"
  (let ((refnames (make-hash-table :test #'equal)))
    (dolist(a (article-index app))
      (setf (gethash (docutils::normalise-name (title a)) refnames) (id a)))
    refnames))

(defmethod docutils.utilities::read-lines((article article))
  (docutils.utilities::read-lines (content article)))


(defmethod parse-rst((app  peer-review) (id string) &key (path-to-root "."))
  (let* ((reader (make-instance 'peer-review-rst-reader))
         (writer (make-instance 'peer-review-html-writer))
         (article (get-article id app))
         (content (content article))
         (refnames (refnames app))
         (docutils.writer.html::*section-level* 1)
         (docutils:*unknown-reference-resolvers*
          (cons #'(lambda(node)
                    (peer-review-resolve-link
                     app node
                     :path-to-root path-to-root
                     :refnames refnames))
                docutils:*unknown-reference-resolvers*))
         (document
          (handler-case
              (docutils:read-document content reader)
            (docutils:markup-condition(e)
                ;; do not just bomb out if there is an unrecoverable
                ;; error in parsing
              (let ((node (docutils:error-node e))
                    (msg (docutils:error-message e))
                    (line (docutils:error-line e)))
                (format *error-output*
                        "Docutils error in~% ~S~% ~@[ line ~A~] ~A~%" ;; and print
                        node line msg)
                (unless node
                  (return-from parse-rst
                    (format
                     nil
                     "<p>Unable to parse document: ~@[ line ~A~] ~A</p>"
                     line msg)))
                (docutils:add-child node
                                    (docutils:make-node
                                     'docutils.nodes:system-message
                                     "Unable to continue parsing document"))
                (docutils:document node))))))
    (fix-image-links document (format nil "~A/images/~A-" path-to-root id))
    (setf (docutils:document writer) document)
    (with-output-to-string(os)
      (docutils:write-part writer 'docutils.writer.html:body os))))

(defun peer-review-resolve-link(app node &key (path-to-root ".")
                                (refnames (refnames app)))
  (let ((refname (docutils:attribute node :refname)))
    (when refname
      (docutils:rem-attribute node :refname)
      (let ((id (gethash refname refnames)))
        (unless id
          (setf id (generate-new-article-id app))
          (add-article app id
                       :title (docutils:as-text node)))
        (setf (docutils:attribute node :refuri)
              (with-output-to-string(os)
                (write-string path-to-root os)
                (write-char #\/ os)
                (write-string id os)))))))