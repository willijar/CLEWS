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
(in-package :clews.articles)

(defclass article-rst-reader(docutils.parser.rst:rst-reader)
  ()
  (:documentation "Specialisation parser for articles"))

(defmethod transforms((reader article-rst-reader))
  (nconc (list 'docutils.transform:fignum)
         (call-next-method)))

(defclass article-writer()
  ((lock :initform (make-mutex) :reader mutex)
   (errors :initarg :show-errors :reader errors :initform nil :type boolean))
  (:documentation "Mix in for article docutils writers"))

(defclass article-html-writer(html-writer article-writer)
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


(defclass evaluation(docutils.nodes:element)
  ((language :type symbol :initform :lisp :initarg :language :reader language)
   (output-format :type symbol :initform :markup
                  :initarg  :output-format :reader output-format)
   (content :initarg :content :reader content)
   (evaluation)))

(def-directive evaluate
    (parent language
            &option
            (output symbol :markup)
            (package symbol :markup)
            &content content)
  (let ((language (intern (string-upcase language) :keyword))
        (*package* (or (find-package package) (find-package :markup))))
  (if content
      (let ((content
             (with-output-to-string(os)
               (loop :for line :across content
                  :do (write-line line os)))))
        (add-child
         parent
         (make-instance
          'evaluation
          :language language
          :output-format output
          :content (ecase language
                     (:lisp (read-from-string content))))))
      (report :error "Evaluation directive is empty; content required."))))

(defmethod evaluation((node evaluation))
  "If we have cached an evaluation use it otherwise return an evaluation
without caching."
  (if (slot-boundp node 'evaluation)
      (slot-value node 'evaluation)
      (ecase (language node)
        (:lisp (eval (content node))))))

(defmethod evaluate((node evaluation))
  "Reevaluate and Cache node"
  (handler-bind
       ((error
         #'(lambda(e)
             (when *document-error-hook*
               (funcall *document-error-hook* e))
             (docutils::report :terminal (format nil "~S" e)))))
    (setf (slot-value node 'evaluation) (evaluation node))))

(defmethod copy-of-node((node evaluation))
  (let ((copy (call-next-method)))
    (dolist(slot '(language output-format content evaluation))
      (when (slot-boundp node slot)
        (setf (slot-value copy slot) (slot-value node slot))))
    copy))

(defmethod visit-node
    ((writer docutils.writer.latex::latex-writer)
     (node evaluation))
  (let ((evaluation (evaluation node)))
      (case (output-format node)
        (:markup (part-append
                  (with-output-to-string(os)
                    (markup:latex os evaluation))))
        (:latex (part-append evaluation)))))

(defmethod visit-node((writer docutils.writer.html:html-writer)
                      (node evaluation))
  (let ((evaluation (evaluation node)))
      (case (output-format node)
        (:markup (part-append
                  (with-output-to-string(os)
                    (markup:html os evaluation))))
        (:html (part-append evaluation)))))
