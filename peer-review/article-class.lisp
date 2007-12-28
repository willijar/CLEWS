;;;; $Id: article-class.lisp,v 1.1 2006/07/30 17:42:02 willijar Exp $
;;;; Copyright (C) 2002,2003
;;;;   Dr. John A.R. Williams, J.A.R.Williams@blueyonder.co.uk

;;;; This file is part of the Common Lisp Education Web Application
;;;; Server - Peer Review Application omponent

;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.

;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
;;;; or retreive a copy from http://www.gnu.org/licenses/gpl.txt

;;;; Article class definition, and methods which depend only on it

(in-package :clews.peer-review)
(defclass article ()
  ((title :type string :reader title
          :initform nil
          :initarg :title
          :documentation "The title of the article")
   (summary :type string :reader summary
            :initform ""
            :initarg :summary
            :documentation "A short abstract of the content")
   (content :type string :reader content
            :initform nil
            :initarg :content
            :documentation "The textual content of the article")
   (style :type (member '(:html :structured-text :rst))
          :reader style
          :initform :rst
          :initarg :style
          :documentation "Type of formating used in content")
   (author :type string :reader author
           :initform nil
           :initarg :author
           :documentation "The username of the Author")
   (created :type integer :reader created
            :initarg :created
            :initform (get-universal-time)
            :documentation "Time when the article was last modified")
   (reviews :type list :accessor reviews
            :initform '()
            :documentation "List of all reviews of this article")
   (anonymous :type boolean :reader anonymous
              :initarg :anonymous
              :initform nil
              :documentation "If true article is anonymous (to viewers)")
   (images :type list :accessor images
           :initform '()
           :documentation "Images associated with this article"))
  (:documentation "Class representing a peer review article"))

(defmethod print-object((article article) stream)
  (print-unreadable-object(article stream :type t :identity t)
    (format stream "~S by ~S" (title article) (author article))))

(defmethod initialise-instance :after ((instance article) &key)
  (setf (slot-value instance 'title)
        (string-trim " 	"  (slot-value instance 'title))))

(defmethod make-load-form ((instance article) &optional environment)
  (make-load-form-saving-slots instance :environment environment))

(defmethod reviewers((article article))
  "Return a list of reviewers"
  (mapcar #'(lambda(r) (getf r :author)) (reviews article)))

(defmethod review((article article) username)
  "Return review of article by given author"
  (find username (reviews article) :key #'author :test #'string=))

(defmethod content-length((article article))
  (let ((c (content article)))
    (when c (length c))))

(defmethod has-content(item)
  (let ((c (content-length item)))
    (and c (> (content-length item)) 0)))

(defmethod keywords ((article article) &key (ignore-word-p #'ignore-word-p) &allow-other-keys)
  (nconc (keywords (title article) :ignore-word-p ignore-word-p)
         (keywords (summary article) :ignore-word-p ignore-word-p)
         (keywords (content article) :ignore-word-p ignore-word-p)))

(defmethod has-review((article article))
  (reviews article))

(defmethod mark-article((article article))
  (mean (mapcar #'mark-form-data (reviews article))))

(defclass article-index-entry ()
  ((id :initarg :id :reader id)
   (title :type string :initarg :title :reader title)
   (author :type string :initarg :author :reader author)
   (created :type bignum :initarg :created :reader created)
   (review-stats :type list :initarg :review-stats :reader review-stats)
   (targets :type list :initform nil :initarg :targets :reader targets
            :documentation "Target names in the article")
   (links-in  :initarg :links-in)
   (links-out  :initarg :links-out)
   (content-length :initform nil
                   :initarg :content-length :reader content-length))
  (:documentation "Short record used in indexing articles
for statistics purposes"))

(defmethod print-object((idx article-index-entry) stream)
  (print-unreadable-object(idx stream :type t :identity t)
    (princ (id idx) stream)))

(defstruct review-stat
  (reviewer nil)
  (mark nil)
  (feedback-mark nil))

(defmethod reviewers((idx article-index-entry))
  (mapcar #'review-stat-reviewer (review-stats idx)))

(defmethod mark((idx article-index-entry))
  (mean (mapcar #'review-stat-mark (review-stats idx))))

(defun make-article-index-entry(app id article)
  (make-instance
   'article-index-entry
   :id id
   :title (title article)
   :author (author article)
   :created (created article)
   :review-stats (mapcar
                  #'(lambda(review)
                      (make-review-stat
                       :reviewer (author review)
                       :mark (mark-form-data review)
                       :feedback-mark (when (review-feedback review)
                                        (* 100 (mark-form-data
                                                (review-feedback review))))))
                  (reviews article))
   :content-length (content-length article)
   :links-in app
   :links-out app))

(defmethod links-in((idx article-index-entry))
  (let ((entry (slot-value idx 'links-in)))
    (if (listp entry)
        entry
        (setf (slot-value idx 'links-in)
              (find-links-in entry (get-article (id idx) entry))))))

(defmethod links-out((idx article-index-entry))
  (let ((entry (slot-value idx 'links-out)))
    (if (listp entry)
        entry
        (let ((article (get-article (id idx) entry)))
          (if article
              (setf (slot-value idx 'links-out)
                    (find-links-out entry article))
              (error "Article ~A missing ~A:~A:~A" (id idx) article idx entry))))))

(defmethod link-html((idx article-index-entry))
  `((a :href ,(slot-value idx 'id))
    ,(slot-value idx 'title)))

(defmethod has-review((idx article-index-entry))
  (reviewers idx))

; these take index entry or article
(defun is-author(article username)
  (string= username (author article)))

(defun is-reviewer(article username)
  (member username (reviewers article) :test #'string=))

(defmethod author((review list))
  (getf review :author))

(defmethod created((review list))
  (getf review :created))

(declaim (inline review-feedback (setf review-feedback)))
(defun review-feedback(review)
  (getf review 'review-feedback))

(defmethod needs-review-feedback((idx article-index-entry))
  (when (review-stats idx)
    (not (every #'review-stat-feedback-mark (review-stats idx)))))

(defmethod needs-review-feedback((article article))
  (when (reviews article)
    (not (every #'(lambda(review) (mark-form-data (review-feedback review)))
                (reviews article)))))