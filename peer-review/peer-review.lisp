;;;; $Id: peer-review.lisp,v 1.2 2006/10/14 13:23:49 willijar Exp willijar $
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
;;; most of the internal functionality associated with the peer-review
;;;;application including dealing  the indexing of entries,
;;; adding and removing of articles and reviews.

(defmethod add-article((app peer-review) id &rest rest)
  "Add an article with given id to the peer review system.
The remaining arguments are passed to the article constructor."
  (let ((article (apply #'make-instance (cons 'article rest))))
    (setf (get-dictionary id (articles app)) article)
    (reindex-article app id article)
    article))

(defmethod add-review ((app peer-review) id &rest rest)
  "Add a review to an article by name of id. The remaining arguments are the
   data from the review form"
  (let ((article (get-article id app)))
    (push rest (reviews article))
    (reindex-article app id article)
    rest))

(defmethod rem-article((app peer-review) id)
  "Remove an article from the peer review system"
  (unindex-article app id)
  (rem-dictionary id (articles app)))

(defmethod rem-review((app peer-review) article-id review-author)
  (let ((article (get-article article-id app)))
    (setf (slot-value article 'reviews)
          (delete-if #'(lambda(r) (string= review-author (author r)))
                     (slot-value article 'reviews)))
    (reindex-article app article-id article)
    (reviewers article)))

(defmethod get-review((app peer-review) article-id review-author)
  (let ((article (get-article article-id app)))
    (find-if #'(lambda(r) (string= review-author (author r)))
             (slot-value article 'reviews))))

(defmethod published-methods ((app peer-review) baseurl)
  (declare (ignore baseurl))
  `(,@(call-next-method)
    ("directory/" ,#'directory-handler :stage :response :match :prefix)
    ("new" ,#'new-article-handler :stage :response :match :exact)
    ("help" ,#'help-handler :stage :response)
    ("edit/" ,#'edit-article-handler :stage :response :match :prefix)
    ("edit/images/" ,#'manage-images-handler :stage :response :match :prefix)
    ("delete/" ,#'delete-article-handler :stage :response :match :prefix)
    ("delete-review/" ,#'delete-review-handler :stage :response :match :prefix)
    ("review/" ,#'review-article-handler :stage :response :match :prefix)
    ("marks/" ,#'marks-handler :stage :response :match :prefix :role :tutor)
    ("dregs" ,#'dregs-handler :stage :response :match :exact :role :tutor)
    ("all" ,#'all-handler :stage :response :match :exact :role :tutor)
    ("search" ,#'search-handler :stage :response)
    ("images/" ,(image-directory app))
    ("author/" ,#'author-handler :stage :response :match :prefix)))

(defmethod initialize-instance :after ((app peer-review) &key)
  (setf (slot-value app 'keyword-index)
        (make-instance
         'keyword-index
         :keywords (lambda(idx &key (ignore-word-p #'ignore-word-p))
                     (keywords (get-article idx app)
                               :ignore-word-p ignore-word-p))))
  (push (make-instance 'status-plugin :clews.peer-review app)
        (slot-value app 'clews::plugins))
  (reindex-articles app))

(defmethod reindex-article((app peer-review) id (article article))
  "Reindex given article with given id. returns the index entry.
   article is written back to dictionary i.e. marked as modified"
  (let ((entry (make-article-index-entry app id article))
        (old-entry (get-index id app)))
    (setf (get-dictionary id (articles app)) article)
    (setf (slot-value app 'article-index)
          (cons entry (delete old-entry (slot-value app 'article-index))))
    ;; and update crosslinks - this bit is tricky but nice
    (let ((entry-links-out (links-out entry))
          (entry-links-in (links-in entry)))
      (dolist (idx (article-index app))
        (when (listp (slot-value idx 'links-in))
          (when old-entry
            (setf (slot-value idx 'links-in)
                  (delete old-entry (slot-value idx 'links-in))))
          (when (member idx entry-links-out)
            (setf (slot-value idx 'links-in)
                  (cons entry (slot-value idx 'links-in)))))
        (when (listp (slot-value  idx 'links-out))
          (when old-entry
            (setf (slot-value idx 'links-out)
                  (delete old-entry (slot-value idx 'links-out))))
          (when (member idx entry-links-in)
            (setf (slot-value  idx' links-out)
                  (cons entry  (slot-value idx 'links-out)))))))
    entry))

(defmethod unindex-article((app peer-review) id)
  (let ((entry (get-index id app)))
    (unindex (keyword-index app) entry)
    (dolist (idx (article-index app))
      (when (listp (slot-value idx 'links-in))
        (setf (slot-value idx 'links-in)
              (delete entry (slot-value idx 'links-in))))
      (when (listp (slot-value  idx 'links-out))
        (setf (slot-value  idx' links-out)
              (delete entry (slot-value idx 'links-out)))))
    (setf (slot-value app 'article-index)
          (delete entry (slot-value app 'article-index)))))

(defmethod background-index-process((app peer-review))
  "Runs a background task - updates crosslinks as well as index. Returns the
   process id. "
  (reindex-articles app)
  (port:make-process
   "Indexing peer review articles"
   #'(lambda(app)
       ;; first ensure we have, as much as possible, calculated all links out
       (dolist (idx (article-index app))
         (links-out idx)
         (port:process-yield))
       ;; now we can calculate links-in from links-out's - could
       ;; only do this in background task after the above
       ;; as would take a long time otherwise - I like it!!!
       (dolist (idx (article-index app))
         (unless (listp (slot-value idx 'links-in))
           (setf (slot-value idx 'links-in)
                 (mapcan
                  #'(lambda(idx2)
                      (when (member idx (links-out idx2))
                        (list idx2)))
                  (article-index app))))))
   app))

(defmethod background-index-process((app peer-review))
  "Runs a background task - updates crosslinks as well as index. Returns the
   process id. "
  (reindex-articles app)
  (port:make-process
   "Indexing peer review articles"
   #'(lambda()
       (dolist (idx (article-index app))
         (setf (slot-value idx 'links-in) nil
               (slot-value idx 'links-out) nil))
       (dolist (idx (article-index app))
         (port:process-yield)
         (let ((content (content (get-article idx app))))
           (dolist (idx2 (article-index app))
             (when (search (title idx2) content )
               (push idx (slot-value idx2 'links-in))
               (push idx2 (slot-value idx 'links-out)))))))))

(defmethod reindex((app peer-review))
  (reindex-articles app)
  (dolist (idx (article-index app))
    (setf (slot-value idx 'links-in) nil
          (slot-value idx 'links-out) nil))
  (dolist (idx (article-index app))
    (let ((content (content (get-article idx app))))
      (dolist (idx2 (article-index app))
        (when (search (title idx2) content )
          (push idx (slot-value idx2 'links-in))
          (push idx2 (slot-value idx 'links-out)))))))

(defmethod reindex-articles((app peer-review))
  (clrindex (keyword-index app))
  (setf (slot-value app 'article-index)
        (mapcar #'(lambda(id)
                    (let((idx (make-article-index-entry
                               app id (get-article id app))))
                      (index (keyword-index app) idx)
                      idx))
                (article-ids app))))

(defmethod get-index((id string) (app peer-review))
  "Return the index entry corresponding to a particular id"
  (find-if #'(lambda(item) (string= id (id item)))
           (article-index app)))

(defun best-articles(indx &key reverse)
  "Given an article-index return best by mark in descending order"
  (sort
   (mapcan #'(lambda(r) (when (and (has-content r) (mark r) (> (mark r) 0))
                          (list r)))
           indx)
   (if reverse #'< #'>)
   :key #'mark))

(defun shortest-articles(indx &key reverse)
  (sort
   (mapcan #'(lambda(r) (when (has-content r) (list r))) indx)
   (if reverse #'> #'<)
   :key #'content-length))

(defun articles-requiring-writeup(indx)
  "Given an article index return articles requiring writeup, oldest first"
  (sort
   (mapcan #'(lambda(r) (unless (has-content r) (list r))) indx)
   #'< :key #'created))

;;;XXX how best to fix this??;;;
(defun articles-requiring-review(indx username)
  "Given an article index return those articles most requiring reviews
   not authored by given username"
  (sort (mapcan #'(lambda(r)
                    (when (and (not (or (is-author r username)
                                        (is-reviewer r username)))
                               (has-content r))
                      (list r)))
                indx)
        #'(lambda(a b)
            (let ((la (length (review-stats a)))
                  (lb (length (review-stats b))))
              (cond ((< la lb) t)
                    ((= la lb) (< (created a) (created b))))))))

(defun articles-authored-by(indx username)
  (mapcan
   #'(lambda(r) (when (string= (author r) username) (list r)))
   indx))

(defun articles-reviewed-by(indx username)
  (mapcan
   #'(lambda(r) (when (find username (reviewers r) :test #'string=)
                  (list r)))
   indx))

(defun newest-articles(indx)
  (sort (copy-seq indx) #'> :key #'created))

(defun contribution-statistics
    (app username
     &optional (indx (mapcan #'(lambda(r) (when (mark r)  (list r)))
                             (article-index app))))
  "return the following (unmoderated) statistics mark no-articles
no-reviews average-article-mark average-review-mark last-artcle-time
no-reviews-received average-feedback-mark-received"
  (let* ((authored (sort (articles-authored-by indx username) #'> :key #'mark))
         (authored-marks (mapcar #'mark authored))
         (review-stats
          (mapcan
           #'(lambda(idx)
               (let ((stat (find username (review-stats idx)
                                 :test #'string=
                                 :key #'review-stat-reviewer)))
                 (when stat (list stat))))
           (articles-reviewed-by indx username))))
    (values
     (if authored
         (/ (reduce #'+ (subseq authored-marks 0
                                (min (no-articles-required app)
                                     (length authored))))
            (no-articles-required app))
         0)
     (length authored)
     (length review-stats)
     (mean authored-marks)
     (mean (mapcar #'review-stat-mark review-stats))
     (if authored (apply #'max (mapcar #'created authored)) 0)
     (if authored
         (reduce #'+
                 (mapcar #'length
                         (mapcar #'reviewers authored)))
         0)
     (if review-stats
         (mean (mapcar #'(lambda(stat)
                           (or (review-stat-feedback-mark stat) 55))
                       review-stats))
         55))))

(defun marks-analysis(app authors &optional (moderation authors))
  "Return a hash of mark information for given set of
authors. moderation can be a number to normalise average against or a
list of reviewers names to normalise against.  Returned data will be a
p-list with atttributes :mark :raw-mark :no-articles :no-reviews
:article-mark :review-mark :last-post :no-reviews-received
:feedback-mark-received"
  (let ((results (make-hash-table :test #'equal))
        (indx (mapcan #'(lambda(r) (when (mark r) (list r)))
                      (article-index app))))
    (flet ((author-contribution(author)
             (or (gethash author results)
                 (multiple-value-bind
                       (mark no-articles no-reviews article-mark
                             review-mark last-post no-reviews-received
                             feedback-mark)
                     (contribution-statistics app author indx)
                   (setf (gethash author results)
                         (list :raw-mark mark
                               :no-articles no-articles
                               :no-reviews no-reviews
                               :article-mark article-mark
                               :review-mark review-mark
                               :last-post last-post
                               :no-reviews-received no-reviews-received
                               :feedback-mark-received feedback-mark))))))
      (when (listp moderation)
        (setq moderation
              (mean (mapcar
                     #'(lambda(r) (getf (author-contribution r) :review-mark))
                     moderation))))
      (flet((moderated-mark(idx)
              (if (review-stats idx)
                  (let((weight 0)
                       (total 0))
                    (dolist(stat (review-stats idx))
                      (let*((c (author-contribution
                                (review-stat-reviewer stat)))
                            (w (or (getf c :feedback-mark-received) 100)))
                        (when (getf c :review-mark)
                          (incf weight w)
                          (incf total (* moderation w
                                         (/ (review-stat-mark stat)
                                            (getf c :review-mark)))))))
                    (if (> weight 0)
                        (/ total weight)
                        0))
                  0)))
        (dolist (author authors)
          (let ((moderated-marks
                 (sort (mapcan
                        #'(lambda(idx)
                            (when (string= (author idx) author)
                              (let ((m (moderated-mark idx)))
                                (when m (list (moderated-mark idx))))))
                        indx)
                       #'>)))
            (setf (getf (gethash author results) :mark)
                  (if moderated-marks
                      (/ (reduce #'+ (subseq moderated-marks 0
                                             (min (no-articles-required app)
                                                  (length moderated-marks))))
                         (no-articles-required app))
                      0))))))
    (values results moderation)))

(defmethod articles-with-title((app peer-review) title)
  "return list of ids of articles given a title"
  (mapcan #'(lambda(idx) (when (string-equal title (title idx)) (list idx)))
          (article-index app)))

(defmethod articles-with-duplicate-titles((app peer-review))
  "return a list of lists of article indexes which have duplicated titles"
  (let ((title-hash (make-hash-table :test #'equal)))
    (dolist (idx (article-index app))
      (let ((title (string-downcase (title idx))))
        (setf (gethash title title-hash)
              (cons idx (gethash  title title-hash)))))
    (let ((duplicates '()))
      (maphash (lambda(k v)
                 (declare (ignore k))
                 (when (> (length v) 1) (push v duplicates)))
               title-hash)
      duplicates)))

(defmethod scan-content-for-new-topics((app peer-review) content username)
  "Scans content, creating new nodes in app where applicable. returns content
   with markup removed."
  (regex:regex-substitute-string
   (load-time-value
    (regex:compile-str "#\\{[:space:]*(.+?)[:space:]*\\}") t)
   content
   (list #'(lambda(regs)
             (unless (articles-with-title app (elt regs 1))
               (add-article app (generate-new-article-id app)
                            :title (elt regs 1) :author username))
             (elt regs 1) ))))

(defmethod authors((app peer-review))
  "Return list of all authors of articles"
  (delete-duplicates
   (mapcar #'(lambda(idx) (author idx)) (article-index app))
   :test #'string=))

(defmethod related-articles((app peer-review) article)
  (sort (search-index (keyword-index app) (keywords article))
        #'>
        :key #'cdr))

(defun get-review-stats(app &optional ( review-form-name "peer-review-2001"))
  "Return the review questionnaire stats for given peer review system and questionnaire name - used to analyse form use"
  (let* ((form (find-form review-form-name ))
         (results
          (mapcan
           #'(lambda(element)
               (multiple-value-bind(tag attr content)
                   (split-markup (clews.form::markup-template element))
                 (declare (ignore attr))
                 (when (eq tag 'mcq)
                   (list (cons (clews.form::name element)
                               (mapcar
                                #'(lambda(choice)
                                    (cons (if (consp choice)
                                              (car choice)
                                              choice)
                                          0))
                                content))))))
           (clews.form::elements form))))
    (map-dictionary
     #'(lambda(id article)
         (declare (ignore id))
         (dolist (review (reviews article))
           (when (string-equal review-form-name (getf review :form))
             (dolist (element results)
               (incf (cdr (assoc (getf review (car element)) (cdr element) ))))
             )))
     (articles app))
    results))

(defun remove-worst-articles(app &optional (min-mark 40))
  "Remove articles (not written by admin or tutor) which have a mark lower than min-mark"
  (warn "~D Articles" (length (article-index app)))
  (let ((count 0))
    (dolist(idx (copy-list (article-index app)))
      (when (and (not (has-permission
                       '(:tutor :admin)
                       app
                       (get-dictionary (author idx) (users app))))
                 (< (mark idx) min-mark))
        (incf count)
        (rem-article app (id idx))))
    (warn "~D articles removed" count)))

(defun average-mark(marks &key
                    (ignore-nil t)
                    (ignore-0 nil)
                    (mark #'identity)
                    (weight  #'(lambda(m) (declare (ignore m)) 1)))
  "Given a set of mark entities return the weighted average.
The keywords
:mark is the function which when applied to an entity returns the mark
:weight is the function which when applied to an entity returns the weight
:ignore-nil if true do not count marks if they are nil (default t)
:ignore-0 if true do not count marks if they are 0 (default nil)

It is generally assumed that if a mark value is nil it has not been
entered yet. If it is 0 then probable the student didn't attend."
  (let ((mark-total 0)
        (weight-total 0))
    (dolist(entity marks)
      (let ((w (funcall weight entity))
            (m (funcall mark entity)))
        ;;(unless (null m) (incf mark-total (* w (funcall mark entity))))
        (when (cond ((null m) (not ignore-nil))
                    ((< m 0.5) (not ignore-0))
                    (t))
          (incf mark-total (* w m))
          (incf weight-total w))))
    (if (= 0 weight-total) 0 (/ mark-total weight-total))))


(defmethod clews::user-component-preferences((self peer-review) user)
  (declare (ignore user))
  (append (call-next-method)
          '((:refresh
             :text
             "How often do you want the tutorial pages to
automatically refresh?  Every"
             :markup ((mcq :style :dropdown)
                      (30 . "30 seconds")
                      (60 . "1 minute")
                      (120 . "2 minutes")
                      (600 . "5 minutes")
                      (nil . "Never"))
             :type (integer :min 10 :max 86400 :nil-allowed t)
             :default nil)
            (:related-threshold
             :text "Set the similarity threshold for determining
related notes. A larger number means articles must be more similar to
be considered related and reduces the number of related node links."
             :markup ((mcq :style :dropdown)
                      (0.05 . "5%")
                      (0.1 . "10%")
                      (0.2 . "20%")
                      (0.3 . "30%"))
             :type (number :min 0.0 :max 1.0)
             :default 0.1)
            (:search-threshold
             :text "Set the search term threshold. A larger number
means articles must be more similar to the search term to be listed
and reduces the number of entries listed when searching."
             :markup ((mcq :style :dropdown)
                      (0.1 . "10%")
                      (0.2 . "20%")
                      (0.4 . "40%")
                      (0.8 . "80%")
                      (0.9 . "90%"))
             :type (number :min 0.0 :max 1.0)
             :default 0.1))))