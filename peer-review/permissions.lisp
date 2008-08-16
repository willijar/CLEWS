;;;; $Id: permissions.lisp,v 1.1 2006/07/30 17:43:44 willijar Exp willijar $
;;;; Methods determining actions (via web interface) allowed by users.
;;;; of course inside the lisp environment anything is possible!!

(in-package :clews.peer-review)

(defmethod can-add-article((app peer-review) user
                           &key no-articles no-reviews title)
  "Return if username can add article with given name to peer-review
application. If no-reviews and no-topics are provided they will be
used, otherwise they will be calculated"
  (let((username (username user)))
    (cond ((has-permission :tutor app user) t)
          ((has-permission :student app user)
           (unless (and no-reviews no-articles)
             (multiple-value-bind (mark no-articles-c no-reviews-c)
                 (contribution-statistics app username)
               (declare (ignore mark))
               (setq no-reviews no-reviews-c)
               (setq no-articles no-articles-c)))
           (cond
             ((and (> no-articles 0)
                   (< no-reviews (* no-articles (review-ratio app))))
              (values nil (format nil "<b>~A</b> must complete ~D more reviews before they can add more articles"
                                  username
                                  (- (* no-articles (review-ratio app)) no-reviews))))
             ((and (review-feedback-form app)
                   (some #'needs-review-feedback
                         (articles-authored-by (article-index app) username)))
              (values nil "You must complete feedback on all the reviews of your articles before you can submit a new article"))
             (title
              (let ((articles-with-title (articles-with-title app title)))
                (cond
                  ((> (length articles-with-title) (max-no-writeups app))
                   (values nil
                           (format nil
                                   "There can be at most ~S writeups on ~A"
                                   (max-no-writeups app) title)))
                  ((mapcan
                    #'(lambda(a)
                        (when (string-equal username
                                            (slot-value a 'author))
                          (list a)))
                    articles-with-title)
                   (values nil (format nil "~S can only write one article on ~S" username title)))
                  (t t))))
             (t t)))
          (t
           (values nil
                   (format nil "~S is not a student or tutor." username))))))

;; the permission for articles can take the actual article or
;; its index entry

(defmethod can-edit-article((app peer-review) article user)
  "True if can edit an article. returns nil and explanaition if cannot.
  Can take index or actual article"
  (let ((username (username user)))
    (cond ((has-permission :tutor app user) t)
          ((has-permission :student app user)
           (if (and (has-content article) (not (is-author article username)))
               (values nil (format nil "~A cannot edit the article on ~A which was authored by ~A"
                                   username (title article) (author article)))
               (if (has-review article)
                   (values nil (format nil "Article ~A cannot be edited as it haas been reviewed." (title article)))
                   t )))
          (t (values nil
                     (format nil "~S is not a student or tutor." username))))))

(defmethod can-provide-review-feedback((app peer-review) article user review)
  (let ((username (username user)))
    (cond ((not (review-feedback-form app))
           (values nil "No review feedback form is provided."))
          ((has-permission :tutor app user) t)
          ((has-permission :student app user)
           (cond
             ((not (is-author article username))
              (values nil (format nil "~A cannot provide review feedback for the article on ~A which was authored by ~A"
                                  username (title article) (author article))))
             ((review-feedback review)
              (values nil
                      "Feedback has already been provided for this review"))
             (t t)))
          (t (values nil
                     (format nil "~S is not a student or tutor." username))))))

(defmethod can-delete-article((app peer-review) article user)
  (can-edit-article app article user))

(defmethod can-delete-review((app peer-review)  article
                             user review-author)
  (declare (ignore review-author article))
  (or
   (has-permission :tutor app user)
   (values nil
           (format nil "~S is not allowed to delete this review."
                   (username user)))))

(defmethod can-review-article((app peer-review) article user)
  "True if given user is allowed to review an article - can take the
   article or its index entry"
  (let ((username (username user)))
    (cond ((not (has-content article))
           (values
            nil
            (format nil
                    "The article on ~A has no content ans cannot be reviewed"
                    (title article))))
          ((and (created article)
                (< (get-universal-time)
                   (+ (created article) (reviewer-delay app))))
           (values
            nil
            (format nil
                    "The article on ~A is not available for reviewing until "
                    (format-time nil (+ (created article)
                                        (reviewer-delay app))))))
          ((has-permission :tutor app user) t)
          ((>= (length (reviews article)) (max-no-reviews app))
           (values nil
                   (format nil "At most ~D reviews are allowed per article"
                           (max-no-reviews app))))
          ((has-permission :student app user)
           (cond
             ((is-author article username)
              (values
               nil
               (format
                nil
                "~S cannot review the article on ~A which they have authored."
                username (title article))))
             ((is-reviewer article username)
              (values
               nil
               (format nil
                       "~S cannot review the article on ~A more than once."
                       username (title article))))
             (t)))
          (t (values nil
                     (format nil "~S is not a student or tutor." username))))))

(defmethod can-view-reviews((app peer-review) article user)
  "True if user is allowed to see the reviews"
  (let ((username (username user)))
    (cond ((has-permission :tutor app user) t)
          ((has-permission :student app user)
           (if (or (is-reviewer article username)
                   (is-author article username))
               t
               (values nil
                       (format nil
                               "~S cannot view the reviews on ~A until they have reviewed it."
                               username (title article)))))
          (t (values nil (format nil "~S is not a student or tutor." username))))))