;;;; CLEWS Peer review status plugin
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;; $Id: status-plugin.lisp,v 1.1 2006/07/30 17:43:53 willijar Exp $

(in-package :clews.peer-review)

(defclass status-plugin (plugin)
  ((peer-review :type peer-review :initarg :clews.peer-review
                :documentation "The parent adaptive tutor"))
  (:default-initargs :id :clews.peer-review-status)
  (:documentation "Status in peer review assessment"))

;;; we have the same id as the tutor - i.e. preferences are in
;;; same property subset - is this a good idea?

(defmethod plugin-markup((self status-plugin) request rest)
  (declare (ignore rest))
  (let* ((app (slot-value self 'peer-review))
         (user (remote-user request))
         (username (username user)))
    (multiple-value-bind
          (mark no-articles no-reviews average-article-mark
                average-review-mark last-article-time no-reviews-received feedback-mark)
        (contribution-statistics app username)
      (declare (ignore last-article-time))
      `((p "Welcome " ,username (br)
         "You have completed " (br)
         ,no-articles "/" ,(no-articles-required app) " articles" (br)
         ,no-reviews "/" ,(* (no-articles-required app)
                             (review-ratio app))
         " reviews" (br)
         "Total score :" ,(round mark) "" (br)
         ,@(when (> no-reviews-received 2)
                 `("Average score received :" ,(round average-article-mark)
                   (br)))
         ,@(when (> no-reviews 2)
                 `("Average score given :" ,(round average-review-mark)
                   (br)
                   "Review Quality :" ,(format nil "~4,2F" feedback-mark)
                   (br)))
         ((a :href "author/") "More Details"))))))



