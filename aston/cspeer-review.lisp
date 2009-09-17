;;;; Aston Web Applications
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: peer-review.lisp,v 1.17 2005/03/13 09:33:46 willijar Exp $

(in-package :aston)


(defvar *cs-peer-review*
  (make-instance
   'clews.peer-review:peer-review
   :id :cd-peer-review
   :article-dictionary
   (make-filesystem-dictionary
    (translate-logical-pathname "clews:cs-peer-review;*.lisp")
    :deleted-directory (translate-logical-pathname
                        "clews:cs-peer-review;deleted;"))
   :image-directory (translate-logical-pathname "clews:cs-peer-review;images;")
   :review-form (find-form "peer-review-2004")
   :review-feedback-form (find-form "peer-review-feedback-2005")
   :user-dictionary *user-source*
   :plugins nil
   :acl '((:view . (:all))
          (:student . (:cs2009 ))
          (:tutor . ("willijar" "ekarta"))
          (:admin . ("willijar" "ekarta")))
   :pwd-source *pwd-source* ))

(defmethod render-page ((app (eql *cs-peer-review*)) stream markup)
  (html stream (apply-aston-style markup)))
