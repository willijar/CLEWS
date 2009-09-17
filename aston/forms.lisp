;;;; Aston Web Applications - Global form handling
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: forms.lisp,v 1.12 2005/03/10 19:54:25 willijar Exp $

(in-package :aston)

(defvar *aston-forms*
  (make-instance 'clews.data-collection:data-collection-application
                 :id :data-collection
                 :user-dictionary *user-source*
                 :pwd-source *pwd-source*
                 :plugins (list	*error-reporter-plugin*)
                 :acl '((:view . (:all))
                        (:admin . ("willijar")))))

(defmethod render-page ((app (eql *aston-forms*)) stream
				       markup)
  (html stream (apply-aston-style markup)))
