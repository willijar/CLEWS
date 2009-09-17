;;;; Aston Web Applications
;;;; Copyright (C) 2004-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: discussions.lisp,v 1.2 2005/03/13 09:33:21 willijar Exp $

(in-package :aston)

(defvar *news-source*
  (make-instance 'clews.discussion:directory-news-source
                 :directory (translate-logical-pathname #p"clews:news;")))

(defvar *discussions*
  (make-instance 'clews.discussion:discussion-groups
                 :user-dictionary *user-source*
                 :pwd-source *pwd-source*
                 :news-source *news-source*
                 :plugins (list *polls-plugin*
                                *who-plugin*
                                *chatterbox-plugin*
                                *error-reporter-plugin*
                                *quotes-plugin*)
                 :acl '((:view . (:all))
                        (:tutor "willijar")
                        (:admin . ("willijar")))))
