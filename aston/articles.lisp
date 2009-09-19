;;;; Aston Web Applications
;;;; Copyright (C) 2002-2005 John A.R. Williams <J.A.R.Williams@jarw.org.uk>
;;;; Released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: adaptive-tutor.lisp,v 1.15 2005/03/10 19:54:25 willijar Exp $

(in-package :aston)

(setf clews.articles::*document-error-hook #'invoke-debugger)

(defvar *articles*
  (make-instance
   'clews.articles:tutorial-collection
   :id :test-tutorials
   :class 'clews.articles:tutorial-article
   :path #p"/home/willijar/www/clews/tutorials/"
   :file-type "rst"))

(defvar *article-app*
  (make-instance
   'clews.articles:clews-tutorial
   :articles *articles*
   :pwd-source *pwd-source*
   :user-dictionary *user-source*
   :acl '((:view . (:all))
          (:edit . ("willijar"))
          (:admin . ("willijar")))))

(aston::publish *tutorial-app* "/tutorials/")

