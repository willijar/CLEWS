;;;; $Id: passwd.lisp,v 1.1 2005/03/10 19:54:25 willijar Exp $
;;;; Copyright (C) 2003 Dr. John A.R. Williams, J.A.R.Williams@blueyonder.co.uk

;;;; This file is part of the Aston EWAS application configuration

(in-package :aston)

(defvar *passwd*
  (make-instance
   'clews.passwd:passwd-manager
   :admin "J.A.R.Williams@aston.ac.uk"
   :user-dictionary *user-source*
   :pwd-source *pwd-source*
   :authenticators (list *pwd-source*)
   :realm "Aston Electronic Engineering"
   :acl '((:view . (:all))
          (:admin . ("willijar")))))

