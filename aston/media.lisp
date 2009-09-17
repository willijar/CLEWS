;;;; $Id: passwd.lisp,v 1.1 2005/03/10 19:54:25 willijar Exp $
;;;; Copyright (C) 2003 Dr. John A.R. Williams, J.A.R.Williams@blueyonder.co.uk

;;;; This file is part of the Aston CLEWS application configuration

(in-package :aston)

(setf jarw.media:*media-server*
  (make-instance
   'clews.media:media-manager
   :user-source *user-source*
   :pwd-source *pwd-source*
   :path (translate-logical-pathname #p"clews:media;")
   :acl '((:view . (:all))
          (:add . (:staff :research :admin))
          (:admin . ("willijar")))))



