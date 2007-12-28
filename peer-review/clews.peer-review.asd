;;;; CLEWS Peer Review System -*- Lisp -*- Copyright (C) 2002-2005
;;;; John A.R. Williams <J.A.R.Williams@jarw.org.uk> Released under
;;;; the GNU General Public License (GPL) See
;;;; <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: clews.peer-review.asd,v 1.1 2006/07/30 17:42:26 willijar Exp willijar $

(in-package :asdf)
(defsystem :clews.peer-review
  :name "peer-review"
  :description "Web based peer review system"
  :author "Dr. John A.R. Williams"
  :version "0.1"
  :licence "GPL"
  :depends-on (:clews :docutils)
  :components
  ((:file "defpackage")
   (:file "peer-review-class"
          :depends-on ("defpackage" "article-class" "forms"))
   (:file "article-class" :depends-on ("defpackage"))
   (:file "peer-review"
          :depends-on ("defpackage" "peer-review-class" "article-class"))
   (:file "status-plugin" :depends-on ("defpackage" "peer-review-class"))
   (:file "permissions" :depends-on ("peer-review"))
   (:file "forms" :depends-on ("defpackage"))
   (:file "formatting" :depends-on ("article"))
   (:file "article"
          :depends-on ("defpackage" "article-class" "peer-review-class"))
   (:file "peer-review-handlers"
          :depends-on ("permissions" "peer-review" "forms" "defpackage"))))