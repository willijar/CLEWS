;;;; CLEWS Peer Review System -*- Lisp -*- Copyright (C) 2002-2005
;;;; John A.R. Williams <J.A.R.Williams@jarw.org.uk> Released under
;;;; the GNU General Public License (GPL) See
;;;; <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; $Id: clews.peer-review.asd,v 1.1 2006/07/30 17:42:26 willijar Exp willijar $

(in-package :asdf)
(defsystem :clews.groups
  :name "Groups"
  :description "Web based group assessment system"
  :author "Dr. John A.R. Williams"
  :version "0.1"
  :licence "GPL"
  :depends-on (:clews)
  :components
  ((:file "defpackage")
   (:file "command" :depends-on ("defpackage"))
   (:file "view" :depends-on ("defpackage"))
   (:file "components" :depends-on ("view"))
   (:file "controller" :depends-on ("command" "view" "components"))
   (:file "application" :depends-on ("controller"))))
