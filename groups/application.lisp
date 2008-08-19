;;;; Application entity -*- Lisp -*- Copyright (C) 2008
;;;; John A.R. Williams <J.A.R.Williams@jarw.org.uk> Released under
;;;; the GNU General Public License (GPL) See
;;;; <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;

(in-package :clews.groups)


(defclass application-module()
  ((name :type string :initarg :name :reader name)
   (title :type string :initarg :title :reader title)
   (description :type string :initarg :description :reader description))
  (:documentation "Basic self describing module - as views and may
  contain submodules"))



